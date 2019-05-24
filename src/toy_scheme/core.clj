(ns toy-scheme.core
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.tools.trace :refer :all]
            [clojure.test :as t]
            [instaparse.core :as insta]
            [sc.api]))


(declare  form-eval form-apply self-evaluating? eval-seq setup-env extend-env  env-find env-set env-get  
          eval-define eval-lambda eval-if eval-defmacro eval-assignment eval-let*  macroexpand set-built-in!)


(defn form-eval [exp env]
  (let [exp (macroexpand exp env)]
    (cond (self-evaluating? exp)  exp
          (symbol? exp)           (env-get exp env)
          (= (first exp) 'quote)  (second exp)
          (= (first exp) 'if)     (eval-if exp env)
          (= (first exp) 'define) (eval-define exp env)
          (= (first exp) 'begin)  (eval-seq (rest exp) env)
          (= (first exp) 'set!)   (eval-assignment exp env)
          (= (first exp) 'let*)   (eval-let* exp env)
          (= (first exp) 'lambda) (eval-lambda exp env)
          (= (first exp) 'defmacro) (eval-defmacro exp env)
          (= (first exp) 'macroexpand) (macroexpand (second exp) env)
          :else (form-apply (form-eval (first exp) env)
                            (map #(form-eval % env) (rest exp))))))


(defn form-apply [proc args]
  (letfn [(primitive? [p] (= (first p) 'primitive))
          (procedure? [p] (= (first p) 'procedure)) ;;<-- from lambda
          (proc-params [p] (second p))
          (proc-body [p] (nth p 2))
          (proc-env [p] (nth p 3))]
    (cond (primitive? proc) (apply (second proc) args) ;;<-- magic
          (procedure? proc) (eval-seq (proc-body proc)
                                      (extend-env (proc-env proc)
                                                  (proc-params proc)
                                                  args)))))

(def primitive-procs {
                      'true true
                      'false false
                      'count count
                      'car first
                      'cdr rest
                      '+   +
                      '-   -
                      '*   *
                      '/   /
                      '<   <
                      '>   >
                      'eq? =
                      '=   =
                      'not not
                      'list list
                      'print  prn
                      'cons cons
                      'empty? empty?
                      })

(defn self-evaluating? [x] 
  (or (number? x)
      (string? x)
      (nil? x)
      (boolean? x)))

(defn eval-assignment [exp env]
  (let [[op var val] exp]
    (env-set var val env)))

(defn eval-lambda [exp env]
  (list 'procedure
        (second exp)
        (drop 2 exp)
        env))


(defn eval-if [exp env]
  (let [[a0 a1 a2 a3] exp]
    (if (form-eval a1 env)
      (form-eval a2 env)
      (form-eval a3 env))))


(defn eval-seq [exp env]
  (reduce #(form-eval %2 env) nil exp))

(defn eval-let* [exp env]
  (let [eenv (extend-env env)
        [op vars body] exp]
    (doseq [[b e] vars]
      (swap! (first eenv) assoc b (form-eval e eenv)))
    (form-eval body eenv)))

(defn eval-defmacro [exp env]
  (let [[a0 a1 a2] exp
        mbody (with-meta (form-eval a2 env) {:ismacro true})]
    (swap! (first env) assoc a1  mbody)
    nil))


;;; enviroment ;;;

(defn setup-env []
  (-> '()
      (extend-env (keys primitive-procs)
                  (map #(list 'primitive %) (vals primitive-procs)))
      (extend-env)
      (set-built-in!)
      (extend-env)))


(defn define-var [exp]
  (let [var (second exp)]
    (if (seq? var)
      (first var)
      var)))

(defn define-val [exp env]
  (let [var (second exp)
        make-lambda #(cons 'lambda  (cons %1 %2))]
    (if (seq? var)
      (form-eval (make-lambda (rest var) (drop 2 exp)) env)
      (form-eval (nth exp 2) env))))


(defn eval-define [exp env]
  (let [var (define-var exp)
        val (define-val exp env)]
    (swap! (first env) assoc var val))
  "ok")


(defn extend-env
  ([env]
   (extend-env env '() '()))
  
  ([env vars vals]
   (loop [vr vars vl vals vmap {}]
     (let [x (first vr)
           y (first vl)]
       (cond
         (and (nil? x) (nil? y))  (cons (atom vmap) env)
         (or (nil? x) (nil? y))   (throw (Exception. "arguments not match!" env))
         (= x '.)                 (recur nil nil (assoc vmap (second vr) vl))
         :else                    (recur (rest vr) (rest vl) (assoc vmap x y)))))))


(defn env-find [var env action not-found]
  (loop [[e & tail] env]
    (cond (nil? e)           (not-found)
          (contains? @e var) (action e)
          :else              (recur tail))))

(defn env-get [var env]
  (env-find var env
            #(get (deref %) var)
            #(throw (Exception. "wrong binding id!"))))

(defn env-set [var val env]
  (env-find var
            env
            #(swap! % assoc var (form-eval val env))
            #(throw (Exception. "wrong binding id!"))))

;;;; macro ;;;;

(defn is-macro-call [exp env]
  (and (seq? exp)
       (symbol? (first exp))
       (env-find (first exp)
                 env
                 #(:ismacro (meta (get @% (first exp))))
                 #(identity false))))


(defn macroexpand [exp env]
  (loop [exp exp]
    (if (is-macro-call exp env)
      (let [mac (env-get (first exp) env)]
        (recur (form-apply mac (rest exp))))
      exp)))

;;;;;; built-in functions

(defn set-built-in! [env]
  (form-eval
   '(define (map f xs)
      (if (empty? xs)
        (quote ())
        (cons
         (f (car xs))
         (map f (cdr xs))))) env)

  (form-eval
   '(define (reduce f val xs)
      (if (empty? xs)
        val
        (reduce f
                (f val (car xs))
                (cdr xs)))) env)
  
  (form-eval
   '(define (filter f xs)
      (if (empty? xs)
        (quote ())
        (let* ((x (car xs))
               (y (cdr xs)))
              (if (f x)
                (cons x (filter f y))
                (filter f y))))) env)
  env)

;;;;;;
(def validate
  (insta/parser
    "S = Symbol | Form
     Symbol = #'[^() \"]*'
     Symbol_Space = #'[^()\"]*'
     Form = '(' (Symbol_Space*| Form*)* ')'"))


(defn repl []
  (let [global-env (setup-env)]
    (loop [x ""]
      (when (empty? x)
        (print "my scheme => "))
      (flush)
      (let [form (str x (read-line))]
        (if (insta/failure? (validate form)) 
          (recur form)
          (do
            (prn  (form-eval (read-string form) global-env))
            (recur "")))))))


(defn -main  [& args] (repl))