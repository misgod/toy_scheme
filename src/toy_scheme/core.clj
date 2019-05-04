(ns toy-scheme.core
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.tools.trace :refer :all]
            [clojure.test :as t]
            [instaparse.core :as insta]
            [sc.api]))


(declare eval-seq scan-var lookup-var form-apply extend-env setup-env 
         eval-define eval-lambda eval-if eval-defmacro eval-cond eval-assignment eval-let*)

(defn form-eval [exp env]
  ;;(prn "eval" exp "===>\n" (keys (first (persistent! (env)))) )
  (letfn [(constant? [x] (or (number? x)
                             (string? x)
                             (nil? x)
                             (instance? Boolean x)))
          (lookup-var [exp env] (scan-var exp
                                          env
                                          #(get (deref %) exp)))]

    (cond (constant? exp)         exp
          (symbol? exp)           (lookup-var exp env)
          (= (first exp) 'quote)  (second exp)
          (= (first exp) 'if)     (eval-if exp env)
          (= (first exp) 'cond)   (eval-cond exp env)
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
  ;(prn "apply:" (take 10 proc) "=>" args)
  (letfn [(primitive? [p] (= (first p) 'primitive))
          (apply-primitive [p a] (apply (second p) a)) ;;<-- magic
          (compound? [p] (= (first p) 'procedure))
          (proc-params [p] (second p))
          (proc-body [p] (nth proc 2))
          (proc-env [p] (nth proc 3))]
    (cond (primitive? proc) (apply-primitive proc args)
          (compound? proc)  (eval-seq (proc-body proc)
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
                      'eq? =
                      'not not
                      'map map
                      'reduce reduce
                      'filter filter
                      'list list
                      'cons cons
                      })
(defn setup-env []
  (-> '()
      (extend-env (keys primitive-procs)
                  (map #(list 'primitive %) (vals primitive-procs)))
      ;;above is base env
      (extend-env '() '()))) ;;avoid to modify base env


(defn define-var [exp]
  (let [var (second exp)]
    (if (coll? var)
      (first var)
      var)))

(defn define-val [exp env]
  (let [var (second exp)
        make-lambda #(cons 'lambda  (cons %1 %2))]
    (if (coll? var)
      (form-eval (make-lambda (rest var) (drop 2 exp)) env)
      (form-eval (nth exp 2) env))))


(defn eval-define [exp env]
  (let [var (define-var exp)
        val (define-val exp env)
        e (first env)]
    (swap! e assoc var val)))


(defn eval-assignment [exp env]
  (let [var (second exp)
        val (nth exp 2)]
    (scan-var var
              env
              #(swap! % assoc var (form-eval val env)))))

(defn eval-lambda [exp env]
  (list 'procedure
        (second exp)
        (drop 2 exp)
        env))


(defn eval-if [exp env]
  (if (form-eval (second exp) env)
    (form-eval  (nth exp 2) env)
    (form-eval  (nth exp 3) env)))

(defn eval-cond [exp env]
 ; (let [(rest exp)])
  
  )



(defn scan-var [var env action not-found]
  (let [e (first env)]
    (cond (nil? e)           (throw (Exception. "wrong binding id!"))
          (contains? @e var) (action e)
          :else              (recur var (rest env) action))))

(defn eval-seq [exps env]
  (reduce #(form-eval %2 env) nil exps))



(defn extend-env [env vars vals]
  (loop [vr vars vl vals vmap {}]
    (let [x (first vr)
          y (first vl)]
      (cond
        (and (nil? x) (nil? y))  (cons (atom vmap) env)
        (or (nil? x) (nil? y))   (throw (Exception. "arguments not match!" env))
        (= x '.)                 (recur nil nil (assoc vmap (second vr) vl))
        :else                    (recur (rest vr) (rest vl) (assoc vmap x y))))))


(defn eval-let* [exp env]
  (let [eenv (extend-env env '() '())
        vars (second exp)]
    (doseq [[b e] vars]
      (swap! (first eenv) assoc b (form-eval e eenv)))
    (form-eval (nth exp 2) eenv)))

(defn eval-defmacro [exp env]
  (let [[a1 a2 a3] env
        mbody      (with-meta (form-eval a3 env) {:ismacro true})]
    (swap! (first env) assoc a2 mbody)))

(defn is-macro-call [exp env]
  (and (seq? exp)
       (symbol? (first exp))
       (env/env-find env (first ast))
       (:ismacro (meta (env/env-get env (first ast))))))

(defn macroexpand [exp env]
  (loop [exp ast]
    (if (is-macro-call ast env)
      (let [mac (env/env-get env (first ast))]
        (recur (apply mac (rest ast))))
      ast)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
        (pr "repl => "))
      (flush)
      (let [input (read-line)
            form (str x input)]
        (cond
          (= input "quit") "bye!"
          (insta/failure? (validate form)) (recur form)
          :else (do
                  (prn  (form-eval (read-string form) global-env))
                  (recur "")))))))


(defn -main  [& args] (repl))


(define-macro when
  (lambda (test . branch)
          (list 'if test
                (cons 'begin branch))))