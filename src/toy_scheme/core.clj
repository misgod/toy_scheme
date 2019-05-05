(ns toy-scheme.core
  (:refer-clojure :exclude [macroexpand])
  (:require [clojure.tools.trace :refer :all]
            [clojure.test :as t]
            [instaparse.core :as insta]
            [sc.api]))


(declare macroexpand env-find env-get eval-seq  form-apply extend-env setup-env 
         eval-define eval-lambda eval-if eval-defmacro eval-cond eval-assignment eval-let*)

(defn form-eval [exp env]
  ;;(prn "eval" exp "===>\n" (keys (first (persistent! (env)))) )
  (let [constant? (fn [x] (or (number? x)
                              (string? x)
                              (nil? x)
                              (instance? Boolean x)))

        ;;exp (macroexpand exp env)
        ]
    (cond (constant? exp)         exp
          (symbol? exp)           (env-get exp env)
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
    (env-find var
              env
              #(swap! % assoc var (form-eval val env))
              #(throw (Exception. "wrong binding id!")))))

(defn env-set! [var val]
  )


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

(defn env-find [var env action not-found]
  (loop [env env]
    (let [e (first env)]
      (cond (nil? e)           (not-found)
            (contains? @e var) (action e)
            :else              (recur (rest env))))))

(defn env-get [var env] 
  (env-find var env
            #(get (deref %) var)
            #(throw (Exception. "wrong binding id!"))))


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
  (prn "eval-defmacro" exp) 
  (let [[a1 a2 a3] exp
        mbody      (with-meta a3 {:ismacro true})]
    (prn "--->" mbody)   
    (swap! (first env) assoc a2  mbody)
    (prn "===" (second env) )
    (prn "===="   env)
    nil) )


(defn is-macro-call [exp env]
  (prn "is-macro-call" exp)
  (and (seq? exp)
       (symbol? (first exp))
       (env-find (first exp)
                 env
                 #(:ismacro (meta (get (deref %) (first exp))))
                 #(identity false))))


(defn macroexpand [exp env]
  (prn "macroexpand" exp)
  (loop [exp exp]
    (if (is-macro-call exp env)
      (let [mac (env-get (first exp) env)]
        (recur (form-apply mac (rest exp))))
      exp)))



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

; (def eee (setup-env))

; (try
;   (form-eval
;    '(defmacro when
;       (lambda (test . branch)
;               (list 'if test
;                     (cons 'begin branch)))) eee)
;   (catch Exception e
;     (clojure.stacktrace/print-stack-trace e)))

;  (try
;    (form-eval '(macroexpand (when (= 1 1) (+ 1 1))) eee)
;    (catch Exception e
;      (clojure.stacktrace/print-stack-trace e)))
;  (try
; (form-eval '(define (fact x)
;               (if (eq? x 1)
;                 1
;                 (* x (fact (- x 1))))) (setup-env))
; (catch Throwable e
;      (clojure.stacktrace/print-stack-trace e)))

; ; eee

; (form-eval
;  '(when (= 1 1) (+ 1 1)) eee
;  )
