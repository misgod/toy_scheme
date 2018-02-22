(ns toy-scheme.core
  (:require [clojure.tools.trace :refer :all]
            [clojure.test :as t]
            [instaparse.core :as insta]

            ))




(declare eval-seq scan-var form-apply extend-env setup-env eval-define eval-if eval-cond)

(defn form-eval [exp env]
  ;;(prn "eval" exp "===>\n" (keys (first (persistent! (env)))) )
  (letfn [(constant? [x]
            (or (number? x) (string? x) (nil? x) (instance? Boolean x)))
          (make-proc [param body env ]
            (list 'procedure  param body env))
          (lookup-var [exp env]
            (scan-var exp
                      env
                      #(get (deref %) exp)
                      #(throw (Exception. "wrong binding id!")) ))
        
          (eval-assignment [exp env]
            (let [var (second exp)
                  val (nth exp 2)]
              (scan-var var
                        env
                        #(swap! % assoc var (form-eval val env))
                        #(throw (Exception. "wrong binding id!")))))]
    
    (cond (constant? exp) exp
          (symbol? exp) (lookup-var exp env) 
          (= (first exp) 'quote) (second exp) 
          (= (first exp) 'if) (eval-if exp env)
          (= (first exp) 'cond) (eval-cond exp env)
          (= (first exp) 'define) (eval-define exp env)
          (= (first exp) 'begin) (eval-seq (rest exp) env)
          (= (first exp) 'set!) (eval-assignment exp env)
          (= (first exp) 'lambda) (make-proc (second exp)
                                             (drop 2 exp)
                                             env)
          :else (form-apply (form-eval (first exp) env)
                            (map #(form-eval % env) (rest exp))))))


(defn form-apply [proc args]
  ;;(prn "apply:" (take 3 proc) "=>" args)
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
                      })
(defn setup-env []
  (-> '()
      (extend-env (keys primitive-procs)
                  (map #(list 'primitive %) (vals primitive-procs)))
      ;;above is base env
      (extend-env '() '())))

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
    (swap! e assoc var val)
    "ok"))

(defn eval-if [exp env]
  (if (form-eval (second exp) env)
    (form-eval  (nth exp 2) env)
    (form-eval  (nth exp 3) env)))

(defn eval-cond [exp env]
 ; (let [(rest exp)])


  )


(defn scan-var [var env action not-found]
  (let [e (first env)]
    (cond (nil? e) (not-found)
          (contains? @e var) (action e)
          :else (recur var (rest env) action not-found))))

(defn eval-seq [exps env]
  (reduce #(form-eval %2 env) nil exps))


(defn extend-env [env vars vals]
  (if (= (count vars) (count vals))
    (cons (atom (zipmap vars vals)) env)
    (throw (Exception. "arguments not match!" env))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))



(t/is (= 34 (form-eval '(- (+ (* 2 3 5) 1 6) (+ 1 2)) (setup-env))))
(t/is (= 2 (form-eval '(if false 1 2) (setup-env)))) 
(t/is (= 1 (form-eval '(if true 1 2) (setup-env))))

(t/is (= 'a (form-eval '(quote a) (setup-env))))

(t/is (= 11 (form-eval '((lambda (a b) (+ a b)) 6 5) (setup-env))))
(t/is (= 22 (form-eval '((lambda (x) (* 2 x)) ((lambda (a b) (+ a b)) 6 5)) (setup-env))))

(t/is (= 6 (form-eval '((lambda (x) (begin (set! x  (+ x 1)) x)) 5) (setup-env))))


(t/is (= 6 (form-eval '(begin
                       (define x 5)
                       (define x (+ x 1))
                       (lambda () (define x 10))
                       x) (setup-env))))
(t/is (= 15 (form-eval '(begin
                         (define (f x y) (* x y))
                         (f 3 5))
                       (setup-env))))

(t/is (= 120 (form-eval '(begin
                          (define (f x y)
                            (if (eq? x 1)
                              y
                              (f (- x 1) (* x y))))
                          (f 5 1))
                        (setup-env))))




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




