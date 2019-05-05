(ns toy-scheme.core-test
  (:require [clojure.test :refer :all]
            [toy-scheme.core :refer :all]))

(def feval #(form-eval % (setup-env)))


(deftest my-test
  (testing "xxxx"
    (is (= 34 (feval '(- (+ (* 2 3 5) 1 6) (+ 1 2)))))
    (is (= 2  (feval '(if false 1 2))))
    (is (= 1  (feval '(if true 1 2))))
    (is (= 'a (feval '(quote a))))
    (is (= 11 (feval '((lambda (a b) (+ a b)) 6 5))))
    (is (= 22 (feval '((lambda (x) (* 2 x)) ((lambda (a b) (+ a b)) 6 5)))))

    (is (= 15 (feval '((lambda (a b . c) (+ a  b (count c))) 6 5 4 3 2 1))))
    (is (= 6  (feval '((lambda (x) (begin (set! x  (+ x 1)) x)) 5))))


    (is (= 6  (feval '(begin
                       (define x 5)
                       (define x (+ x 1))
                       (lambda () (define x 10))
                       x))))

    (is (= 15 (feval '(begin
                       (define (f x y) (* x y))
                       (f 3 5)))))

    ;;recursion
    (is (= 720 (feval '(begin
                        (define (fact x)
                          (if (eq? x 1)
                            1
                            (* x (fact (- x 1)))))
                        (fact 6)))))

    ;;closure
    (is (= 15 (feval '(begin
                       (define (f y)
                         (lambda (x) (+ x y)))
                       ((f 10) 5)))))



    (is (= 10 (feval '(let* ((x 3)
                             (y (+ x 4)))
                            (+ y x)))))))
  



(def eee (setup-env))

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


; (form-eval '(define (fact x)
;               (if (eq? x 1)
;                 1
;                 (* x (fact (- x 1))))) (setup-env))


(form-eval '(begin
           (define (fact x)
             (if (eq? x 1)
               1
               (* x (fact (- x 1)))))
           (fact 6)) eee)

(prn (first eee))