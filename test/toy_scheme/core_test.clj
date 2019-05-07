(ns toy-scheme.core-test
  (:require [clojure.test :refer :all]
            [toy-scheme.core :refer :all]))


(def global-env (setup-env))

(def feval #(form-eval % global-env))


(deftest my-test
  (testing "op & if"
    (is (= 34 (feval '(- (+ (* 2 3 5) 1 6) (+ 1 2)))))
    (is (= 2  (feval '(if false 1 2))))
    (is (= 1  (feval '(if true 1 2))))
    (is (= 'a (feval '(quote a)))))
  
  (testing "lambda"
    (is (= 11 (feval '((lambda (a b) (+ a b)) 6 5))))
    (is (= 22 (feval '((lambda (x) (* 2 x)) ((lambda (a b) (+ a b)) 6 5)))))

    (is (= 15 (feval '((lambda (a b . c) (+ a  b (count c))) 6 5 4 3 2 1))))
    (is (= 6  (feval '((lambda (x) (begin (set! x  (+ x 1)) x)) 5)))))

  (testing "binding, define"
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
                            (+ y x))))))

  (testing "macro"
    (feval '(defmacro when
              (lambda (test . branch)
                      (list 'if test
                            (cons 'begin branch)
                            nil))))
    
    (is (= true  (feval '(let* ((x false))
                            (begin
                             (when (= 1 1)
                               (set! x true))
                             x)))))
    
    (is (= false  (feval '(let* ((x false))
                            (begin
                             (when (= 1 2)
                               (set! x true))
                             x)))))))