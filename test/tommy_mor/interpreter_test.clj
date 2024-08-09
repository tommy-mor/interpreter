(ns tommy-mor.interpreter-test
  (:require [tommy-mor.interpreter :refer :all]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.rcf]))


(hyperfiddle.rcf/enable!)

(tests
 (trim-plus 'arst+) := 'arst)


(tests
 (split-vec [1 2 3 'split 5 6] 'split) := [[1 2 3] [5 6]]
 (split-vec [1 2 3 'split] 'split) := [[1 2 3] []]
 (split-vec ['split 4 5] 'split) := [[] [4 5]]
 (split-vec [4 5] 'split) := [[4 5] []])

(tests
 "loops"
 ;; loops need a condition value on the stack to pop
 (do (defstackfn f [] (loop)) (f)) :throws java.lang.AssertionError
 (do (defstackfn f []
       ;; for (int i = 4; i != 0; i--)
       4 ;; load 
       true ;; will check/pop at beginning of every loop iteration top value on stack
       (loop
           !i+  ;; 
         !i (invoke> println 1) <pop>
         
         !i
         (invoke> dec 1)
         !i+

         !i 0 (invoke> not= 2))) (f)) := 0
 
 (do (defstackfn f []
       4 !i+

       10 !x+

       !x
       true
       (loop
           !x+ ;; read iter x from stack
         
         (invoke> println 0) <pop>

         !x (invoke> dec 1) !x+ <pop>


         !x ;; int i = 10
         true
         (loop
             !i+ ;; int i = 10
           !i (invoke> print 1) <pop>

           !i (invoke> dec 1) !i+ <pop>

           !i ;; for the !i+

           0 !i (invoke> not= 2))

         !x ;; for the !x+ at the top of the loop
         1 !x (invoke> not= 2) ;; for the pop
         )) (f)) := 1
 "fib"
 (do (defstackfn f []
       ;; for (int i = 0; i != 10; i++)
       "fibbonaci sequence" (invoke> println 1) <pop>

       0
       1
       
       0
       true 
       (loop
           !i+ <pop>
           
           !i
           (invoke> inc 1)
           !i+ <pop>

           !b+ <pop>
           !a+ <pop>

           !b
           !a !b (invoke> + 2) !c+

           !c (invoke> println 1) <pop>
           
           !i
           !i 10 (invoke> not= 2)
           )) (f))
 )

(tests
 "virtual method calls"
 (do (defstackfn f [] "arst" (invoke> .toUpperCase 1)) (f)) := "ARST"
 (do (defstackfn f [] (invoke> .get 2)) (f)) :throws clojure.lang.ExceptionInfo
 (do (defstackfn f [] 2 [1 2 3] (invoke> .get 2)) (f)) := 3)

(tests
 "anonymous stack functions"
 ;; produces a function callable from clojure
 (do (defstackfn f [] 3 (fn [!a] !a !a (invoke> + 2))) ((f) 3)) := 6

 ;; invoke!> grabs the function off of the stack, as well as the arguments
 (do (defstackfn f [] 5 (fn [!a] !a !a (invoke> + 2)) (invoke!> 2)) (f)) := 10
 (do (defstackfn f [] 5 (fn [!a] !a !a (invoke> + 2)) (invoke!> 3)) (f)) :throws clojure.lang.ExceptionInfo
 
 ;; 1st argument = first item on stack, 2nd arg = 2nd item on stack (feels like its reversed)
 (do (defstackfn f [] "world" "hello " (invoke> str 2)) (f)) := "hello world"

 (do (defstackfn f [] "world" "hello " (fn [!a !b] !b !a (invoke> str 2)) (invoke!> 3)) (f)) := "hello world"

 "with lexical closures"
 (do (defstackfn f [] 10 !b+ 5 !c+ !b (fn [!a] !a !c (invoke> + 2)) (invoke!> 2)) (f)) := (+ 5 10)
 
 "nested functions, carrying values with closures"
 (do (defstackfn f []
       10 !b+
       5 !c+
       (fn [!a]
         (fn [!b] !a !b (invoke> * 2)))
       !f+

       !b !f (invoke!> 2) !f2+
       !c !f2 (invoke!> 2)) (f)) := 50
 
 (do (defstackfn f []
       (fn [!a]
         10 !c+
         (fn [!b] !a !b !c (invoke> + 3)))
       !f+

       3 !f (invoke!> 2) !f2+
       4 !f2 (invoke!> 2)) (f)) := 17
 
 "multi arity functions"
 (do (defstackfn f []
       10 !b+
       5 !c+
       (fn
         ([!a] !a 100 (invoke> + 2))
         ([!a !b] !a !b (invoke> + 2))) !f+

       !b !c !f (invoke!> 3)) (f)) := 15
 
 (do (defstackfn f []
       (fn
         ([!a] !a 100 (invoke> + 2))
         ([!a !b] !a !b (invoke> + 2))) !f+

       10 !f (invoke!> 2) !result+

       10 !result !f (invoke!> 3)) (f)) := 120
 
 "varargs"
 (do (defstackfn f []
       5 !b+
       (fn [& !rest] !rest (invoke> count 1)) !f+

       !b !b !b !b !b !f (invoke!> 5)) (f)) := 4
 
 (do (defstackfn f []
       (fn
         ([!a] !a 3 (invoke> + 2))
         ([!a & !rest] !rest (invoke> count 1) !a (invoke> + 2))) !f+

       10 !f (invoke!> 2) !result+

       10 !result 1 2 3 4 5 6 7 8 !f (invoke!> 10)) (f)) := (+ 8 8)
 
 (do (defstackfn f []
       (fn [!x] !x (invoke> inc 1)) !f+

       [1 2 3] !f (invoke> map 2)) (f)) := [2 3 4]
 
 (do (defstackfn f []
       
       (fn [!x]
         2 !x (invoke> = 2)
         (if> 3 else> 4))
       !f+

       [1 2 3] !f (invoke> map 2)) (f)) := [4 3 4]
 
 (do (defstackfn f []
       
       (fn [!x]
         2 !x (invoke> = 2)
         (if> 3 else> 0 true (loop !i+

                               !i (invoke> inc 1) !i+

                               !i 10 (invoke> > 2))))
       !f+

       [1 2 3] !f (invoke> map 2)) (f)) := [10 3 10]
 )

(tests
 "constants"
 
 
 (do (defstackfn f []) (f)) := nil  ;; stack is empty. might make sense for this to throw
 (do (defstackfn f [] "ars") (f)) := "ars"
 (do (defstackfn f [] true) (f)) := true
 
 (do (defstackfn f [] 3 4) (f)) := 4
 
 "variables"
 (do (defstackfn f [!a] !a) (f 3)) := 3
 (do (defstackfn f [!a] 3 !a) (f 4)) := 4
 
 ;; (defstackfn f [!a] !a !b) :throws java.lang.RuntimeException ;; rcf cant catch at macroexpransion time I guess
 
 (do (defstackfn f [!a !b] !b) (f 2 4)) := 4

 "assigning"
 (do (defstackfn f [] !b+) (f)) :throws java.lang.AssertionError
 (do (defstackfn f [] 3 !b+ 4 !b) (f)) := 3
 
 (do (defstackfn f [] 3 !a+ 5 !b+ !a) (f)) := 3
 
 "shadowing"
 (do (defstackfn f [!a] 3 !a+ 5 !b+ !a) (f 4)) := 3
 (do (defstackfn f [] 3 !a+ 5 !a+ 6 6 6 !a) (f)) := 5

 "function apply"
 (do (defstackfn f [] 3 4 (invoke> = 2)) (f)) := false
 (do (defstackfn f [] 3 3 (invoke> = 2)) (f)) := true

 (do (defstackfn f [] 3 "arst" (invoke> str 2)) (f)) := "arst3"
 
 (do (defstackfn f [] 3 4 5 (invoke> str 3)) (f)) := "543"
 (do (defstackfn f [] 3 4 (invoke> str 3)) (f)) :throws clojure.lang.ExceptionInfo
 
 (do (defstackfn f [] 3 4 (invoke> + 2)) (f)) := 7
 (do (defstackfn f [] 3 4 5 (invoke> + 2) (invoke> * 2)) (f)) := (* 3 9)

 "pop"
 (do (defstackfn f [] 3 4 5 <pop> (invoke> + 2)) (f)) := 7
 (do (defstackfn f [] 3 4 5 <pop>) (f)) := 4
 (do (defstackfn f [] 3 4 5 <pop> <pop>) (f)) := 3
 
 (do (defstackfn f [] <pop>) (f)) :throws java.lang.IllegalStateException

 "if"
 (do (defstackfn f [] (if> 3 4)) (f)) :throws java.lang.IllegalStateException
 (do (defstackfn f [] true (if> 3 4)) (f)) := 4
 (do (defstackfn f [] true (if> 3 else> 4)) (f)) := 3
 (do (defstackfn f [] false (if> 3 else> 4)) (f)) := 4
 (do (defstackfn f [] true (if> 3 4 (invoke> * 2) else> 4)) (f)) := 12
 (do (defstackfn f [] 3 true (if> else> 4)) (f)) := 3
 (do (defstackfn f [] 3 false (if> else> 4) (invoke> * 2)) (f)) := 12
 (do (defstackfn f [] false (if> else> true (if> 3 else> 4))) (f)) := 3

 (do (defstackfn f [!a !b !c]
       !a
       !b
       (invoke> + 2)
       !v1+
       !c
       !c
       <pop>
       2
       (invoke> * 2)
       !v2+
       (invoke> = 2)
       (if>
           !v1
         !v2
         (invoke> - 2)
         else>
         "false!!"
         (invoke> println 1)
         <pop>
         !v1
         !v2
         (invoke> * 2)))
     (f 1 2 4)) := 24)
