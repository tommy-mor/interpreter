(ns tommy-mor.interpreter-test
  (:require [tommy-mor.interpreter :refer :all]
            [hyperfiddle.rcf :refer [tests]]
            [hyperfiddle.rcf]))

(hyperfiddle.rcf/enable!)


(tests
 (split-vec [1 2 3 'split 5 6] 'split)
 (split-vec [1 2 3 'split] 'split) := [[1 2 3] []]
 (split-vec ['split 4 5] 'split) := [[] [4 5]]
 (split-vec [4 5] 'split) := [[4 5] []])

(tests
 "constants"
 (do (defstackfn f [] 3) (f)) := 3
 
 (do (defstackfn f [] "ars") (f))
 (do (defstackfn f [] true) (f)) := true
 
 (do (defstackfn f [] 3 4) (f)) := 4
 
 "variables"
 (do (defstackfn f [!a] !a) (f 3)) := 3
 (do (defstackfn f [!a] 3 !a) (f 4)) := 4
 
 ;; (defstackfn f [!a] !a !b) :throws java.lang.RuntimeException ;; rcf cant catch at macroexpransion time I guess
 
 (do (defstackfn f [!a !b] !b) (f 2 4)) := 4

 "assigning"
 (do (defstackfn f [] 3 !b+ 4 !b) (f)) := 3
 
 (do (defstackfn f [] 3 !a+ 5 !b+ !a) (f)) := 3
 
 "shadowing"
 (do (defstackfn f [!a] 3 !a+ 5 !b+ !a) (f 4)) := 3
 (do (defstackfn f [] 3 !a+ 5 !a+ 6 6 6 !a) (f)) := 5

 "function apply"
 (do (defstackfn f [] 3 4 (invoke> = 2)) (f)) := false
 (do (defstackfn f [] 3 3 (invoke> = 2)) (f)) := true

 (do (defstackfn f [] 3 "arst" (invoke> str 2)) (f)) := "3arst"
 
 (do (defstackfn f [] 3 4 5 (invoke> str 3)) (f)) := "345"
 (do (defstackfn f [] 3 4 (invoke> str 3)) (f)) :throws clojure.lang.ExceptionInfo
 
 (do (defstackfn f [] 3 4 (invoke> + 2)) (f)) := 7
 (do (defstackfn f [] 3 4 5 (invoke> + 2) (invoke> * 2)) (f)) := (* 3 9)

 "pop"
 (do (defstackfn f [] 3 4 5 <pop> (invoke> + 2)) (f)) := 7
 (do (defstackfn f [] 3 4 5 <pop>) (f)) := 4
 (do (defstackfn f [] 3 4 5 <pop> <pop>) (f)) := 3
 
 (do (defstackfn f [] <pop>) (f)) :throws java.lang.IllegalStateException

 "if"
 (do (defstackfn f [] true (if> 3 4)) (f)) := 4
 (do (defstackfn f [] true (if> 3 else> 4)) (f)) := 3
 (do (defstackfn f [] false (if> 3 else> 4)) (f)) := 4
 (do (defstackfn f [] true (if> 3 4 (invoke> * 2) else> 4)) (f)) := 12
 (do (defstackfn f [] 3 true (if> else> 4)) (f)) := 3
 (do (defstackfn f [] 3 false (if> else> 4) (invoke> * 2)) (f)) := 12

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
