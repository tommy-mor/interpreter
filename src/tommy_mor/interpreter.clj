(ns tommy-mor.interpreter
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]])
  (:gen-class))

(hyperfiddle.rcf/enable!)

(defn trim-plus [sym]
  (let [idx (dec (count (name sym)))]
    (symbol (subs (name sym) 0 idx))))

(tests
 (trim-plus 'arst+) := 'arst)

(defn compile-expr [tape [expr & program]]
  (cond (nil? expr)
        `(last (deref ~tape))
        
        (and (symbol? expr)
             (str/starts-with? (name expr) "!")
             (str/ends-with? (name expr) "+"))
        `(let [~(trim-plus expr) (last (deref ~tape))]
           ~(compile-expr tape program))
        
        (and (symbol? expr)
             (str/starts-with? (name expr) "!"))
        
        `(do (swap! ~tape conj ~expr)
             ~(compile-expr tape program))
        
        (or (string? expr) (number? expr) (boolean? expr))
        `(do (swap! ~tape conj ~expr)
             ~(compile-expr tape program))

        (= (first expr) 'invoke>)
        (let [[_ f arity] expr]
          (assert (number? arity) "arity must be a number")
          `(if (> ~arity (count (deref ~tape)))
             (throw (ex-info "not enough values on stack" {:stack ~tape :arity ~arity}))
             (let [res# (apply ~f (take-last ~arity (deref ~tape)))]
               (reset! ~tape (vec (drop-last ~arity (deref ~tape))))
               (swap! ~tape conj res#)
               ~(compile-expr tape program))))
       

        
        true
        (throw (ex-info "unknown form" {:form expr}))))

(defn compile [program]
  (let [tape (gensym 'tape)]
    `(let [~tape (atom [])]
       ~(compile-expr tape program))))

(defmacro defstackfn [name args & program]
  (let [result (compile program)]
    `(defn ~name ~args ~result)))

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
 
 )

(f)


(defstackfn f [!a] !a 2 3 4)

(f 2)


