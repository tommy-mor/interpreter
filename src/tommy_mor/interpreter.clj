(ns tommy-mor.interpreter
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]])
  (:gen-class))

(hyperfiddle.rcf/enable!)



(defn compile-expr [tape [expr & program]]
  (cond (nil? expr)
        `(last (deref ~tape))
        
        (and (symbol? expr)
             (str/starts-with? (name expr) "!"))
        
        `(do (swap! ~tape conj ~expr)
             ~(compile-expr tape program))
        
        (or (string? expr) (number? expr))
        `(do (swap! ~tape conj ~expr)
             ~(compile-expr tape program))
        
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
 
 (do (defstackfn f [] "ars") (f)) := "ars"
 
 (do (defstackfn f [] 3 4) (f)) := 4
 
 (do (defstackfn f [!a] !a) (f 3)) := 3
 (do (defstackfn f [!a] 3 !a) (f 4)) := 4
 
 ;; (defstackfn f [!a] !a !b) :throws java.lang.RuntimeException ;; rcf cant catch at macroexpransion time I guess
 
 (do (defstackfn f [!a !b] !b) (f 2 4)) := 4


 )


(defstackfn f [!a] !a 2 3 4)

(f 2)


