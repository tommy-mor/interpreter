(ns tommy-mor.interpreter
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]])
  (:gen-class))

(defn split-vec [v on]
  (let [[a b] (split-with #(not= on %) v)]
    [(vec a) (vec (rest b))]))

(defn trim-plus [sym]
  (let [idx (dec (count (name sym)))]
    (symbol (subs (name sym) 0 idx))))

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
        
        (or (string? expr) (number? expr) (boolean? expr) (vector? expr))
        `(do (swap! ~tape conj ~expr)
             ~(compile-expr tape program))

        (and (seq? expr) (= (first expr) 'invoke>))
        (let [[_ f arity] expr
              args (for [x (range arity)]
                     `(nth (deref ~tape) (- (count (deref ~tape)) ~x 1) ))]
          `(if (> ~arity (count (deref ~tape)))
             (throw (ex-info "not enough values on stack" {:stack ~tape :arity ~arity}))
             (let [res# (~f ~@args)]
               (reset! ~tape (vec (drop-last ~arity (deref ~tape))))
               (swap! ~tape conj res#)
               ~(compile-expr tape program))))
        
        (and (seq? expr) (= (first expr) 'invoke!>))
        (let [[_ arity] expr
              args (for [x (range arity)]
                     `(nth (deref ~tape) (- (count (deref ~tape)) ~x 1) ))]
          `(if (> ~arity (count (deref ~tape)))
             (throw (ex-info "not enough values on stack" {:stack ~tape :arity ~arity}))
             (let [res# (~@args)]
               (reset! ~tape (vec (drop-last ~arity (deref ~tape))))
               (swap! ~tape conj res#)
               ~(compile-expr tape program))))
        
        (= expr '<pop>)
        `(do (swap! ~tape pop)
             ~(compile-expr tape program))
        
        (and (seq? expr) (= (first expr) 'if>))
        (let [[ifs elss] (split-vec (rest expr) 'else>)]
          `(let [top# (last (deref ~tape))]
             (swap! ~tape pop)
             (if top# ~(compile-expr tape ifs) ~(compile-expr tape elss))
             ~(compile-expr tape program)))
        
        (and (seq? expr) (= (first expr) 'fn))
        (let [[_ args & body] expr]
          `(do (swap! ~tape conj (fn ~args ~(compile-expr tape body)))
               ~(compile-expr tape program)))
        
        
        true
        (throw (ex-info "unknown form" {:form expr}))))

(defn compile [program]
  (let [tape (gensym 'tape)]
    `(let [~tape (atom [])]
       ~(compile-expr tape program))))

(defmacro defstackfn [name args & program]
  (let [result (compile program)]
    `(defn ~name ~args ~result)))






