(ns tommy-mor.interpreter
  (:require [clojure.string :as str]
            [hyperfiddle.rcf :refer [tests]]
            [com.rpl.specter :refer :all])
  (:gen-class))

(defn split-vec [v on]
  (let [[a b] (split-with #(not= on %) v)]
    [(vec a) (vec (rest b))]))

(comment
  (split-vec [1 2 3 'on 4 5 6] 'on))

(defn trim-plus [sym]
  (symbol (transform [LAST] NONE (str sym))))

(defn compile-expr [tape [expr & program]]
  (cond (nil? expr)
        `(last (deref ~tape))
        
        (and (symbol? expr)
             (str/starts-with? (str expr) "!")
             (str/ends-with? (str expr) "+"))
        `(let [~(trim-plus expr) (last (deref ~tape))]
           (assert (>= (count (deref ~tape)) 1) "cant read value from empty stack")
           ~(compile-expr tape program))
        
        (and (symbol? expr)
             (str/starts-with? (str expr) "!"))
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
          (cond (vector? args)
                `(do (swap! ~tape conj (fn ~args ~(compile-expr tape body)))
                     ~(compile-expr tape program))
                
                (list? args) ;; multi arity fn
                `(do (swap! ~tape conj (fn ~@(for [[args & body] (rest expr)]
                                               `(~args ~(compile-expr tape body)))))
                     ~(compile-expr tape program))))
        
        (and (seq? expr) (= (first expr) 'loop))
        (let [[_ & body] expr]
          ;; ive decided that loops don't have values, they are only for side effects
          `(do (loop []
                 
                 (assert (>= (count (deref ~tape)) 1) "loop must have test value to pop")
                 (let [v# (last (deref ~tape))]
                   (swap! ~tape pop)
                   (when v#
                     ~(compile-expr tape body)
                     (recur))))
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






