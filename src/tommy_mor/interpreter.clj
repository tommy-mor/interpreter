(ns tommy-mor.interpreter
  (:gen-class))


(defn compile [program]
  (let [tape (gensym 'tape)]
    `(let [~tape (atom [])]
       ~@(for [expr program]
           (cond (symbol? expr) `(swap! ~tape conj ~expr)
                 true (println 'unknown (type expr)))))))

(defmacro defstackfn [name args & program]
  (let [result (compile program)]
    `(defn ~name ~args ~result)))

(macroexpand '(defstackfn f [!a] !a 2 3 4))
(def f (clojure.core/fn ([!a] (clojure.core/let [tape3430 (clojure.core/atom [])] (clojure.core/swap! tape3430 clojure.core/conj !a) nil nil nil))))
(f 1)

(def f (fn [!a]
         (let [tommy-mor.interpreter/tape (clojure.core/atom [])]
           (swap! tape conj (clojure.core/unquote expr))
           nil
           nil
           nil)))
