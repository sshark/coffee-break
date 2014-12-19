(def m [:a [
             [:b [
                   [:e] [:f] [:g]]]
             [:c [
                   [:h [
                         [:i [
                               [:j]]]]]]]
             [:d [
                   [:k]]]]])

(defn path [root goal]
  (letfn [(_path [currentNode queue]
            (def node (first currentNode))
            (def children (second currentNode))
            (def history (nth currentNode 2))
            (if (= node goal)
              (conj history node )
              (do
                (def newQueue (into (map #(vector (first %1) (second %1) (conj history node)) children) queue))
                (if (empty? newQueue)
                  nil
                  (do
                    (recur (first newQueue) (rest newQueue)))
                  )))
            )]
    (_path (vector (first root) (second root) []) [])))

(path m :j)

(defn simple [x]
  (let [y (+ x 1)]
    (let [z (* 2 y)]
      (println "hello")
      (+ z y)
    )))

(defn sum [l]
  (letfn [(_sum [x acc]
    (if (= x 0)
      acc
      (recur (- x 1) (+ x acc))))]
  (_sum l 0)))

(defn flat-map [f xs] (filter identity (map (fn [x] (f x))  xs)))       

(defn show [x]
    (let [[current path-to-root] x]
        (map #(vector (%1 current) (conj path-to-root current)) [identity identity])))

(show [[1 2] [[3 5]]])
        
 

