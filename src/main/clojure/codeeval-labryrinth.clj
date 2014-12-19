(with-open [rdr (clojure.java.io/reader (first *command-line-args*))]
  (do
  ; Read each line ignoring empty ones
  (def content (apply str (map #(str %1 "\n") (remove empty? (line-seq rdr)))))

    (defn map-maze [strMaze]
      (def v (map #(vec %1) (.split strMaze "\n")))
      (vec (map-indexed (fn [a b] (vec (map-indexed (fn [c x] (if (= \* x) nil [a c])) b))) v))
      )

    (def mapped-maze (map-maze content))

    (defn make-blueprint [raw-maze]
      (fn [y x] (get (get raw-maze y) x)))

    (def blueprint (make-blueprint mapped-maze))

    (def not-nil? (complement nil?))

    (defn move-up [pos]
      (let [[y x] pos]
        (blueprint (dec y) x)))

    (defn move-right [pos]
      (let [[y x] pos]
        (blueprint y (inc x))))

    (defn move-down [pos]
      (let [[y x] pos]
        (blueprint (inc y) x)))

    (defn move-left [pos]
      (let [[y x] pos]
        (blueprint y (dec x))))

    (def start (first (drop-while nil? (first mapped-maze))))

    (def exit (first (drop-while nil? (last mapped-maze))))

    ; flatMap equivalent??
    (defn flat-map [g f xs] (filter #(g %1) (map (fn [x] (f x)) xs)))

    ; instead of create new positions, create new markers. marker has a ancesteral path
    (defn neigbours [x]
      (let [all-moves [move-up move-left move-down move-right]
            [current path-to-root] x]
        (flat-map #(not-nil? (first %1)) #(vector (%1 current) (conj path-to-root current)) all-moves)))

    (neigbours [[1 5] []])

    ; uses Breadth First Search (BFS) to solve given maze
    (defn bfs-path [origin goal]
      (letfn [(_bfs-path [marker queue history]
                (let [pos (first marker)]
                  (if (= pos goal) (vector pos (conj (second marker) pos))
                    (do
                      (def new-neighbours (neigbours marker))
                      (def new-queue (filter #(nil? (history (first %1))) (into queue new-neighbours)))
                      (if (nil? new-queue) nil
                        (recur (first new-queue) (vec (rest new-queue)) (conj history (ffirst new-queue)))
                        )))))]
        (_bfs-path [origin []] [] #{origin})))

    (def solution (second (bfs-path start exit)))

    (def solution-set (set solution))

    (def solved-maze-layout (map #(map (fn [x] (cond
                                                 (= x nil) \*
                                                 (solution-set x) \+
                                                 :else \space)) %1) mapped-maze))

    (doseq [row solved-maze-layout] (println (apply str row)))
    ))