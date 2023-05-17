(ns dijkstra-2.core)

(defrecord Graph [verticies edges])
(defrecord Vertex [label lat lon neighbors status distance])
(defrecord Edge [from to label weight])
(defrecord Neighbor [label weight])

(defn make-neighbor [label weight]
  (Neighbor. label weight))

(defn make-graph []
  (Graph. (ref {}) (ref '())))

(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defn make-vertex [label lat lon]
  (Vertex. label lat lon (ref '()) (ref 0) (ref ##Inf)))

(defn graph-add-vertex! [graph label lat lon]
  (dosync
   (ref-set
    (:verticies graph)
    (assoc @(:verticies graph) label (make-vertex label lat lon)))))

(def g (make-graph))

(defn graph-add-edge! [graph from to label weight]
  (let [edges (:edges graph)
        verticies @(:verticies graph)
        from-vertex (get verticies from)
        to-vertex (get verticies to)
        from-neighbors (:neighbors from-vertex)
        to-neighbors (:neighbors to-vertex)]
    (dosync
     (ref-set edges (conj @edges (make-edge from to label weight)))
     (ref-set from-neighbors (conj @from-neighbors
                                   (make-neighbor to weight)))
     (ref-set to-neighbors (conj @to-neighbors
                                 (make-neighbor from weight))))))

(load-file "/Users/matvii_kharchenko/Desktop/PCU/ALDS/assignment2/e-roads-2020-full.clj")


(defn graph-reset [graph]
  (doseq [vertex (vals @(:verticies graph))]
    (dosync (ref-set (:status vertex) (ref 0))
            (ref-set (:distance vertex) (ref ##Inf))
            (ref-set (:neighbors vertex) (ref '())))))

(defn vertex-unseen? [vertex]
  (= @(:status vertex) 0))


(defn bfs-add-to-queue [queue graph neighbors]
  (loop [queue (reverse queue)
         neighbors neighbors]
    (if (empty? neighbors) (reverse queue)
        (let [neighbor-record (first neighbors)
              neighbor (get @(:verticies graph) (:label neighbor-record))]
          (if (vertex-unseen? neighbor)
            (do
              (dosync (ref-set (:status neighbor) 1))
              (recur (conj queue (:label neighbor-record))
                     (rest neighbors)))
            (recur queue (rest neighbors)))))))

(defn add-distance [vertex graph]
  (let [distance @(:distance vertex)]
    (doseq [neighbor @(:neighbors vertex)]
      (let [neighbor-label (:label neighbor)
            neighbor-vertex (get @(:verticies graph) neighbor-label)
            neighbor-distance @(:distance neighbor-vertex)
            neighbor-weight (+ (:weight neighbor) distance)]
        (if (= neighbor-distance ##Inf)
          (dosync
           (ref-set (:distance neighbor-vertex) neighbor-weight))
          (dosync
           (when (> neighbor-distance neighbor-weight)
             (ref-set (:distance neighbor-vertex) neighbor-weight))))))))

(defn graph-bfs! [graph start]
  (dosync
   (ref-set (:distance (get @(:verticies graph) start)) 0))
  (loop [queue (list start)]
    (when (not (empty? queue))
      (let
       [current-label (first queue)
        current-vertex (get @(:verticies graph) current-label)]
        (dosync (ref-set (:status current-vertex) 2))
        (add-distance current-vertex graph)
        (dosync (ref-set (:status current-vertex) 3))
        (recur
         (bfs-add-to-queue (rest queue) graph @(:neighbors current-vertex)))))))

(defn find-best-neighbor [graph label]
  (let [neighbors @(:neighbors (get @(:verticies graph) label))]
    (loop [neighbors neighbors
           best-distance ##Inf
           best-label nil]
      (if (empty? neighbors)
        (if (= best-distance ##Inf) nil best-label)
        (let [currentDistance @(:distance (get @(:verticies graph) label))
              currentLabel  (:label (first neighbors))
              currentNeighbor (get @(:verticies graph) currentLabel)
              neighborWeight (:weight (first neighbors))
              currentNeighborDistance @(:distance currentNeighbor)
              isMoveValid (=
                           (- currentDistance neighborWeight) currentNeighborDistance)]
          (println "neighborWeight" neighborWeight "\n"
                   "currentDistance" currentDistance "\n"
                   "currentDistance - neighborWeight" "\n"
                   (- currentDistance neighborWeight) "\n"
                   "\n"
                   "isMoveValid" isMoveValid "\n"
                   "currentNeighborDistance" currentNeighborDistance "\n"
                   "currentLabel" currentLabel "\n\n" "*********")

          (if (and (< currentNeighborDistance best-distance) isMoveValid)
            (recur (rest neighbors) currentNeighborDistance currentLabel)
            (recur (rest neighbors) best-distance best-label)))))))

(defn dijsktra-weights [from to graph]
  ;; (graph-reset graph)
  (graph-bfs! graph to)
  (loop [from from
         places (list from)]
    (let [closestNeighbor (find-best-neighbor graph from)]
      (println  "------ closestNeighbor" closestNeighbor "------")
      (if (nil? closestNeighbor) "There is no path"
          (if (= closestNeighbor to)
            (loop [placesList (reverse (conj places to))
                   index 1]
              (when (not (empty? placesList))
                (let [currentPlace (first placesList)
                      distance @(:distance (get @(:verticies g) currentPlace))]
                  (println (str index ". " distance "km " currentPlace))
                  (recur (rest placesList) (inc index)))))
            (recur closestNeighbor
                   (conj places closestNeighbor)))))))

(dijsktra-weights "Manchester" "Dover" g)
(dijsktra-weights "Brest, Belarus" "Prague" g)

(graph-bfs! g "Dover")

(defn visited? [vertex]
  (not (= @(:status vertex) 0)))

(defn status-color [s]
  (case s
    0 "#ffffff"
    1 "#01afd1"
    2 "#f8e71c"
    3 "#e6e6e6"
    "#ff0000"))

;; TODO: status colors
(defn graph-print-dot
  ([graph]
   (graph-print-dot graph (fn [v] true)))
  ([graph pred?]
   (graph-print-dot graph pred? false))
  ([graph pred? inverted?]
   (let [vertices (doall (map-indexed
                          (fn [idx itm] [idx itm])
                          (filter pred? (vals @(:verticies graph)))))
         l2i (apply hash-map (flatten (doall (map
                                              (fn [p] [(:label (second
                                                                p)) (first p)]) vertices))))]
     (println "graph G{")
     (doseq [vertexpair vertices]
       (let [idx (first vertexpair)
             vertex (second vertexpair)
             lat (:lat vertex)
             lon (:lon vertex)]
         (println (str "v" idx
                       "[label=\"{"
                       (:label vertex)
                        ;;"|status: " @(:status vertex)
                       "|distance: " (or @(:distance vertex) "--")
                      ;;  "|estimate: " (if @(:estimate vertex)
                      ;;                  (int (Math/floor @(:estimate
                      ;;                                     vertex)))
                      ;;                  "--")
                       "}\""
                       ",shape=record,style=filled,fillcolor=\""
                       (status-color @(:status vertex))
                       "\",pos=\""
                       (if inverted? lat lon)
                       ","
                       (if inverted? lon lat)
                       "\""
                       "]"))))
     (let [edges (filter
                  (fn [a]
                    (< (first a) (second a)))
                  (apply
                   concat
                   (for [vertex vertices]
                     (map (fn [n]
                            [(first vertex)
                             (get l2i (:label n))
                             (:weight n)
                             (:road n)])
                          @(:neighbors (second vertex))))))]
       (doseq [edge edges]
         (println (str "v" (first edge) " -- v" (second edge)
                       " [label=\"" (nth edge 3) ", " (nth edge 2) "km\"]"))))
     (println "}"))))

(graph-print-dot g visited?)

