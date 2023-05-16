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
            neighbor-weight (:weight neighbor)]
        (if (= neighbor-distance ##Inf)
          (dosync
           (ref-set (:distance neighbor-vertex)
                    (+ distance neighbor-weight)))
          (dosync
           (when (> neighbor-distance neighbor-weight)
             (ref-set (:distance neighbor-vertex)
                      (+ distance neighbor-weight)))))))))

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
                   "currentDistance - neighborWeight"
                   (- currentDistance neighborWeight) "\n"
                   "currentNeighborDistance" currentNeighborDistance "\n"
          "currentLabel" currentLabel "\n\n" "*********") 

          (if (and (< currentNeighborDistance best-distance) isMoveValid) 
            (recur (rest neighbors) currentNeighborDistance currentLabel)
            (recur (rest neighbors) best-distance best-label)))))))

(defn dijsktra-weights [from to graph]
  (graph-bfs! graph to)
  (loop [from from
         places (list from)]
    (let [closestNeighbor (find-best-neighbor graph from)]
      (println  "------ closestNeighbor" closestNeighbor "------")
      (if (nil? closestNeighbor) "There is no path"
          (if (= closestNeighbor to)
            (doseq [val (reverse (conj places to))] (println val))
            (recur closestNeighbor
                   (conj places closestNeighbor)))))))

(get @(:verticies g) "Belfast")
(dijsktra-weights "Belfast" "Dublin" g)

