(ns swarm-project.core
  (use [clojure.core])
  (use [clojure.set])
  (import 
   (java.awt Color Graphics Dimension)
   (java.awt.image BufferedImage)
   (javax.swing JPanel JFrame)))

;; (def colors {:red :blue :green :yellow})

;; global parameters

(def dim 20)

(def microbe-sleep-ms   100)
(def animation-sleep-ms  50)
(def supply-sleep-ms    100)

;; max number of neighbors allowed ?
(def max-neighbors 8)

;; full health of a microbe
(def microbe-life  8)
;; metabolism
(def microbe-metabolism {:rate 8 :split 12})

(def max-nutrients 20)
(def supply-delta 3)

;pixels per world cell
(def scale 10)

(def render-nutrients? true)
(def render-microbes?  true)
(def running false)

(defn toggle-render-nutrients []
  (def render-nutrients? (not render-nutrients?)))

(defn toggle-render-microbes [] 
  (def render-microbes?  (not render-microbes?)))

(defn toggle-running []  
  (def running (not running)))

;; tracking 
;; (def alive-agents (ref {}))
(def microbes-alive (ref  {}))
(def microbes-dead  (ref #{}))

(defn id-generator [] 
  "Generate a unique id for each object"
  (. clojure.lang.RT (nextID)))

;; structures, world
(defstruct cell :mic-count :life :nutrient)

(def world
  (mapv (fn [_] 
          (mapv (fn [_] (ref (struct cell 0 0 10))) 
                (range dim)))
        (range dim)))

(defstruct microbe :id :pos :health :metabolism :children :age)

(defn create-microbe 
  "create a new microbe a location loc"
  [loc init-life metabolism]
  (dosync 
   (let [pos       (place loc)
         mic-count (:mic-count @pos)
         id        (id-generator)
         new-mic   (struct microbe id loc init-life metabolism 0 0)]
     (alter pos assoc :mic-count (inc mic-count))
     ;; add agent to global map
     (alter microbes-alive conj [id (agent new-mic)])
     id))) ;; ?

(defn place [[x y]] 
  (-> world (nth x) (nth y)))

(defmacro bound? [loc]
  `(let [[x# y#] ~loc]
     (and (>= x# 0) (< x# dim) (>= y# 0) (< y# dim))))

(def consumption {[-1 -1] 0.6
                  [-1  0] 1
                  [-1  1] 0.6
                  [ 0 -1] 1
                  [ 0  0] 4
                  [ 0  1] 1
                  [ 1 -1] 0.6
                  [ 1  0] 1
                  [ 1  1] 0.6})

(defn eat [[x y]]
  "must be called inside dosync block"
  (apply + (for [dx [-1 0 1]
                 dy [-1 0 1]]
             (let [i (+ x dx)
                   j (+ y dy)]
               (if (bound? [i j])
                 (let [p  (place [i j])
                       fd (:nutrient @p)
                       n  (consumption [dx dy])]
                   (if (>= fd n)
                     (do (alter p assoc :nutrient (- fd n)) n)
                     (do (alter p assoc :nutrient 0) fd )))
                 0)))))

(def neighbor-delta {0 [0 -1]
                     1 [1 -1]
                     2 [1 0]
                     3 [1 1]
                     4 [0 1]
                     5 [-1 1]
                     6 [-1 0]
                     7 [-1 -1]})

(defn delta-loc 
  "returns the location one step in the given dir. Note the world is a torus"
  [[x y] [dx dy]]
  [(+ x dx) ( + y dy)])

(defn select-pos
  "algorithm for selecting location of a new microbe to be born"
  [curr-pos]
  (loop [x (delta-loc curr-pos (neighbor-delta (rand-int 8)))]
    (if (bound? x)
      x
      (recur (delta-loc curr-pos (neighbor-delta (rand-int 8)))))))    

(defn live [{id       :id 
             pos      :pos 
             health   :health 
             metab    :metabolism
             children :children 
             age      :age}]
  ;; (println "id = " id "location = " loc "health = " health)
  (dosync 
   (let [loc       (place pos)
         cell-life (:life @loc)]
     (cond 
      (> health (:split metab)) (let [mic-life (quot health 2)
                                      new-id   (create-microbe (select-pos pos) mic-life metab)
                                      life     (let [left (- cell-life mic-life)]
                                                 (if (pos? left) left 0))]
                                  (alter loc assoc :life life)
                                  (send-off (@microbes-alive new-id) live)
                                  (when running
                                    (send-off *agent* live))
                                  (. Thread (sleep microbe-sleep-ms))
                                  (struct microbe 
                                          id
                                          pos
                                          mic-life
                                          metab
                                          (inc children) 
                                          (inc age)))
      (> health 0) (let [portion (eat pos)
                         life    (- (+ cell-life portion) (:rate metab))]
                     (alter loc assoc :life life)
                     (when running
                       (send-off *agent* live))
                     (. Thread (sleep microbe-sleep-ms))
                     (struct microbe
                             id
                             pos 
                             (- (+ health portion) (:rate metab)) 
                             metab
                             children 
                             (inc age)))
      :else (let [mic-count (:mic-count @loc)]
              (alter loc assoc :mic-count (dec mic-count))
              (alter microbes-dead  conj   (@microbes-alive id))
              (alter microbes-alive dissoc id)
              (struct microbe id pos 0 metab children age))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 

;; animation
(def microbe-scale  (* microbe-life 2))
(def nutrient-scale (/ 255 max-nutrients))

(defn fill-cell [#^Graphics g x y c]
  (doto g
    (.setColor c)
    (.fillRect (* x scale) (* y scale) scale scale)))

(defn render-place [g p x y]
  (when (and render-nutrients? (pos? (:nutrient p)))
    (fill-cell g x y (new Color 0 255 0 
                          ;; (if (pos? (:nutrientc p)) 0 255)
                          (int (min 255 (* 255 (/ (:nutrient p) max-nutrients))))
                          )))
  (when (and render-microbes? (pos? (:life p)))
    (fill-cell g x y (new Color 255 0 0
                          (int (min 255 (* 255 (/ (:life p) microbe-scale))))
                          ;; (if (pos? (:mic-count p)) 255 0)
                          ))))

(defn render [g]
  (let [v (dosync (apply vector (for [x (range dim) y (range dim)] 
                                   @(place [x y]))))
        img (new BufferedImage (* scale dim) (* scale dim) 
                 (. BufferedImage TYPE_INT_ARGB))
        bg (. img (getGraphics))]
    (doto bg
      (.setColor (. Color white))
      (.fillRect 0 0 (. img (getWidth)) (. img (getHeight))))
    (dorun 
     (for [x (range dim) y (range dim)]
       (render-place bg (v (+ (* x dim) y)) x y)))
    ;; (doto bg
    ;;   (.setColor (. Color blue))
    ;;   (.drawRect (* scale home-off) (* scale home-off) 
    ;;              (* scale nants-sqrt) (* scale nants-sqrt)))
    (. g (drawImage img 0 0 nil))
    (. bg (dispose))))
 
(def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                     (* scale dim) 
                                     (* scale dim)))))
(def frame (doto (new JFrame) (.add panel) .pack .show))

(def animator (agent 0))

(defn animation [age]
  (when running
    (send-off *agent* #'animation))
  (. panel (repaint))
  (. Thread (sleep animation-sleep-ms))
  (inc age))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; use ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reset-world [& {:keys [nutrients dimension] 
                      :or   {nutrients 10 
                             dimension 20}}]
  (dosync 
   (def world (mapv (fn [_] 
                      (mapv (fn [_] (ref (struct cell 0 0 nutrients))) 
                            (range dimension))) 
                    (range dimension)))))

(defn start-animation [] 
  (def animator (agent 0))
  (def panel (doto (proxy [JPanel] []
                        (paint [g] (render g)))
             (.setPreferredSize (new Dimension 
                                     (* scale dim) 
                                     (* scale dim)))))
  (def frame (doto (new JFrame) (.add panel) .pack .show))
  (send-off animator animation))

(defn set-nutrients [val]
  (dosync 
   (dorun 
    (for [x (range dim)
          y (range dim)]
      (let [p (place [x y])] 
        (alter p assoc :nutrient val))))))

(defn seed-microbe [n]
  (for [m (range n)]
    (let [x (rand-int dim)
          y (rand-int dim)]
      (create-microbe [x y] microbe-life microbe-metabolism))))

(def supplier (agent 0))

(defn supply []
  (dorun 
   (for [x (range dim) 
         y (range dim)]
     (dosync 
      (let [p (place [x y])
            n (:nutrient @p)]
        (when (< n max-nutrients)
          (alter p assoc :nutrient (+ n supply-delta))))))))

(defn nutrient-supply [x]
  (when running 
    (send-off *agent* nutrient-supply))
  (supply)
  (. Thread (sleep supply-sleep-ms))
  (inc x))



(defn run [] 
  (dosync 
   (reset-world)
   (def running true)
   (def microbes-alive (ref {}))
   (def supplier (agent 0))
   (def microbes (seed-microbe 1))
   (start-animation)
   (dorun (map (fn [id] (send-off (@microbes-alive id) live)) microbes))
   (send-off supplier nutrient-supply)))

(defn stop []
  (dosync 
   (def running false)))

(defn count-alive []
  (apply + (map (fn [[k v]] 
                  (if (pos? (@v :health)) 1 0)) 
                @microbes-alive)))

(defn count-dead [] 
  (count @microbes-dead))
