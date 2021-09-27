(ns jam20.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [debux.cs.core :as dbg :refer-macros [dbg  dbgn
                                                  dbg_ dbgn_]]))

(def k (* 5e3 6))
(def s 1.5)
(def k-ship 2e5)
(def b-ship 1.5)
(def d-ship 8000)

(def FUEL_MAX 400)
(def FUEL_MIN 3)
(def FUEL_DEC 2)
(def FUEL_BURN 30)
(def FUEL_F 4e3)
(def PLANET_N 3)
(def PLANET_SIZE [32 128])
(def PLANET_SPREAD 64)
(def CELL_SIZE [6 9])
(def CELL_DELAY [5 10])
(def CELL_DENS_DIV 32)
(def CELL_GAIN 12)
(def WARP_FR .4)

(defn sub [a b] [(- (first a) (first b)) (- (second a) (second b))])
(defn add [a b] [(+ (first a) (first b)) (+ (second a) (second b))])
(defn mul [v s] (map #(* % s) v))
(defn magsq [v] (->> v (map #(* % %)) (reduce +)))
(defn mag   [v] (-> v magsq Math/sqrt))
(defn norm  [v] (map #(* % (/ 1 (mag v))) v))
(defn fmod [v s] [(mod (first v) s) (mod (second v) s)])
(defn   lt [v s] (reduce #(or %1 %2) (map #(< % s) v)))
(defn   gt [v s] (reduce #(or %1 %2) (map #(> % s) v)))

(defn avg [v] (/ (reduce + v) (count v)))
(defn rdir [] (let [a (q/random 0 (* 2 3.14))] [ (q/sin a) (q/cos a) ]))
(defn rrange [r] (q/random (first r) (second r)))
(defn circle-area [r] (* 3.14 .25 r r))

(def width 800)
(def dt (/ 1 60.))

(defn inv-y [v] [ (first v) (- width (second v)) ])
(defn mouse-pos [] (inv-y [ (q/mouse-x) (q/mouse-y) ]))

(defn zone-mk [pos]
      { :pos pos
        :size (rrange PLANET_SIZE)
        :cells { :data '()
                 :t (rrange CELL_DELAY) } })
(defn zone-tick [z]
      { :pos (:pos z)
        :size (:size z)
        :cells (let [t (- (:t (:cells z)) dt)
                     data (:data (:cells z))]
                 (if (and (< t 0) (< (count data)
                                     (/ (circle-area (:size z))
                                        (* CELL_DENS_DIV (circle-area (avg CELL_SIZE))))))
                   { :data (conj data { :pos (mul
                                               (rdir)
                                               (* (q/random 1) (:size z) .5))
                                        :r (rrange CELL_SIZE) })
                     :t (rrange CELL_DELAY) }
                   { :data data
                     :t t })) })
(defn zone-render [z]
      (let [pos (inv-y (:pos z))]
        (q/ellipse
          (first pos) (second pos)
          (:size z) (:size z))))
(defn cell-render [c origin]
      (let [pos (inv-y (add (:pos c) origin))]
        (q/ellipse
          (first pos) (second pos)
          (:r c) (:r c))))

(defn scale [i n]
      (+ (rrange (mul [-1 1] PLANET_SPREAD))
        (+ (* width .5) (* (* width .5) (- (/ i (- n 1)) .5)))))
(defn zone-locs [n]
      (map
        (fn [ij] [ (scale (first ij) n) (scale (second ij) n) ])
        (mapcat (fn [i] (map (fn [j] [i j]) (range 0 n))) (range 0 n))))
(defn mk-zones []
      (map zone-mk (zone-locs PLANET_N)))
(defn render-zones [zs]
      (q/stroke-weight 3)
      (q/stroke 30)
      (q/fill 100)
      (doseq [z zs] (zone-render z)))
(defn render-cells [zs]
      (q/stroke-weight 1)
      (q/stroke 55 60 60)
      (q/fill 55 10 85)
      (doseq [z zs]
             (doseq [c (:data (:cells z))]
                    (cell-render c (:pos z)))))

(defn zone-attract [z pos mass]
      (let [diff (sub (:pos z) pos)
            r (mag diff)]
        (if (< r (* .5 (:size z)))
          [0 0]
          (let [inv (/ 1 (* r s))
                dir (mul diff (/ 1 r))]
            (mul dir (* k mass (:size z) inv inv))))))
(defn zone-forces [zs pos mass]
      (->> zs (map #(zone-attract % pos mass)) (reduce add)))

(defn mass-fn [m]
      (let [exp (Math/pow b-ship m)]
        (/ exp (+ d-ship exp))))
(defn ship-attract [ship pos mass]
      (let [diff (sub (:pos ship) pos)
            r (mag diff)]
        (if (< r (+ (* .5 mass) (* .5 (:size ship))))
          [0 0]
          (let [mm (* mass (:size ship))
                inv (/ 1 (* r s))
                dir (mul diff (/ 1 r))]
            (mul dir (* k-ship mm inv inv))))))
(defn ship-forces [ships pos mass]
      (->> ships (map #(ship-attract % pos mass)) (reduce add)))

(defn ship-mk [col]
      { :vel [(q/random -128 128) (q/random -128 128)]
        :pos [(q/random 32 (- width 32))
              (q/random 32 (- width 32))]
        :ang 0
        :size 8
        :fuel 100
        :col col
        :trail { :t (q/millis)
                 :q '() } })
(defn ship-tick [p as zs inp len]
      (let [grav (add
                   (zone-forces zs (:pos p) (:size p))
                   (ship-forces as (:pos p) (:size p)))
            fuel (Math/max 0 (- (:fuel p) (+ (* dt inp FUEL_BURN) (* dt FUEL_DEC))))
            dir (norm (sub (mouse-pos) (:pos p)))
            f (add (mul dir (* (if (and inp (>= fuel FUEL_MIN)) 1 0) FUEL_F)) grav)
            packet (let [ v (add (:vel p) (mul f (/ dt (:size p))))
                         dx (mul v dt)
                         pr (add (:pos p) dx)]
                     (if (or (lt pr 0) (gt pr width))
                       (let [nv (mul v (- 1 WARP_FR))
                             pr (add (:pos p) (mul v dt))]
                         { :v nv
                           :x (fmod pr width) })
                       { :v v
                         :x (fmod pr width) }))
            v (:v packet)
            x (:x packet)]
        { :vel [ (first v) (second v) ]
          :pos x
          :ang (+ (q/cos (first dir)) (q/sin (second dir)))
          :size (q/lerp 4 12 (/ fuel 100.))
          :col (:col p)
          :fuel fuel
          :trail (let [last (:t (:trail p))
                       t (q/millis)
                       q (:q (:trail p))]
                 ; (if (> (- t last) 100)
                   (if (> (mag (sub x (:pos (first q)))) 12)
                     { :t t
                       :q (conj q { :pos x :t 1 }) }
                     (if (> (count q) len)
                       { :t t
                         :q (drop-last q) }
                       { :t last
                         :q q }))) }))
(defn player-tick [p as zs]
      (let [inp (and
                  (q/mouse-pressed?)
                  (= (q/mouse-button) :left))]
        (ship-tick p as zs inp 32)))

(defn mk-agents []
      (->> (range 2)
           (map #(ship-mk (q/color (q/random 10 100) 30 100)))))

(defn trail-render [p]
      (if (>= (:fuel p) 2)
        (let [col (q/color
                    (q/hue        (:col p))
                    (q/saturation (:col p))
                    (q/brightness (:col p))
                    75)]
          (q/stroke-weight 4)
          (q/stroke col)
          (doseq [v (:q (:trail p))]
                 (let [pos (inv-y (:pos v))]
                   (q/point
                     (first  pos)
                     (second pos)))))
        (do)))
(defn ship-render [p]
      (q/stroke-weight 2)
      (q/stroke (if (< (:fuel p) 2) (q/color 0 90 50) 30))
      (q/fill   (if (< (:fuel p) 2) (q/color 0 90 100) (:col p)))
      (let [pos (inv-y (:pos p))
            x (- (first  pos) (* (:size p) .5))
            y (- (second pos) (* (:size p) .5))
            s (* (/ 2 8) (/ (:fuel p) 50))]
        (q/rect
          x y
          (:size p) (:size p)
          4 4
          4 4)))

(defn find-near [zs a]
      (flatten
        (map
          (fn [z]
              (filter
                (fn [cell]
                    (let [dist (mag (sub
                                      (:pos a)
                                      (add (:pos cell) (:pos z))))
                          range (* (+ (:r cell) (:size a)) .5)]
                      (< dist range)))
                (:data (:cells z))))
          zs)))

(defn collide [state]
      (let [ manifold
             (->> (cons (:player state) (:agents state))
                  (map (fn [a]
                         (let [near (find-near (:zones state) a)]
                             { :near near
                               :agent
                                 (list
                                   (update a :fuel
                                           (fn [prev]
                                               (Math/min FUEL_MAX
                                                      (+ prev (* (count near) CELL_GAIN)))))) })))
                  (reduce (fn [acc e]
                              { :rmlist (distinct (concat (:rmlist acc) (:near e)))
                                :agents (concat (:agents acc) (:agent e)) })
                          { :rmlist '()
                            :agents '() }))
             zones
             (map (fn [z]
                      (update z :cells
                              (fn [prev]
                                  (update prev :data
                                          (fn [cells]
                                              (remove
                                                (fn [cell]
                                                    (some #{cell}
                                                          (:rmlist manifold)))
                                                cells))))))
                  (:zones state))]
        { :zones zones
          :agents (rest  (:agents manifold))
          :player (first (:agents manifold)) }))

(defn init []
      (q/smooth 2)
      (q/frame-rate 60)
      (q/color-mode :hsb 100)
      (q/cursor :cross)
      { :zones  (mk-zones)
        :agents (mk-agents)
        :player (ship-mk (q/color 40 75 90)) })

(defn tick [state]
      (let [post (collide state)]
        { :zones  (map #(zone-tick %) (:zones post))
          :agents (map #(ship-tick
                          % (cons (:player post)
                                  (->> (:agents post)
                                       (filter
                                         (fn [a] (not (= a %))))))
                          (:zones post)
                          false 12)
                       (:agents post))
          :player (player-tick (:player post) (:agents post) (:zones post)) }))

(defn debug-attract [zs p]
      (q/stroke-weight 3)
      (doseq [z zs]
             (let [f (zone-attract z (:pos p) (:size p))
                   src (inv-y (:pos z))
                   dst (inv-y (add
                                (:pos z)
                                (mul (norm f)
                                     (* -1 (- (* .5 (:size z)) 3)))))]
               (if (< (mag f) 1)
                 (do)
                 (do
                   (q/stroke-weight f)
                 ; (q/stroke 0 (mag f) 100)
                   (q/stroke (Math/max 50 (- 100 (* (mag f) .2))))
                   (q/line src dst))))))
(defn debug-mouse [p]
      (if (and
            (q/mouse-pressed?)
            (= (q/mouse-button) :left)
            (>= (:fuel p) FUEL_MIN))
        (let [dir (norm (sub (:pos p) (mouse-pos)))
              src (inv-y (:pos p))
              dst (inv-y (add (:pos p)
                            ; (mul dir (* 32 .02 (:fuel p)))))]
                              (mul dir 32)))]
          (q/stroke-weight 3)
          (q/stroke 50 100 100)
          (q/line src dst)))
        (do))
(defn render [state]
      (q/background 50)
      (doseq [a (:agents state)] (trail-render a))
      (trail-render (:player state))
      (render-zones (:zones state))
      (debug-attract (:zones state) (:player state))
      (render-cells (:zones state))
      (doseq [a (:agents state)] (ship-render a))
      (debug-mouse (:player state))
      (ship-render (:player state)))

(defn ^:export run-sketch []
      (q/defsketch jam20
                   :size [width width]
                   :update tick
                   :draw render
                   :setup init
                   :host "jam20"
                   :middleware [m/fun-mode]))
