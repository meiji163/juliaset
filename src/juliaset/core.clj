(ns juliaset.core
  (:require [juliaset.complex :as C])
  (:import juliaset.complex.Complex))

(defn iter [f n ^Complex z]
  (if (> n 0)
    (recur f (- n 1) (f z))
    z))

(defn iter-while< [thresh f n ^Complex z]
  (letfn [(iter [z i]
            (if (or 
                 (>= i n) 
                 (> (max (.real z) (.imag z)) thresh))
              i
              (recur (f z) (+ 1 i))))]
    (iter z 0)))

(defn scc [^Complex z]
  (let [c (Complex. 0.99 0.14)] 
    (C/times z (C/plus z c))))

(defn rabbit [^Complex z]
  (let [c (Complex. -0.122 0.745)]
    (C/plus (C/times z z) c)))

(defn grid [xmin xmax nx 
            ymin ymax ny]
  (let [delx (/ (- xmax xmin) nx)
        dely (/ (- ymax ymin) ny)
        xs (range xmin xmax delx)
        ys (range ymin ymax dely)]
    (for [x xs y ys] (Complex. (double x) (double y)))))

(defn map-grid [f xmin xmax nx 
                ymin ymax ny]
  (let [delx (/ (- xmax xmin) nx)
        dely (/ (- ymax ymin) ny)
        xs (range xmin xmax delx)
        ys (range ymin ymax dely)]
    (for [x xs y ys]
      (f (Complex. (double x) (double y))))))

(defn to-cmap [min max]
  ())

(defn init-frame [px py]
  (def frame (java.awt.Frame.))
  (do
    (.setSize frame (java.awt.Dimension. px py))
    (.setVisible frame true)
    frame))

(defn -main
  [& args]
  (let [n 20
        px 500
        py 500
        escaped? (comp
                  #(if (= n %) 0 255)
                  (partial iter-while< 1E20 rabbit n))
        grays (vec (map-grid escaped? 0 1 px 0 1 py))
        frame (init-frame px py)]
    (for [x (range px) y (range py)] 
      (let [gfx (.getGraphics frame)
            col (get grays (+ (* px x) y))]
        (do (.setColor gfx (java.awt.Color. col col col)) 
            (.fillRect gfx x y 1 1))))))
