(ns juliaset.core
  (:require [juliaset.complex :as C])
  (:import juliaset.complex.Complex)
  (:import java.awt.image.BufferedImage)
  (:import javax.imageio.ImageIO)
  (:import java.awt.Color))


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

(defn pmap-grid [f xmin xmax nx
                 ymin ymax ny]
  (let [grd (grid xmin xmax nx
                ymin ymax ny)]
    (doall (pmap f grd))))


(defn to-cmap [min max]
  ())

(defn iters [n px py]
  (let [escaped? (partial iter-while< 1E10 rabbit n)]
    (vec (pmap-grid escaped? -1 1 px -1 1 py))))

(defn red-black [n itr]
  (let [i (* 100 (/ itr n))]
    (cond
      (< i 20) (Color. 218 5 38)
      (< i 40) (Color. 152 0 25)
      (< i 80) (Color. 87 0 14)
      :else Color/black)))

(defn -main
  [n px py]
  (let [img (BufferedImage. px py BufferedImage/TYPE_INT_RGB)
        gfx (.createGraphics img)
        its (iters n px py)]
    (doseq [x (range px) y (range py)]
      (let [it (get its (+ (* x px) y))
            col (red-black n it)]
        (.setColor gfx col)
        (.fillRect gfx x y 1 1)))
  (ImageIO/write img "PNG" (clojure.java.io/file "blah.png"))))
