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

(defn iter-while<
  "iterate func f for n steps, or until |z|>thresh.
  Returns the number of iterations"
  [thresh f n ^Complex z]
  (letfn [(iter [z i]
            (if (or
                 (>= i n)
                 (> (max (.real z) (.imag z)) thresh))
              i
              (recur (f z) (+ 1 i))))]
    (iter z 0)))

(defn map-grid
  "map complex func f over grid"
  [f xmin xmax nx
   ymin ymax ny]
  (let [delx (/ (- xmax xmin) nx)
        dely (/ (- ymax ymin) ny)
        xs (range xmin xmax delx)
        ys (range ymin ymax dely)]
    (for [x xs y ys]
      (f (Complex. (double x) (double y))))))

(def grid (partial map-grid identity))

(defn pmap-grid [f xmin xmax nx
                 ymin ymax ny]
  (let [grd (grid xmin xmax nx
                ymin ymax ny)]
    (doall (pmap f grd))))


;;;; some polynomial functions ;;;;;
(defn scc [^Complex z]
  (let [c (Complex. 0.99 0.14)]
    (C/times z (C/plus z c))))

(defn dragonfly
  "z^5 + (0.8+0.8i)z^3 + z"
  [^Complex z]
  (let [c (Complex. 0.8 0.8)
        one (Complex. 1.0 0.0)
        z3 (C/times z (C/times z z))]
    (C/times z (C/plus one
                       (C/times z3 (C/plus z c))))))

(defn z2-c [^Complex c ^Complex z]
  (C/plus (C/times z z) c))

(def spiral (partial z2-c (Complex. -0.744336 0.121198)))

(def rabbit (partial z2-c (Complex. -0.122 0.745)))

(def airplane (partial z2-c (Complex. -1.0 0.0)))

;;;; color gradients ;;;;
(defn red-black [n itr]
  (let [i (* 100 (/ itr n))]
    (cond
      (< i 20) (Color. 220 5 38)
      (< i 50) (Color. 152 0 25)
      (< i 99) (Color. 87 0 14)
      :else Color/black)))

(defn iters [n px py]
  (let [escaped? (partial iter-while< 1E9 rabbit n)]
    (vec (pmap-grid escaped? -1 1 px -1 1 py))))

(defn -main
  [n px py]
  (let [img (BufferedImage. px py BufferedImage/TYPE_INT_RGB)
        gfx (.createGraphics img)
        its (iters n px py)]
    (doseq [x (range px) y (range py)]
      (let [it (get its (+ (* x py) y))
            col (red-black n it)]
        (.setColor gfx col)
        (.fillRect gfx x y 1 1)))
  (ImageIO/write img "PNG" (clojure.java.io/file "blah.png"))))
