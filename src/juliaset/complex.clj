(ns juliaset.complex)

(deftype Complex [^double real ^double imag])

(defn plus [^Complex z1 ^Complex z2]
  (let [x1 (double (.real z1))
        y1 (double (.imag z1))
        x2 (double (.real z2))
        y2 (double (.imag z2))]
    (Complex. (+ x1 x2) (+ y1 y2))))

(defn times [^Complex z1 ^Complex z2]
  (let [x1 (double (.real z1))
        y1 (double (.imag z1))
        x2 (double (.real z2))
        y2 (double (.imag z2))] 
    (Complex. (- (* x1 x2) (* y1 y2)) (+ (* x1 y2) (* y1 x2)))))

(defn norm [^Complex z]
  (let [x (double (.real z))
        y (double (.imag z))]
    (+ (* x x) (* y y))))
