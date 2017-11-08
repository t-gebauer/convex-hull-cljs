(ns convex-hull-cljs.vector)

;;; vector functions

(defn dot [[a b] [c d]]
  (+ (* a c) (* b d)))

(defn sub [[x0 y0] [x1 y1]]
  [(- x0 x1) (- y0 y1)])

(defn magnitude [[x y]]
  (js/Math.sqrt (+ (* x x) (* y y))))

(defn norm [v]
  (let [m (magnitude v)
        [x y] v]
    [(/ x m) (/ y m)]))

(defn angle [v w]
  (let [dot (dot (norm v) (norm w))]
    (if (> dot 1) 0 (js/Math.acos dot)))) ; in case of js/Math rounding error
