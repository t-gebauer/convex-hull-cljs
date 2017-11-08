(ns convex-hull-cljs.core
    (:require-macros [hiccups.core :as hiccups :refer [html]])
    (:require [hiccups.runtime :as hiccupsrt]
              [clojure.string :as str]))

(enable-console-print!)

(defonce state (atom {}))

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

;;;

(defn make-scaler [width height target-width target-height]
  (let [ratio (/ width height)
        target-ratio (/ target-width target-height)
        scale (if (> ratio target-ratio)
                (/ target-width width)
                (/ target-height height))]
    (fn [[x y]] [(* x scale) (* y scale)])))

(defn convex-area [points]
  (->>
    (partition 2 1 points points)
    (map (fn [[[x1 y1] [x2 y2]]] (- (* x1 y2) (* y1 x2))))
    (apply +)
    (* 0.5)))

(def timeout (atom nil))

(defn init-animation [hull-points]
  (let [path-static (.getElementById js/document "hull-static")
        text (.getElementById js/document "area-text")
        total-length (.getTotalLength path-static)
        total-duration 5
        durations (->> hull-points
                       (#(partition 2 1 % %))
                       (map (fn [[p1 p2]] (magnitude (sub p2 p1))))
                       (map #(* total-duration (/ % total-length)))
                       vec)
        colors (atom ["#d81b60"  "#1e88e5"  "#7cb342" "#fb8c00" "#546e7a"])
        ;["#d32f2f" "#c2185b" "#7b1fa2" "#512da8" "#303f9f" "#1976d2""#0288d1")
        color (atom "transparent")
        current (atom 0)
        anim-name (atom "dash")] ;; alternating animation name to restart css-anim
    (defn on-anim-tick [] ;; named function so that we can reference when we .setTimeout
      (let [dur (get durations @current)
            line (.getElementById js/document (str "line" @current))
            circle (.getElementById js/document (str "circle" @current))]
        (when (= @current 0)
          (aset path-static "style" "stroke" @color)
          (swap! colors (fn [v] (take (count v) (rest (cycle v)))))
          (reset! color (first @colors))
          (aset text "style" "fill" @color)
          (swap! anim-name #(if (= % "dash") "dash2" "dash")))
        (aset circle "style" "fill" @color)
        (aset line "style" "stroke" @color)
        (aset line "style" "animation" (str @anim-name " " dur "s linear forwards"))
        (if (< @current (dec (count hull-points)))
          (swap! current inc)
          (reset! current 0))
        (swap! timeout (fn [t]
                         (if t (.clearTimeout js/window t))
                         (.setTimeout js/window on-anim-tick (* 1000 dur))))))
    (on-anim-tick)))

;; generate and display a svg highlighting the hull-points
(defn render-svg [points width height hull-points]
  (let [root (.getElementById js/document "app")
        hull-area (convex-area hull-points)
        target-width 300
        target-height 300
        border 10
        transf (comp (fn [[x y]] [x (- target-height y)]) ;; y-axis from bottom to top
                     (make-scaler width height target-width target-height))
        points (map transf points)
        hull-points (map transf hull-points)
        path-data (str "M" (str/join " L" (map (fn [[x y]] (str x " " y)) hull-points)) "Z")]
    (set! (.-innerHTML root)
      (html
       [:svg {:viewBox (str (- border) " " (- border) " "
                            (+ target-width border) " "
                            (+ target-height border))
              :preserveAspectRatio "xMidYMid meet"}
        (for [[x y] points]
          [:circle {:cx x :cy y :r 2 :fill "#aaa"}])
        [:path {:id "hull-static" :d path-data :stroke "transparent" :fill "transparent"}]
        (map-indexed (fn [i [p1 p2]]
                       (let [[x1 y1] p1 [x2 y2] p2
                             len (magnitude (sub p2 p1))]
                         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                                 :id (str "line" i)
                                 :style (str "stroke-dasharray:" len
                                             ";stroke-dashoffset:" len)}]))
                     (partition 2 1 hull-points hull-points))
        (map-indexed (fn [i [x y]]
                       [:circle {:cx x :cy y :r 2 :fill "#ffd54f"
                                 :id (str "circle" i)}])
                     hull-points)
        [:text {:id "area-text"
                :x (/ target-width 2) :y (/ target-height 2)
                :fill "#ffd54f" :text-anchor "middle"} hull-area]]))
    (init-animation hull-points)))


;; get all min and max values in one loop
(defn get-bounds [points]
  (let [[x y] (first points)]
    (loop [minx x miny y
           maxx x maxy y
           points (rest points)]
      (if (empty? points)
        [minx miny maxx maxy]
        (let [[x y] (first points)]
          (recur (min minx x) (min miny y)
                 (max maxx x) (max maxy y)
                 (rest points)))))))

;; find all points in the hull
(defn hull-points [points]
  ; start with a helper point which is definitly outside the hull
  (loop [prev [0 1]
         current [0 0]
         hull-points []]
    (let [[_ next] (->> points
                        (filter #(not= current %))
                        (map (fn [point]
                               [(angle (sub prev current)
                                       (sub point current))
                                point]))
                        (sort (comp - compare))
                        first)]
      (if (= next (first hull-points))
        hull-points
        (recur current next (conj hull-points next))))))

(defn on-text-read [text]
  (let [words (str/split text #"\s+")
        numbers (map js/parseInt words)
        pairs (partition 2 numbers)
        [minx miny maxx maxy] (get-bounds pairs)
        width (- maxx minx)
        height (- maxy miny)
        translated (map (fn [[x y]] [(- x minx) (- y miny)]) pairs)
        hull-points (hull-points translated)]
   (render-svg translated width height hull-points)))

(defn handle-drag-over [evt]
  (.stopPropagation evt)
  (.preventDefault evt)
  (aset evt "dataTransfer" "dropEffect" "copy"))

(defn handle-drop [evt]
  (.stopPropagation evt)
  (.preventDefault evt)
  (let [files (.. evt -dataTransfer -files)
        reader (js/FileReader.)]
    (aset reader "onload" #(on-text-read (aget % "target" "result")))
    (.readAsText reader (aget files 0))))

;; called in devel by figwheel
(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)

;; only add event listeners once (only relevant in devel)
(defonce init
  (let [root (.getElementById js/document "app")]
    (.addEventListener root "dragover" handle-drag-over)
    (.addEventListener root "drop" handle-drop)
    (set! (.-innerHTML root)
          (html [:div.center-container [:p "drag a file here!"]]))))
