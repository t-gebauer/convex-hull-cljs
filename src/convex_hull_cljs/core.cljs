(ns convex-hull-cljs.core
    (:require-macros [hiccups.core :as hiccups :refer [html]])
    (:require [hiccups.runtime :as hiccupsrt]
              [clojure.string :as str]))

(enable-console-print!)

(defonce state (atom {}))

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
        hull-points (map transf hull-points)]
    (set! (.-innerHTML root)
      (html
       [:svg {:viewBox (str (- border) " " (- border) " "
                            (+ target-width border) " "
                            (+ target-height border))
              :preserveAspectRatio "xMidYMid meet"}
        (for [[x y] points]
          [:circle {:cx x :cy y :r 2 :fill "#fff"}])
        [:path {:d (str "M" (str/join " L" (map (fn [[x y]] (str x " " y)) hull-points)) "Z")
                :stroke "#f00" :fill "transparent"}]
        (map-indexed (fn [i [x y]]
                       [:circle {:cx x :cy y :r 2 :fill "#f00"
                                 :id (str "circle" i)}])
                     hull-points)
        [:text {:x (/ target-width 2) :y (/ target-height 2)
                :fill "#f00" :text-anchor "middle"} hull-area]]))))

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

(defonce current-point (atom 0))

(defn get-current-circle []
  (.getElementById js/document (str "circle" @current-point)))

(defn on-timer []
  (if-let [point (get-current-circle)]
    (.remove (.-classList point ) "highlight"))
  (swap! current-point inc)
  (if-let [point (get-current-circle)]
    (.add (.-classList point) "highlight")
    (do
      (reset! current-point 0)
      (if-let [point (get-current-circle)]
        (.add (.-classList point) "highlight")))))

;; only add event listeners once (only relevant in devel)
(defonce init
  (let [root (.getElementById js/document "app")]
    (.addEventListener root "dragover" handle-drag-over)
    (.addEventListener root "drop" handle-drop)
    (set! (.-innerHTML root)
          (html [:div.center-container [:p "drag a file here!"]]))
    (.setInterval js/window #(on-timer) 300)))
