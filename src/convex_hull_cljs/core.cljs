(ns convex-hull-cljs.core
    (:require-macros [hiccups.core :as hiccups :refer [html]])
    (:require [hiccups.runtime :as hiccupsrt]
              [clojure.string :as str]
              [convex-hull-cljs.vector :as vec]))

(enable-console-print!)

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
        total-duration 4
        durations (->> hull-points
                       (#(partition 2 1 % %))
                       (map (fn [[p1 p2]] (vec/magnitude (vec/sub p2 p1))))
                       (map #(* total-duration (/ % total-length)))
                       vec)
        colors (atom ["#d81b60"  "#1e88e5"  "#7cb342" "#fb8c00" "#546e7a"])
        color (atom "transparent") ;; default: invisible path
        current (atom 0)
        anim-name (atom "dash")] ;; alternating animation name to restart css-anim
    (aset text "style" "transition" (str "fill " (* (/ total-duration 5) 3)
                                         "s ease " (/ total-duration 5) "s"))
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

;; generates a function to scale points
(defn make-scale [width height target-width target-height]
  (let [ratio (/ width height)
        target-ratio (/ target-width target-height)
        scale (if (> ratio target-ratio)
                (/ target-width width)
                (/ target-height height))]
    (fn [[x y]] [(* x scale) (* y scale)])))

;; generates a function which centers points
(defn make-center [width height]
  (let [ratio (/ width height)]
    (if (= ratio 1) (fn [e] e)
      (if (< ratio 1)
        (let [dx (- (/ height 2) (/ width 2))]
          (fn [[x y]] [(+ x dx) y]))
        (let [dy (- (/ width 2) (/ height 2))]
          (fn [[x y]] [x (+ y dy)]))))))

;; generate and display a svg highlighting the hull-points
(defn render-svg [points width height hull-points]
  (let [root (.getElementById js/document "app")
        hull-area (convex-area hull-points)
        target-width 300
        target-height 300
        border 15
        transf (comp (fn [[x y]] [x (- target-height y)]) ;; y-axis from bottom to top
                     (make-scale width height target-width target-height)
                     (make-center width height))
        points (map transf points)
        hull-points (map transf hull-points)
        path-data (str "M" (str/join " L" (map (fn [[x y]] (str x " " y)) hull-points)) "Z")]
    (set! (.-innerHTML root)
      (html
       [:svg {:viewBox (str (- border) " " (- border) " "
                            (+ target-width (* 2 border)) " "
                            (+ target-height (* 2 border)))
              :preserveAspectRatio "xMidYMid meet"}
        (for [[x y] points]
          [:circle {:cx x :cy y :r 2 :fill "#999"}])
        [:path {:id "hull-static" :d path-data
                :stroke "transparent" :fill "transparent"}]
        (map-indexed (fn [i [p1 p2]]
                       (let [[x1 y1] p1 [x2 y2] p2
                             len (vec/magnitude (vec/sub p2 p1))]
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
                               [(vec/angle (vec/sub prev current)
                                       (vec/sub point current))
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

;; xml request to request text...
(defn xml-request [resource callback]
  (let [xhr (new js/XMLHttpRequest)]
    (aset xhr "responseType" "text")
    (aset xhr "onload"
          (fn [e]
           (if (= (.-readyState xhr) 4)
            (if (= (.-status xhr) 200)
              (callback (.-responseText xhr))
              (.error js/console (.-statusText xhr))))))
    (aset xhr "onerror" #(.error js/console (.-statusText xhr)))
    (.open xhr "GET" resource true)
    (.send xhr nil)))

(defn load-random []
  (xml-request (str "examples/ex" (rand-nth (range 1 9))) on-text-read))

;; only add event listeners once (only relevant in devel)
(defonce init
  (let [root (.getElementById js/document "app")]
    (.addEventListener root "dragover" handle-drag-over)
    (.addEventListener root "drop" handle-drop)
    ;; load random example
    (.addEventListener root "click" load-random)
    (load-random)))
