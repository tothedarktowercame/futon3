(ns scripts.build-sigils-density
  "Render a Tokipona × truth-table density map for the devmap embeddings."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.awt.image BufferedImage]
           [java.awt RenderingHints]
           [javax.imageio ImageIO]))

(System/setProperty "java.awt.headless" "true")

(def ^:private baseline 40)

(def ^:private overlay-palette
  {baseline [40 40 40]
   210 [120 120 120]
   230 [200 140 60]
   245 [255 190 90]
   255 [255 230 160]})

(def ^:private overlay-multi-palette
  {baseline [40 40 40]
   210 [80 140 220]
   230 [60 200 255]
   245 [170 240 255]
   255 [255 255 255]})

(def ^:private truth-normalize
  {"門" "门"
   "義" "义"})

(defn- read-index []
  (edn/read-string (slurp "resources/sigils/index.edn")))

(defn- read-tokipona-emoji []
  (->> (str/split-lines (slurp "holes/tokipona.org"))
       (keep (fn [line]
               (when (and (str/starts-with? line "|")
                          (not (str/starts-with? line "|---")))
                 (let [cols (->> (str/split line #"\|")
                                 (map str/trim)
                                 (remove str/blank?))]
                   (when (>= (count cols) 2)
                     [(str/lower-case (second cols)) (first cols)])))))
       (into {})))

(defn- load-patterns []
  (->> (str/split-lines (slurp "resources/sigils/patterns-index.tsv"))
       (remove #(or (str/blank? %) (str/starts-with? % "#")))
       (map #(str/split % #"\t"))
       (keep (fn [[_ tokipona truth & _]]
               (when (and tokipona truth)
                 {:tokipona (-> tokipona (str/split #"\(") first str/trim)
                  :truth (-> truth (str/split #"\s" 2) first)})))))

(defn- tokipona->emoji [m section]
  (some-> section
          str/lower-case
          (str/split #"\s+")
          first
          m))

(defn- truth->char [c]
  (truth-normalize (str c) (str c)))

(defn- build-counts []
  (let [{:keys [emoji hanzi]} (read-index)
        emoji-pos (zipmap emoji (range))
        hanzi-pos (zipmap hanzi (range))
        tok->emoji (read-tokipona-emoji)
        counts (vec (repeatedly (count hanzi) #(int-array (count emoji))))]
    (doseq [{:keys [tokipona truth]} (load-patterns)]
      (when-let [emoji-char (tokipona->emoji tok->emoji tokipona)]
        (when-let [x (get emoji-pos emoji-char)]
          (when-let [y (get hanzi-pos (truth->char truth))]
            (let [row (counts y)]
              (aset-int row x (inc (aget row x))))))))
    {:counts counts
     :width (count hanzi)
     :height (count emoji)}))

(defn- intensity [value]
  (cond
    (<= value 0) baseline
    (= value 1) 210
    (= value 2) 230
    (<= value 4) 245
    :else 255))

(defn- write-grayscale [{:keys [counts width height]} path]
  (let [cell-size 12
        img (BufferedImage. (* cell-size width) (* cell-size height) BufferedImage/TYPE_BYTE_GRAY)
        raster (.getRaster img)]
    (doseq [y (range height)
            x (range width)]
      (let [val (intensity (aget ^ints (counts x) y))]
        (doseq [dy (range cell-size)
                dx (range cell-size)]
          (.setSample raster (+ (* x cell-size) dx)
                         (+ (* y cell-size) dy)
                         0
                         val))))
    (ImageIO/write img "png" (io/file path))
    img))

(defn- scale-image [^BufferedImage img factor type]
  (let [w (* factor (.getWidth img))
        h (* factor (.getHeight img))
        scaled (BufferedImage. w h type)
        g (.createGraphics scaled)]
    (.setRenderingHint g RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    (.drawImage g img 0 0 w h nil)
    (.dispose g)
    scaled))

(defn- rgb [[r g b]]
  (bit-or (bit-shift-left (int r) 16)
          (bit-shift-left (int g) 8)
          (int b)))

(defn- write-color [{:keys [counts width height]} palette path]
  (let [cell-size 12
        img (BufferedImage. (* cell-size width) (* cell-size height) BufferedImage/TYPE_INT_RGB)]
    (doseq [y (range height)
            x (range width)]
      (let [val (intensity (aget ^ints (counts x) y))
            color (get palette val [val val val])]
        (doseq [dy (range cell-size)
                dx (range cell-size)]
          (.setRGB img (+ (* x cell-size) dx)
                       (+ (* y cell-size) dy)
                       (rgb color)))))
    (ImageIO/write img "png" (io/file path))
    img))

(defn- write-variants [img base-path]
  (doseq [factor [2 5]]
    (ImageIO/write (scale-image img factor (.getType img)) "png"
                   (io/file (format "%s@%dx.png" base-path factor)))))

(defn -main [& _]
  (let [data (build-counts)
        base (write-grayscale data "resources/sigils/devmap-density.png")
        overlay (write-color data overlay-palette "resources/sigils/devmap-density-overlay.png")
        overlay-multi (write-color data overlay-multi-palette "resources/sigils/devmap-density-overlay-multi.png")]
    (write-variants base "resources/sigils/devmap-density")
    (write-variants overlay "resources/sigils/devmap-density-overlay")
    (write-variants overlay-multi "resources/sigils/devmap-density-overlay-multi")
    (println "Wrote density maps with size" (.getWidth base) "x" (.getHeight base))))
