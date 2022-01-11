(use 'clojure.java.io)

(defn readParseInput [fn]
  (with-open [rdr (reader fn)]
    (let [algorithm (.readLine rdr)]
      (.readLine rdr)
      (let [image (clojure.string/split-lines (slurp rdr))]
        [algorithm, image]))))

(defn print-image [image]
  (doseq [row image]
    (println row)))

(defn triplets [l]
  (let [triplet (take 3 l)]
    (if (< (count l) 3) () (conj (triplets (next l)) triplet))))

(defn wrapped [infinite-char image]
  (let [width (count (first image))
        extra-row (list (repeat (+ width 4) infinite-char))
        wrapped-row (fn [row] (lazy-cat (list infinite-char infinite-char) row (list infinite-char infinite-char)))]
    (lazy-cat extra-row extra-row (map wrapped-row image) extra-row extra-row)))

(defn pix-to-int [ch]
  ({\. 0, \# 1} ch))

(defn pix-seq-to-int-seq [s]
  (map pix-to-int s))

(defn s-to-int [s]
  (reduce +
    (map *
      (map (fn [n] (bit-shift-left 1 n)) (reverse (range (count s))))
      (pix-seq-to-int-seq s))))

(defn algorithm-lookup-by-seq [algo s]
  (nth algo (s-to-int s)))

(defn algorithm-lookup-by-grid [algo grid]
  (let [[row-above row-cur row-below] grid]
    (algorithm-lookup-by-seq algo (lazy-cat row-above row-cur row-below))))

(defn zip3 [a b c]
  (partition 3 (interleave a b c)))

(defn next-image-row [algorithm row-triplet]
  (let [[row-above row-cur row-below] row-triplet
        triplets-above (triplets row-above)
        triplets-cur (triplets row-cur)
        triplets-below (triplets row-below)
        triplets (zip3 triplets-above triplets-cur triplets-below)]
    (map (partial algorithm-lookup-by-grid algorithm) triplets)))

(defn next-image [algorithm image infinite-char]
  (let [wrapped-image (wrapped infinite-char image)
        row-triplets (triplets wrapped-image)]
    (map (partial next-image-row algorithm) row-triplets)))

(defn lit-pixel-count-row [row]
  (reduce + (pix-seq-to-int-seq row)))

(defn lit-pixel-count [image]
  (reduce + (map lit-pixel-count-row image)))

(defn iters [algorithm image infinite-char count]
  (if (= count 0)
    image
    (let [image' (next-image algorithm image infinite-char)
          infinite-char' (algorithm-lookup-by-seq algorithm (repeat 9 infinite-char))
          count' (- count 1)]
      (iters algorithm image' infinite-char' count'))))

(def input (readParseInput "../input/day20.txt"))
(def algorithm (first input))
(def image (second input))
(def infinite-char \.)

(def image-final (iters algorithm image infinite-char 50))
(println (lit-pixel-count image-final))
