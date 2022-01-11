(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn parse-line [l]
  (bigint (nth (str/split l #" ") 4)))

(defn read-parse-input [fn]
  (with-open [rdr (reader fn)]
    (let [p1pos (parse-line (.readLine rdr))
          p2pos (parse-line (.readLine rdr))]
      [p1pos p2pos])))

(def rolls
  (frequencies
    (for [r1 [1 2 3] r2 [1 2 3] r3 [1 2 3]]
      (reduce + [r1 r2 r3]))))

(defn game-result [p1-id p1-pos p1-score p2-id p2-pos p2-score mult]
  (apply (partial merge-with +)
    (for [[die-roll die-mult] rolls]
      (let [p1-pos' (+ (mod (- (+ p1-pos die-roll) 1) 10) 1)
            p1-score' (+ p1-score p1-pos')
            mult' (* mult die-mult)]
        (if (>= p1-score' 21)
          {p1-id mult'}
          (game-result p2-id p2-pos p2-score p1-id p1-pos' p1-score' mult'))))))

(def input (read-parse-input "../input/day21.txt"))
(def p1-pos (first input))
(def p2-pos (second input))

(println (apply max (vals (game-result 1 p1-pos 0 2 p2-pos 0 1))))
