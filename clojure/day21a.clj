(use 'clojure.java.io)
(require '[clojure.string :as str])

(defn parse-line [l]
  (bigint (nth (str/split l #" ") 4)))

(defn read-parse-input [fn]
  (with-open [rdr (reader fn)]
    (let [p1pos (parse-line (.readLine rdr))
          p2pos (parse-line (.readLine rdr))]
      [p1pos p2pos])))

(def deterministic-dice
  (cycle (range 1 101)))

(defn roll [die]
  [(reduce + (take 3 die)) (drop 3 die)])

(defn game-result [p1-pos p1-score p2-pos p2-score roll-count die]
  (let [[die-roll die'] (roll die)
        roll-count' (+ roll-count 3)
        p1-pos' (+ (mod (- (+ p1-pos die-roll) 1) 10) 1)
        p1-score' (+ p1-score p1-pos')]
    (if (>= p1-score' 1000)
      (* p2-score roll-count')
      (game-result p2-pos p2-score p1-pos' p1-score' roll-count' die'))))

(def input (read-parse-input "../input/day21.txt"))
(def p1-pos (first input))
(def p2-pos (second input))

(println (int (game-result p1-pos 0 p2-pos 0 0 deterministic-dice)))
