(ns chess.core
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clojure.string :as str])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Load user-ranks map and last game timestamp from users.edn.
;; Load games for each user from chess.com and filter for games having "end_time" timstamp higher than last game timestamp.
;; Add games to a single map using the unique "url" as key. This eliminates double counting.
;; Filter games to make sure that both players are in our ladder.
;; Sort games by "end_time" asc.
;; Get the last "end_time" from games and update users.edn last game timestamp with this.
;; Intialize
;; Go through games, 1 by 1, for each game,
;; do
;; get white, black. get score for each from game, rankings from user-ranks map and calculate updated rankings.
;; Update the new rankings in user-ranks map for each user.
;; end
;; write to users.edn new ranks.
;; Forget change of month for now

(defn load-db []
  (first (read-string (slurp "users.edn"))))

(defn write-db [db]
  (spit "users.edn" (clojure.core/pr-str [db])))

; https://metinmediamath.wordpress.com/2013/11/27/how-to-calculate-the-elo-rating-including-example/
(defn calc-rating-elo [r1 r2 s1 s2 k]
  ;(println "calc-rating : (r1, r2, s1, s2, k) = " r1 r2 s1 s2 k)
  (let [R1 (Math/pow 10 (/ r1 400))
        R2 (Math/pow 10 (/ r2 400))
        E1 (/ R1 (+ R1 R2))
        E2 (/ R2 (+ R1 R2))]
    [(Math/round (+ r1 (* k (- s1 E1))))
     (Math/round (+ r2 (* k (- s2 E2))))]))
; (update-rating 2400 2000 1 0 32)

(defn calc-rating-ladder [r1 r2 s1 s2]
  ;(println "calc-rating : (r1, r2, s1, s2, k) = " r1 r2 s1 s2 k)
  (letfn [(new-rank [r1 r2 s]
                    (cond (= s 1) (if (< r1 r2) r1)))]))

(defn update-rank [rold rnew player-rankings]
  (let [player-rankings-swapped (apply hash-map (interleave (map second player-rankings) (map first player-rankings)))]
    ))

(def res-code-to-score-map
  {"win" 1
   "checkmated" 0
   "agreed" 1/2
   "repetition" 1/2
   "timeout" 0
   "resigned" 0
   "stalemate" 1/2
   "lose" 0
   "insufficient" 1/2
   "50move" 1/2
   "timevsinsufficient" 1/2
   "abandoned" 0})

(defn get-new-ranks-for-game-elo [{white "white" black "black" :as game} player-rankings]
  ; (println "get-new-ranks-for-game : " game)
  (let [uname1 (white "username")
        uname2 (black "username")
        r1 (player-rankings uname1)
        r2 (player-rankings uname2)
        s1 (res-code-to-score-map (white "result"))
        s2 (res-code-to-score-map (black "result"))
        k 32
        n-ranks (calc-rating-elo r1 r2 s1 s2 k)]
    [uname1 (first n-ranks) uname2 (second n-ranks)]))

(defn get-new-ranks-for-game-ladder [{white "white" black "black" :as game} player-rankings]
  (let [uname1 (white "username")
        uname2 (black "username")
        r1 (player-rankings uname1)
        r2 (player-rankings uname2)
        s1 (res-code-to-score-map (white "result"))
        s2 (res-code-to-score-map (black "result"))]
    ))

(defn get-games-for-user-month [user-name]
  (let [now (t/now)
        year (str (t/year now))
        month (format "%02d" (t/month now))
        endpoint (str "https://api.chess.com/pub/player/" user-name "/games/" year "/" month)]
    (map #(select-keys % ["white" "black" "end_time" "url"]) ((json/read-str (slurp endpoint)) "games"))))

(defn get-new-games [db]
  (let [player-rankings (:player-rankings db)
        users (set (map (comp str/lower-case first) player-rankings))
        last-timestamp (:last-timestamp db)
        games (mapcat #(get-games-for-user-month %) users)
        games (map #(-> %
                        (update-in ["white" "username"] str/lower-case)
                        (update-in ["black" "username"] str/lower-case))
                   games)
        unique-games (map second
                          (reduce (fn [res game]
                                    (assoc res (game "url") game))
                                  {} games))
        unique-games-within-group (filter #(and (users (str/lower-case (get-in % ["white" "username"])))
                                                (users (str/lower-case (get-in % ["black" "username"]))))
                                          unique-games)
        new-games (filter #(> (% "end_time") last-timestamp) unique-games-within-group)
        new-sorted-games (sort-by #(% "end_time") new-games)]
    new-sorted-games))

(defn calc-new-elo-rankings [db]
  (let [new-games (get-new-games db)
        player-rankings (:player-rankings db)
        last-timestamp (:last-timestamp db)
        new-player-rankings
        (reduce (fn [ranks game]
                  (let [new-ranks (get-new-ranks-for-game-elo game ranks)]
                    (apply (partial assoc ranks) new-ranks)))
                player-rankings new-games)]
    [(if (empty? new-games) last-timestamp ((last new-games) "end_time")) new-player-rankings]))

(defn update-all-elo []
  (let [db (load-db)
        [last-timestamp new-ranks] (calc-new-elo-rankings db)]
    (write-db {:last-timestamp last-timestamp :player-rankings new-ranks})))

(defn show-ranks []
  (let [db (load-db)
        ranks (:player-rankings db)
        sorted-ranks (sort-by second ranks)
        sorted-ranks (reverse sorted-ranks)]
    (doseq [rank (map conj sorted-ranks (range 1 (inc (count sorted-ranks))))]
      (println (nth rank 2) (first rank) (second rank)))))

(defn reset-db [elo?]
  (let [initial-score 1000
        db (load-db)
        user-names (map first (:player-rankings db))
        clean-db {:last-timestamp 0
                  :player-rankings (apply hash-map (interleave user-names 
                                                               (if elo? (repeat (count user-names) initial-score) (range 1 (inc (count user-names))))))}]
    (write-db clean-db)))

(defn print-game [{white "white" black "black"}]
  (println (get white "username") " vs " (get black "username")))

; win	Win
; checkmated	Checkmated
; agreed	Draw agreed
; repetition	Draw by repetition
; timeout	Timeout
; resigned	Resigned
; stalemate	Stalemate
; lose	Lose
; insufficient	Insufficient material
; 50move	Draw by 50-move rule
; abandoned	Abandoned
; kingofthehill	Opponent king reached the hill - No need
; threecheck	Checked for the 3rd time - No need
; timevsinsufficient	Draw by timeout vs insufficient material
; bughousepartnerlose	Bughouse partner lost - No need
