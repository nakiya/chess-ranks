(ns chess.ladder
  (:require [clojure.data.json :as json]
            [clj-time.core :as t]
            [clojure.string :as str]
            [clojure.set :as cset])
  (:gen-class))

(defn load-db []
  (first (read-string (slurp "ladder.edn"))))

(defn write-db [db]
  (spit "ladder.edn" (clojure.core/pr-str [db])))

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

(defn get-games-for-user-month [user-name]
  (let [now (t/now)
        year (str (t/year now))
        month (format "%02d" (t/month now))
        endpoint (str "https://api.chess.com/pub/player/" user-name "/games/" year "/" month)]
    (->> ((json/read-str (slurp endpoint)) "games")
         (map #(select-keys % ["white" "black" "end_time" "url" "pgn"]))
         (map #(update % "pgn" (fn [pgn] 
                                 (let [date-str (re-find (re-matcher #"\[Date \"\d+.\d+\.\d+\"\]" pgn))]
                                   (second (str/split date-str #"\""))))))
          (map #(cset/rename-keys % {"pgn" "date"}))
          (map #(update % "url" (fn [url]
                                  (let [id (str/split url #"https://www.chess.com/live/game/")]
                                    (second id)))))
         (map #(cset/rename-keys % {"url" "id"})))))

(defn get-new-games [db]
  (let [rankings (set (:rankings db))
        last-timestamp (:last-timestamp db)
        games (mapcat #(get-games-for-user-month %) rankings)
        games (map #(-> %
                        (update-in ["white" "username"] str/lower-case)
                        (update-in ["black" "username"] str/lower-case))
                   games)
        unique-games (map second
                          (reduce (fn [res game]
                                    (assoc res (game "id") game))
                                  {} games))
        unique-games-within-group (filter #(and (rankings (str/lower-case (get-in % ["white" "username"])))
                                                (rankings (str/lower-case (get-in % ["black" "username"]))))
                                          unique-games)
        new-games (filter #(> (% "end_time") last-timestamp) unique-games-within-group)
        new-sorted-games (sort-by #(% "end_time") new-games)]
    new-sorted-games))

(defn fix-ranks-as-int [rankings]
  (apply sorted-map (interleave (range (count rankings)) (map second rankings))))

(defn get-win-rank [r1 r2]
  (dec (- r1 (/ (- r1 r2) 2))))

;(get-win-rank 6 3)
;(get-win-rank 15 14)

(defn get-draw-rank [r1 r2]
  (if (= r2 (dec r1))
    r1
    (dec (- r1 (/ (- r1 r2) 4)))))

(defn get-new-ranks [r1 r2 score]
  (let [[nr1 nr2]
        (cond (and (= score 1) (< r2 r1))
              [(get-win-rank r1 r2) r2]
              (and (= score 0) (< r1 r2))
              [r1 (get-win-rank r2 r1)]
              (and (= score 1/2) (< r2 r1))
              [(get-draw-rank r1 r2) r2]
              (and (= score 1/2) (< r1 r2))
              [r1 (get-draw-rank r2 r1)]
              :else
              [r1 r2])]
    (if (= nr1 nr2)
      (if (< r1 r2) [nr1 (+ nr2 0.1)] [(+ nr1 0.1) nr2])
      [nr1 nr2])))

;; (get-new-ranks 5 10 0)
;; (get-new-ranks 10 5 1)
;; (get-new-ranks 5 6 1/2)
;; (get-new-ranks 5 7 1/2)

(defn get-rank-from-user [rankings user]
  (first (for [[k v] rankings :when (= v user)] k)))

(defn print-rank-change [p1 p2 r1 r2 nr1 nr2 res date]
  (println p1 res "against " p2 " on " date
           (if (not= r1 nr1)
             (str ", " p1 " rank changed from " r1 " to " nr1) "")
           (if (= res "drew") 
             (if (not= r2 nr2)
               (str ", " p2 " rank changed from " r2 " to " nr2) "") "")
           ))

(defn update-player-positions [r1 r2 p1 p2 score rankings game] ; rankings is a sorted map of player positions
  (let [;_ (println "update-player-positions : (r1, r2, p1, p2) = " r1 r2 p1 p2)
        ;_ (println "rankings = " rankings)
        [newr1 newr2] (get-new-ranks r1 r2 score)
        ;_ (println "get-new-ranks: u1, u2, r1, r2 = " p1 p2 r1 r2)
        rankings-without-these-two (apply dissoc rankings [r1 r2])
        ;newr1 (if (rankings r1) (+ newr1 0.1) newr1)
        ;newr2 (if (rankings r2) (+ newr2 0.2) newr2)
        newr1 (if (rankings-without-these-two newr1) (+ newr1 0.1) newr1)
        newr2 (if (rankings-without-these-two newr2) (+ newr2 0.1) newr2)
        new-rankings (assoc rankings-without-these-two newr1 p1 newr2 p2)
        new-rankings (fix-ranks-as-int new-rankings)
        newr1 (get-rank-from-user new-rankings p1)
        newr2 (get-rank-from-user new-rankings p2)
        _ (if (or (not= r1 newr1) (not= r2 newr2))
            (cond (= score 1)
                  (print-rank-change p1 p2 r1 r2 newr1 newr2 "won" (game "date"))
                  (= score 0)
                  (print-rank-change p2 p1 r2 r1 newr2 newr1 "won" (game "date"))
                  (= score 1/2)
                  (print-rank-change p1 p2 r1 r2 newr1 newr2 "drew" (game "date")))
            )
        ;_ (println "new-rankings =" new-rankings)
        ]
    new-rankings))

(defn get-new-ranks-for-game [{white "white" black "black" :as game} rankings]
  (let [;_ (println rankings)
        uname1 (white "username")
        uname2 (black "username")
        r1 (get-rank-from-user rankings uname1)
        r2 (get-rank-from-user rankings uname2)
        ;_ (println uname1 uname2)
        ;_ (println r1 r2)
        s1 (res-code-to-score-map (white "result"))]
    (update-player-positions r1 r2 uname1 uname2 s1 rankings game)))

;; (get-new-ranks-for-game {"white" {"rating" 411, "result" "checkmated", "@id" "https://api.chess.com/pub/player/sajanarana", "username" "sajanarana"}
;;                          "black" {"rating" 1085, "result" "win", "@id" "https://api.chess.com/pub/player/ruk256", "username" "ruk256"}}
;;                         (sorted-map 5 "ruk256" 1 "sajanarana"))

(defn calc-new-rankings [db]
  (let [new-games (get-new-games db)
        player-rankings (apply sorted-map (interleave (range (count (:rankings db))) (:rankings db)))
        last-timestamp (:last-timestamp db)
        new-player-rankings
        (reduce (fn [ranks game]
                  (get-new-ranks-for-game game ranks))
                player-rankings new-games)]
    [(if (empty? new-games) last-timestamp ((last new-games) "end_time")) new-player-rankings]))