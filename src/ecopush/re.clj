(ns ecopush.re
  (:gen-class)
  [use [ecopush.push]
   [clojure.contrib.math :only (round)]
   [clojure.contrib.command-line]
   [incanter core stats]])

(defn linear-payoff
  "linear payoff function"
  [d v r c m]
  (if (= d 0)
    v
    (+ v (* r (- c m)))))

(defn decision
  [gamelist v r capacity population reinforcement]
  (let [past-round (last gamelist)
	previous-entrants (apply + past-round)]
    (loop [i (count past-round) gs '()]
      (if (= 0 i)
	gs
	(recur (dec i) (conj gs (reinforcement i past-round v r capacity previous-entrants population)))))))

(defn le
  "less than or equal to"
  [a b]
  (if (or (< a b) (= a b))
    true
    nil))

(def smne
  (fn
    [i past-round v r capacity previous-entrants population]
    (if (le (rand 1) (/ (- capacity 1) (- population 1)))
      0
      1)))

(defn smne-applic
  "check if smne is applicable for population size and capacity"
  [capacity population]
  (let [smeval (/ (* population (- capacity 1)) (- population 1))]
    (if (and (> capacity smeval) (> smeval (- capacity 1)))
      true
      nil)))

(defn applic
  "check if sme is applicable"
  [cap pop]
  (float (* pop (/
	  (- cap 1)
	  (- pop 1)))))



(def strat-smne
  (fn [population & x]
    (if (le (rand 1) (/ (- 20 1) (- population 1)))
      0
      1)))



(defn smne2
  [plist c]
  (shuffle plist)
  (let [N (count plist)
	j (rand-nth (range (dec c))) 
	k (rand-nth (range (- N c)))
	r (- N j k)]
    (flatten (cons (repeat j 1)
		   (repeat k 0)
		   (repeat r (/ (+ j (* (- c 1 j) (- N j k)))
				(- N j k 1)))))))

(defn linear-choice-rule
  [propensities]
  (let [in (first propensities)
	out (second propensities)]
    (/ in (+ in out))))

(defn entry-probability
  [propensities]
  (map linear-choice-rule propensities))

(defn previous-strategy
  "return the previous strategy"
  [pnum past-round]
  (nth (nth past-round pnum) 3))

(defn in-propensity
  [pnum past-round]
  (nth (nth past-round pnum) 2))

(defn out-propensity
  [pnum past-round]
  (nth (nth past-round pnum) 1))

(defn previous-propensity
 "return the previous propensity"
 [num pnum past-round]
 (if (= 1 num)
   (in-propensity pnum past-round)
   (out-propensity pnum past-round)))



(defn simple-reinforcement
  [n past-round v r capacity previous-entrants population]
  (list
   (+ (previous-propensity 1)
      (* previous-strategy
	 (+ v (* r
		 (- capacity previous-entrants)))))
   (+ (previous-propensity 0)
      (- 1 (* previous-strategy
	      v)))))

(defn hypothetical-reinforcement
  [n past-round v r capacity previous-entrants population]
  (list
   (+ (previous-propensity 1)
      v (* r
	   (- capacity previous-entrants (- 1 previous-strategy))))
   (+ (previous-propensity 0)
      v)))

;;; round needs to be loaded 
(defn activation
  [value]
  (if (not (number? value))
    0
    (round
     (/ (exp value)
	(+ (exp value)
	   1)))))

(def strat-rand
  (fn [& x]
    (rand-int 2)))





(defn push-eval
  [code gamelist]
  (activation
   (top-item :integer
	     (->>
	      (->> (make-push-state)
		   (push-item gamelist :auxiliary))
	      (run-push code)))))

;;; build strategy wrapper - see decide
(defn strat-map
  [gamelist population strat]
  (repeatedly (dec population) #(strat population gamelist))
  ;; (if (empty? gamelist)
  ;;   (repeatedly (dec population) #(strat population gamelist))
  ;;   (map #(strat population %) gamelist))
  )

;v r c m
(defn play
  [gamelist push strat population]
  (let [gamestate  (flatten (cons
		    (push-eval push gamelist)
		    ;; (map (fn [x] (x gamelist)) strat)
		    (strat-map gamelist population strat)
		    ))
	entered (apply + gamestate)]
    (map #(linear-payoff % 1 1 20 entered) gamestate)))

(defn play-game
  [rounds gamelist push strat population]
  (loop [r 0 g gamelist]
    (if (= rounds r)
      g
      (recur (inc r) (conj g (play g push strat population))))))

(defn square
  [x]
  (* x x))

(defn push-payoff-sum
  "Get payoffs of push player for game and return 1/payoff-sum"
  [game]
  (/ 1 (apply + (map first game))))

(def strat-enter
  (fn [a b]
    1))

(def strat-out
  (fn [a b]
    0))

(def strat-mixedrand
  (fn [population gamelist]
    ((rand-nth
      (list strat-mixedrand strat-rand strat-smne strat-enter strat-out))
     population gamelist)))

(defn mermap
  [lists]
  (if (empty? (first lists))
    (remove empty? lists)
    (cons (map first lists)
	  (mermap (map rest lists)))))

(defn push-sum
  [game]
  (let [gdata (mermap game)]
    (/ (apply + (first gdata))
       (count gdata))))

(defn average-score
  [game]
  (let [player-count (count (first game))
	player-totals (map #(/ (apply + %) player-count) (mermap game))]
    (/ (apply + player-totals) player-count)))

;;; interesting with e and x^3
(defn push-pop-avg-diff			; negative is better
  [game]
  (- (average-score game) (push-sum game)))

(defn push-pop-best-diff
  [game]
  "difference b/tw average of all better players and push player"
  (let [player-totals (map #(apply + %) (mermap game))
	push-score (first player-totals)
	player-scores (remove #(<= % push-score) (rest player-totals))]
    (if (empty? player-scores)
      0
      (- (/ (apply + player-scores) (count player-scores))
	 push-score))))


(defn ea-avg-diff
  [game]
  (exp (pow (push-pop-avg-diff game) 3)))

;;; could be total max payoff - push payoff
(defn fitfn
  [population rounds strat]
  (fn [program]
    (let [stratlist (repeat (dec population) strat)
	  game (play-game rounds '[] program strat population)]
      (list
       (push-pop-best-diff game)
       ;; (ea-avg-diff game)
       ;; (push-payoff-sum
       ;; 	(play-game rounds '[] program strat population))

       ))))

(defn run [params]
  (let [population (:population params)
	capacity (:capacity params)
	rounds (:rounds params)
	strat (:strategy params)]
    (pushgp
     :error-function (fitfn population rounds strat))))

(run {:population 21
      :capacity 20
      :rounds 500
      :strategy strat-mixedrand})

(defn -main [& args]
  (with-command-line args
    "Ecopush v.0"
    [[population "Population size" 10]
     [capacity "Capacity value " 20]
     [rounds "Number of rounds" 10]
     [strategy "Strategy" strat-enter]]
    (run {:population population
	  :capacity capacity
	  :rounds rounds
	  :strategy strategy})))



;; switch to clargon
;; http://jakemccrary.com/blog/2011/04/12/command-line-arguments-in-clojure.html