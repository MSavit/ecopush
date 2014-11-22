(ns ecopush.mg
  [use [ecopush push]])

(defrecord Strategy [code type])
(defrecord Player [number choices payoffs capacity strategy])

(defn push-wrapper
  [c]
  (cond (not (number? c)) 0
	(nil? c) 0
	(odd? c) 0
	(even? c) 1))

(defn push-strat
  [player-code player-decisions all-decisions]
  (-> (->> (->> (make-push-state)
		(push-item player-decisions :auxiliary)
		(push-item all-decisions :auxiliary)
		(push-item 1 :integer)
		(push-item 0 :integer))
	   (run-push player-code)
	   (top-item :integer))
      push-wrapper))

(defn player-logic
  [player-strategy player-decisions all-decisions]
  (let [pcode (:code player-strategy)
	ptype (:type player-strategy)]
    (if (= ptype "push")
      (-> pcode (push-strat player-decisions all-decisions))
      (-> pcode (eval))))
  (if (= (:type player-strategy) "push")
    (push-strat (:code player-strategy) player-decisions all-decisions)
    (eval (:code player-strategy))))

(defn create-players
  [popsize capacity pushlist]
  (for [x (range 0 popsize)]
    (let [xloc (nth pushlist x (last pushlist))]
      (->> (Strategy. (:code xloc) (:type xloc))
	   (Player. x [] [] capacity)))))

(defn get-decisions
  [playerlist]
  (pmap #(last (:choices %)) playerlist))

(defn get-all-decisions
  [playerlist]
  (pmap #(:choices %) playerlist))

(defn get-player-decisions
  [playernum playerlist]
  (-> playerlist get-all-decisions (nth playernum)))

(defn payoff-sum
  [decisions capacity]
  (+ 1 (* 2 (- capacity (apply + decisions)))))

(defn apply-payoff
  [payoff player-struct]
  (->> (if (zero? (last (:choices player-struct))) nonentry-payoff payoff)
       (update-in player-struct [:payoffs] conj)))

(defn calculate-payoff
  [playerlist capacity]
  (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
    (pmap #(apply-payoff payoff %) playerlist)))

(defn player-decide
  [playerlist]
  (let [past-decisions (get-all-decisions playerlist)]
    (pmap #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))

(defn play-rounds
  [roundnum capacity & [pushlist]]
  (if (zero? roundnum)
    (create-players popsize capacity pushlist)
    (-> (play-rounds (dec roundnum) capacity pushlist) ; test with macroexpand
	(player-decide)
	(calculate-payoff capacity))))

(defn pr
  [roundnum capacity pushlist]
  (pr2 (create-players popsize capacity pushlist) 0 roundnum))

(defn pr2
  [gamestate counter maxcount]
  (if (> counter maxcount)
    gamestate
    (pr2 (-> gamestate
	     (player-decide)
	     (calculate-payoff capacity))
	 (inc counter)
	 maxcount)))

(defn pr [roundnum capacity pushlist]
  (loop [gamestate (create-players popsize capacity pushlist)
	 cnt 0
	 maxcount roundnum]
    (if (> cnt maxcount)
      gamestate
      (recur (-> gamestate (player-decide) (calculate-payoff capacity))
	     (inc cnt)
	     maxcount))))


(defn game
  [pushlist]
  (-> (for [x capacity-list] (play-rounds rounds-num x pushlist))
      (flatten)))

(defn return-map
  [pushlist]
  (for [x (range popsize)]
    (apply + (pmap #(apply + (:payoffs %)) (filter #(= (:number %) x) (game pushlist))))))

