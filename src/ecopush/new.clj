(ns ecopush.new
  [use [ecopush.push]])

(defrecord Strategy [code type])
(defrecord Player [number choices payoffs capacity strategy])

(defn scores-map
  "returns list of payoff scores indexed by player val"
  [pushlist]
  (let [nonentry-payoff 1
	popsize 10
	capacity-list (range 1 20 1)
	rounds-num (count capacity-list)]
   (letfn [(push-wrapper [c]
	    (cond (not (number? c)) 0
		  (nil? c) 0
		  (odd? c) 0
		  (even? c) 1))
	  (push-strat [player-code player-decisions all-decisions]
	    (-> (->> (->> (make-push-state)
			  (push-item player-decisions :auxiliary)
			  (push-item all-decisions :auxiliary)
			  (push-item 1 :integer)
			  (push-item 0 :integer))
		     (run-push player-code)
		     (top-item :integer))
		push-wrapper))
	  (player-logic [player-strategy player-decisions all-decisions]
	    (let [pcode (:code player-strategy)
		  ptype (:type player-strategy)]
	      (if (= ptype "push")
		(-> pcode (push-strat player-decisions all-decisions))
		(-> pcode (eval))))
	    (if (= (:type player-strategy) "push")
	      (push-strat (:code player-strategy) player-decisions all-decisions)
	      (eval (:code player-strategy))))
	  ;; create a list of player records
	  (create-players [popsize capacity pushlist]
	    (for [x (range 0 popsize)]
	      (let [xloc (nth pushlist x (last pushlist))]
		(->> (Strategy. (:code xloc) (:type xloc))
		     (Player. x [] [] capacity)))))
	  ;; get current player decisions 
	  (get-decisions [playerlist]
	    (pmap #(last (:choices %)) playerlist))
	  ;; get all player decisions
	  (get-all-decisions [playerlist]
	    (pmap #(:choices %) playerlist))
	  ;; get specific player decisions
	  (get-player-decisions [playernum playerlist]
	    (-> playerlist get-all-decisions (nth playernum)))
	  ;; sum payoff value 
	  (payoff-sum [decisions capacity]
	    (+ 1 (* 2 (- capacity (apply + decisions)))))
	  (apply-payoff [payoff player-struct]
	    (->> (if (zero? (last (:choices player-struct))) nonentry-payoff payoff)
		 (update-in player-struct [:payoffs] conj)))
	  (calculate-payoff [playerlist capacity]
	    (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
	      (pmap #(apply-payoff payoff %) playerlist)))
	  (player-decide [playerlist]
	    (let [past-decisions (get-all-decisions playerlist)]
	      (pmap #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))
	  (pr [roundnum capacity pushlist]
	    (loop [gamestate (create-players popsize capacity pushlist)
		   cnt 0
		   maxcount roundnum]
	      (if (> cnt maxcount)	;error
		gamestate
		(recur (-> gamestate (player-decide) (calculate-payoff capacity))
		       (inc cnt)
		       maxcount))))
	  (play-rounds [roundnum capacity & [pushlist]]
	    (if (zero? roundnum)
	      (create-players popsize capacity pushlist)
	      (-> (play-rounds (dec roundnum) capacity pushlist) ; test with macroexpand
		  (player-decide)
		  (calculate-payoff capacity))))
	  (game [pushlist]
	    (-> (for [x capacity-list] (pr rounds-num x pushlist))
		(flatten)))
	  (return-map [pushlist]
	    (for [x (range popsize)]
	      (apply + (pmap #(apply + (:payoffs %)) (filter #(= (:number %) x) (game pushlist))))))]
    (return-map pushlist))))

;; 33, 30, 80, 84
;; fix calls to eval

(defn fit-fn
  [stratlist]
  (fn [program]
    (letfn [(sm [pushprog cljstrat]	
	      (cons (Strategy. pushprog "push")
		    (repeatedly 1 #(Strategy. cljstrat "clj"))))
	    (stratmap [pushprog stratlist]
	      (pmap #(sm pushprog %) stratlist))
	    (gametest [pushcode stratlist]
	      (pmap #(scores-map %) (stratmap pushcode stratlist)))
	    (test-fit [scorelist]
	      (if (empty? (first scorelist))
		0
		(apply + (pmap #(- % (first (second scorelist))) (first scorelist)))))
	    (fit-compare [scorelist]
	      (let [pushval (first scorelist)]
		(list (filter #(> % pushval) scorelist)
		      (filter #(= % pushval) scorelist)
		      (filter #(< % pushval) scorelist))))
	    (sum-games [pushcode stratlist]
	      (list (apply + (pmap #(test-fit %) (pmap fit-compare (gametest pushcode stratlist))))))
	    
	    (sum-games2 [pushcode stratlist]
	      (pmap #(test-fit %) (pmap fit-compare (gametest pushcode stratlist))))]
      ;; (doall
      ;;  (list (sum-games program stratlist)))
      ;; (sum-games2 program stratlist)
      (sum-games program stratlist)
      )))



(defn run [params]
  (let [popsize (:population params)
	caplist (:capacities params)
	stratlist (:strategies params)]
    (pushgp
     :error-function (fit-fn stratlist)
     :atom-generators (concat
		       (registered-for-type :integer)
		       (registered-for-type :exec)
		       (registered-for-type :auxiliary)) ;there may be no such thing
     :population-size 10
     :mutation-probability 0.45
     :crossover-probability 0.45
     :simplification-probability 0.0
     :reproductive-simplifications 10)))

(run {:popsize 10
      :caplist (range 1 20 1)
      :strategies (list (quote (rand-int 2)) 0 1)})