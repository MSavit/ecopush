;;; demo file for testing/debugging
(ns ecopush.demo
  [use [ecopush core push]])

;;; push-strat demos

(push-strat
 (random-code 10 @registered-instructions) ; randomly generate push code 
 (vec (repeatedly 10 #(rand-int 2)))	; random player-decisions   
 (vec (repeatedly 10 #(rand-int 2))))	; random all-decisions

;;; look at the auxiliary stack 
(:auxiliary
 (run-push 
  (random-code 10 @registered-instructions)
  (->>
   (make-push-state)
   (push-item (vec (repeatedly 10 #(rand-int 2))) :auxiliary) ; player-decisions
   (push-item (vec (repeatedly 10 #(rand-int 2))) :auxiliary) ; all-decisions			  
   (push-item 1 :integer)
   (push-item 0 :integer))))

;;; look at top item of auxiliary stack 
(top-item :auxiliary
	  (run-push 
	   (random-code 10 @registered-instructions)
	   (->>
	    (make-push-state)
	    (push-item (vec (repeatedly 10 #(rand-int 2))) :auxiliary) ; player-decisions
	    (push-item (vec (repeatedly 10 #(rand-int 2))) :auxiliary) ; all-decisions			  
	    (push-item 1 :integer)
	    (push-item 0 :integer))))

;;; create-players demos

;;; create player list all with random push stragies
(create-players 7 3 (repeatedly 7 #(Strategy. (random-code 10 @registered-instructions) "push")))

;;; create player list with mix of clojure and push strategies
(create-players 4 3 (cons (Strategy. (quote (rand-int 2)) "clj")
			  (repeatedly 3 #(Strategy. (random-code 10 @registered-instructions) "push"))))

(cons (repeatedly 3 #(Strategy. (random-code 10 @registered-instructions) "push")) (Strategy. (quote (rand-int 2)) "clj"))

;;; play a game as combination of strategies
(game (lazy-cat (repeatedly 3 #(Strategy. (quote (rand-int 2)) "clj")) (repeatedly 3 #(Strategy. (random-code 10 @registered-instructions) "push"))))

(lazy-cat (repeatedly 3 #(Strategy. (quote (rand-int 2)) "clj")) (repeat 1 (Strategy. (random-code 10 @registered-instructions) "push")))

(cat (Strategy. 'program "push") (Strategy. (quote (rand-int 2))))
(cons (repeat 1 (Strategy. nil "push")) (repeat (Strategy. (quote (rand-int 2)))))

(lazy-cat (repeatedly 1 #(Strategy. nil "push")) (repeatedly #(Strategy. (quote (rand-int 2)) "clj")))

(lazy-cat (repeatedly 1 #(Strategy. nil "push")) (take (dec *popsize*) (repeatedly #(Strategy. (quote (rand-int 2)) "clj"))))

;;; map scores-map onto a list of strategies
(map #(scores-map %) (stratmap (random-code 10 @registered-instructions) (list (quote (rand-int 2)))))

;;; play game with two strategies
(map #(scores-map %) (stratmap (random-code 10 @registered-instructions) (list (quote (rand-int 2)) 1 )))

;;; play game with three strategies
(map #(scores-map %) (stratmap (random-code 10 @registered-instructions) (list (quote (rand-int 2)) 1 0)))

;;; get payoffs for each game 
(map #(apply + %) (gametest (random-code 10 @registered-instructions) (list (quote (rand-int 2)) 1 0)))

;;; make a map of games
(genmap (map fit-compare (gametest (random-code 10 @registered-instructions) (list (quote (rand-int 2)) 1 0))))


;; test prog

(defn scores-map
  "returns list of payoff scores indexed by player val"
  [pushlist]
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
	      (-> pcode			; test with macroexpand
		  (if (= ptype "push")
		    (push-strat player-decisions all-decisions)
		    (eval))))
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
	    (map #(last (:choices %)) playerlist))
	  ;; get all player decisions
	  (get-all-decisions [playerlist]
	    (map #(:choices %) playerlist))
	  ;; get specific player decisions
	  (get-player-decisions [playernum playerlist]
	    (-> playerlist get-all-decisions (nth playernum)))
	  ;; sum payoff value 
	  (payoff-sum [decisions capacity]
	    (+ 1 (* 2 (- capacity (apply + decisions)))))
	  (apply-payoff [payoff player-struct]
	    (->> (if (zero? (last (:choices player-struct))) *nonentry-payoff* payoff)
		 (update-in player-struct [:payoffs] conj)))
	  (calculate-payoff [playerlist capacity]
	    (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
	      (map #(apply-payoff payoff %) playerlist)))
	  (player-decide [playerlist]
	    (let [past-decisions (get-all-decisions playerlist)]
	      (map #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))
	  (play-rounds [roundnum capacity & [pushlist]]
	    (if (zero? roundnum)
	      (create-players *popsize* capacity pushlist)
	      (-> (play-rounds (dec roundnum capacity pushlist)) ; test with macroexpand
		  (player-decide)
		  (calculate-payoff capacity))))
	  (game [pushlist]
	    (-> (for [x *capacity-list*] (play-rounds *rounds-num* x pushlist))
		(flatten)))
	  (return-map [pushlist]
	    (for [x (range *popsize*)]
	      (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) (game pushlist))))))]
    (return-map pushlist)))

(defn fit-fn
  [stratlist]
  (fn [program]
    (letfn [(sm [pushprog cljstrat]	
	      (cons (Strategy. pushprog "push")
		    (repeatedly 1 #(Strategy. cljstrat "clj"))))
	    (stratmap [pushprog stratlist]
	      (map #(sm pushprog %) stratlist))
	    (gametest [pushcode stratlist]
	      (map #(scores-map %) (stratmap pushcode stratlist)))
	    (test-fit [scorelist]
	      (if (empty? (first scorelist))
		0
		(apply + (map #(- % (first (second scorelist))) (first scorelist)))))
	    (fit-compare [scorelist]
	      (let [pushval (first scorelist)]
		(list (filter #(> % pushval) scorelist)
		      (filter #(= % pushval) scorelist)
		      (filter #(< % pushval) scorelist))))
	    (sum-games [pushcode stratlist]
	      (apply + (map #(test-fit %) (map fit-compare (gametest pushprog stratlist)))))]
      (doall
       (sum-games program stratlist)))))

(defn run [params]
  (let [popsize (:population params)
	caplist (:capacities params)
	stratlist (:strategies params)]	; gamelist should be a list of strategies 
    (pushgp
     :error-function (fit-fn stratlist)
     :atom-generators (concat
		       (registered-for-type :integer)
		       (registered-for-type :exec)
		       (registered-for-type :auxiliary))
     :population-size 10
     :mutation-probability 0.45
     :crossover-probability 0.45
     :simplification-probability 0.0
     :reproductive-simplifications 10)))

(defn avg [total num]
  (/ total num))

(defn mf1 [pc pd ad c popsize]
  (let [fad (partition pd ad)]
    
    (

     (for [x (range *popsize*)]
      (apply + (map #(nth % x fad) pd))))
    

    ))


(defn player-logic [player-code player-decisions all-decisions capacity]
  "player logic"
;  Uses average
(let [totalparticipation
(loop [totalparticipation (list)]
  (if (= (count totalparticipation) (- (/ (+ 1 capacity) 2) 1))
    totalparticipation
    (recur (cons (apply + (map # (nth % (count totalparticipation)) all-decisions)) totalparticipation))))]

(let [playerchange
(loop [playerchange (list)]
(if (= (count playerchange) (- (/ (+ 1 capacity) 2) 2))
playerchange
(recur (cons (- (nth totalparticipation (+ 1 (count playerchange))) (nth totalparticipation (count playerchange))) playerchange))))]

(if (< capacity 4)
   0
   (if (< (+ (/ (apply + playerchange) 2) (last totalparticipation)) capacity)
     1
     0)))))

(defn player-logic [player-code player-decisions all-decisions capacity]
  "player logic"
;  Uses Median
(let [totalparticipation
(loop [totalparticipation (list)]
  (if (= (count totalparticipation) (- (/ (+ 1 capacity) 2) 1))
    totalparticipation
    (recur (cons (apply + (map # (nth % (count totalparticipation)) all-decisions)) totalparticipation))))]

(let [playerchange
(loop [playerchange (list)]
(if (= (count playerchange) (- (/ (+ 1 capacity) 2) 2))
playerchange
(recur (cons (- (nth totalparticipation (+ 1 (count playerchange))) (nth totalparticipation (count playerchange))) playerchange))))]

(if (< capacity 4)
   0
   (if (< (+ (if ((count playerchange) odd?)
(nth playerchange (/ (- (count playerchange) 1) 2))
(/ (+ (nth playerchange (/ (count playerchange) 2)) (nth playerchange (/ (- (count playerchange) 2) 2))) 2)) (last totalparticipation)) capacity)
     1
     0)))))