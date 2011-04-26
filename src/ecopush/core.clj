(ns ecopush.core
  "Core game for ecopush"
  (:gen-class)
  (:use [ecopush.push]
	[clojure.contrib.math]))

;;; Globals

(def ^{:doc "List of capacities for rounds. These are the odd numbers between 0 and 20."}
  *capacity-list* (range 1 20 1))

(def ^{:doc "The number of rounds is size of the capacity list, because each round represents a market entry game with a fixed capacity."}
  *rounds-num* (count *capacity-list*)) ; the above is not true and should be fixed

(def ^{:doc "population size (number of players in game"}
  *popsize* 10)

(def ^{:doc "payoff for staying out"}
  *nonentry-payoff* 1)

;;; Strategy record.
;; Code is the actual code, and type specifies whether or not the code is clojure or push.
;; Type can potentiall be extended to other jvm languages. type field can currently be either "push" or "clj". 
(defrecord Strategy [code type])

;;; Each player is a record. 
(defrecord Player [number choices payoffs capacity strategy])

;;; Game

;;; (player-logic) is expected to return a 1 or 0. However, pulling values off the :integer stack can return nil, so sanitize the input.
;;; Below is an example that returns 1 if the number is even, and 0 if it is odd. 
(defn push-wrapper
  "Handles exceptions when pulling off the :integer stack to produce 1,0."
  [c]
  (cond (not (number? c)) 0			; can return :no-stack-item for empty stack
	(nil? c) 0
	(neg? c) 0
	(odd? c) 0
	(even? c) 1))

(defn push-strat
  "player logic for push"
  [player-code player-decisions all-decisions]
  (push-wrapper (top-item :integer
			  (run-push
			   player-code
			   (->>
			    (make-push-state)
			    (push-item player-decisions :auxiliary) ; push player-decisions onto aux stack 
			    (push-item all-decisions :auxiliary) ; push all-decisions onto aux stack 
			    (push-item 1 :integer)
			    (push-item 0 :integer))))))

;;; Logic for players. If the player has a strategy in push, then run the push-code with push-strat.
;; If the player has a clojure strategy, eval the strategy.
;; Clojure strategies have access to player-decisions and all-decisions. 
(defn player-logic [player-strategy player-decisions all-decisions]
  "Evaluates player strategy code based on strategy type"
  (if (= (:type player-strategy) "push")
    (push-strat (:code player-strategy) player-decisions all-decisions)
    (eval (:code player-strategy))))

;;; Pushlist is a list of strategy records
(defn create-players [popsize capacity pushlist]
  "Create the initial struct of players with empty keys. If pushlist is shorter than population size, rest of population has last element of pushlist"
  (for [x (range 0 popsize)]
    (let [xloc (nth pushlist x (last pushlist))]
      (Slayer. x [] [] capacity (Strategy. (:code xloc) (:type xloc))))))

;;; potentially catch nil as it may errors
;;; see (get-decisions) run after create players 
(defn get-decisions
  "returns list of all player decisions for past round"
  [playerlist]
  (map #(last (:choices %)) playerlist))

(defn get-all-decisions
  "returns list of all decisions before current round for all players"
  [playerlist]
  (map #(:choices %) playerlist))

(defn get-player-decisions
  "returns a list of individual player's past decisions"
  [playernum playerlist]
  (-> playerlist get-all-decisions (nth playernum)))

(defn payoff-sum
  "sum the player decisions with proper weights"
  [decisions capacity]
  (+ 1 (* 2 (- capacity			; constants as def (from paper) 
	       (apply + decisions)))))			; integrate other weights 

(defn apply-payoff			
  "add the payoff to each player"
  [payoff player-struct]
  (->> (if (zero? (last (:choices player-struct))) *nonentry-payoff* payoff)
       (update-in player-struct [:payoffs] conj)))

(defn calculate-payoff
  "returns list of players with payoff applied"
  [playerlist capacity]
  (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
    (map #(apply-payoff payoff %) playerlist)))

(defn player-decide
  "player decide working"
  [playerlist]
  (let [past-decisions (get-all-decisions playerlist)]
    (map #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))

(defn play-rounds
  "function to play rounds. returns list of player structs"
  [roundnum capacity & [pushlist]]
  (cond
   (zero? roundnum) (create-players *popsize* capacity pushlist)
   :else (calculate-payoff
	  (player-decide
	   (play-rounds (dec roundnum) capacity pushlist))
	  capacity)))

(defn game
  "returns list of players with payoffs and choices in list of rounds"
  [pushlist]
  (flatten				
   (for [x *capacity-list*]
     (play-rounds *rounds-num* x pushlist))))

(defn scores-map
  "return the players with their payoff scores for game"
  [pushlist]
  (let [data (game pushlist)]
    (for [x (range *popsize*)]
      (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) data))))))

(defn get-my-move
  [data game round me]
  (nth (:choices (first (filter #(= (:number %) me) (filter #(= (:capacity %) (-
(* round 2) 1)) data)))) game))

(defn get-round
  [data game round]
  (map #(nth % game) (filter #(= (:capacity %) (- (* round 2) 1)) data)))

(defn payoff-sum
  "sum the player decisions with proper weights"
  [decisions capacity]
  (+ 1 (* 2 (- capacity			; constants as def (from paper)
	       (apply + decisions)))))			; integrate other weights




(defn scores-map1
  "return this players with their payoff scores for game"
  [pushlist]
  (let [data (game pushlist)]
    (for [x (range *popsize*)]
       (map #(apply + (:choices %)) (filter #(= (:number %) x) data)))))

(defn average-payoff
  "returns average payoff of players in game"
  [pushlist]
  (/ (apply + (scores-map pushlist)) *popsize*))

;; (pushgp
;;  :error-function (fn [program]
;; 		   (- 500 (average-payoff (repeat *popsize* program))))
;;  :max-points 100
;;  :population-size 50
;;  :trivial-geography-radius 10)

(defn winner-map
  "returns the winners of the game sorted by payoffs. Key is player number, value is payoff total"
  [pushlist]
  (sort-by last >
	   (let [data (game pushlist)]
	     (for [x (range *popsize*)]
	       [(keyword (str x)) (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) data)))])))) ; messy, clean w/ flatten 

;;;;;;;;;;;;;;;;
;; strategies ;;
;;;;;;;;;;;;;;;;

;;; in strategies, don't vary the capacity but keeping same round number

;; (define-registered decision-info-me
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item (right-in astate) :auxiliary)))))


(defn strat-1 [] "entry strategy" 1)
(defn strat-0 [] "stay-out strategy" 0)

;; probabilistic entry choice rules

;;; linear choice rule
(defn entry-prob-lin
  "linear choice rule for probability of agent entry"
  [strat-1 strat-0]
  (/ strat-1 (+ strat-1 strat-0)))

;;; exponential choice rule
(defn entry-prob-exp
  "exponential choice rule for probability of agent entry"
  []
  nil)

;; learning models

;;; simple reinforcement
(defn entry-lm-sr
  "returns probabilities for simple reinforcement"
  []
  ;; recursive
  ;; p6 of paper
  nil)

;;; hypothetical reinforcement
(defn entry-lm-hr
  "returns probabilities for hypothetical reinforcement"
  [all-decisions]			; past-decisions as well?
  ;; recursive
  ;; p6 of paper
  nil)

(defn entry-prob-sfp
  "stochastic fictitious play"		; combine exp prob and entry hr
  []
  nil)

;; stochastic approximation (p.6)
(defn stoch-approx
  "calculates expected motion of player's strategy adjustment"
  []
  nil)

(defn -main [& args]
  (println "test of main")
  ;; (use (symbol (first args)))
  (System/exit 0)
  )

;;;;;;;;;;;;;;;;;;;;;;
;; equilibria tests ;;
;;;;;;;;;;;;;;;;;;;;;;

(defn eq-nash-pure?
  "returns whether game is pure Nash equilibrium"
  []
  nil)

(defn eq-nash-sme?
  "returns whether game is symmetric mixed Nash equilibrium"
  []
  nil)

(defn eq-nash-asm?
  []
  "returns whether game is asymmetric mixed equilibria"
  nil)

;; random push code
;; (random-code 100 (concat @registered-instructions
;;                              (list (fn [] (lrand-int 100))
;; 				   (fn [] (lrand)))))

;; (run-push '(1 1 integer_add integer_add) (->>
;; 					  (make-push-state)
;; 					  (push-item 3 :integer)
;; 					  (push-item 7 :integer)))

;; (first (:integer (run-push (random-code 100 @registered-instructions) (->>
;; 								       (make-push-state)
;; 								       (push-item 1 :integer)
;; 								       (push-item 0 :integer)))))
;; (repeatedly 5 #(push-strat))

;; (repeatedly 10 #(random-code 10 @registered-instructions)) ; generate random players 
   
	     
;;;;;;;;;;
;; push ;;
;;;;;;;;;;

;; (define-registered out
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item 0 :auxiliary)))))

;; (define-registered in
;;   (fn [state]
;;     (let [astate (stack-ref :auxiliary 0 state)]
;;       (->> state
;; 	   (pop-item :auxiliary)
;; 	   (push-item 0 :auxiliary)))))

;; populate game with list of player code
;; sum payoffs for each player
;; (1/ (player.payoffsum / maxpossible.payoffsum)) * constant multiplier
;; sum total mean payoff? (for collective)

;; are we selecting for a set of push players? - evolve collectively 
;; are we selecting for one individual player? - evolve individually
;; are we going to mix push programs and other strategies? -
;;;; evolve individually / use beat other strategy as fitness function for collective
;; population size? for collective vs. individual

;; reproduction - within game / across games

;; (defn gp-start
;;   []
;;   (pushgp
;;    :error-function (fn [program]
;; 		     (map #(- 3000 %) (scores-map))))
;;   :atom-generators (concat
;; 		    (registered-for-type :integer)
;; 		    (registered-for-type :exec)
;; 		    (registered-for-type :boolean)
;; 		    (list
;; 		     'boolean_and
;; 		     'boolean_not
;; 		     'boolean_or))
;;   :error-threshold 40
;;   :reproduction-simplifications 10)

;;;;;;;;;;
;; push ;;
;;;;;;;;;;

;; http://techbehindtech.com/2010/06/25/parsing-xml-in-clojure/
;; http://stackoverflow.com/questions/4641964/how-to-use-update-in-in-clojure
;; above is update-in
;; http://java.ociweb.com/mark/clojure/article.html
;; maarten kaijser - efficiently representing populations in genetic programming 
;; todo
;;; build in changing capacity [just mod total number of rounds or tmi?]
;;; build game around it
;;; jason nobel richard watson -
;;; juxt - http://richhickey.github.com/clojure/clojure.core-api.html#clojure.core/juxt

;;; push here
