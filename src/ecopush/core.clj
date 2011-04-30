(ns ecopush.core
  "Core game for ecopush"
  (:use [ecopush.push]
	[clojure.contrib.math])
  (:gen-class))

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
      (Player. x [] [] capacity (Strategy. (:code xloc) (:type xloc))))))

;;; potentially catch nil as it may errors
;;; see (get-decisions) run after create players 
(defn get-decisions
  "returns list of all player decisions for past round"
  [playerlist]
  (pmap #(last (:choices %)) playerlist))

(defn get-all-decisions
  "returns list of all decisions before current round for all players"
  [playerlist]
  (pmap #(:choices %) playerlist))

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
    (pmap #(apply-payoff payoff %) playerlist)))

(defn player-decide
  "player decide working"
  [playerlist]
  (let [past-decisions (get-all-decisions playerlist)]
    (pmap #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))

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

(defn create-players [popsize capacity pushlist]
  "Create the initial struct of players with empty keys. If pushlist is shorter than population size, rest of population has last element of pushlist"
  (for [x (range 0 popsize)]
    (let [xloc (nth pushlist x (last pushlist))]
      (Player. x [] [] capacity (Strategy. (:code xloc) (:type xloc))))))


;; (defn scores-map
;;   "returns list of payoff scores indexed by player val"
;;   [pushlist]
;;   (letfn [(push-wrapper [c]
;; 	    (cond (not (number? c)) 0
;; 		  (nil? c) 0
;; 		  (odd? c) 0
;; 		  (even? c) 1))
;; 	  (push-strat [player-code player-decisions all-decisions]
;; 	    (-> (->> (->> (make-push-state)
;; 			  (push-item player-decisions :auxiliary)
;; 			  (push-item all-decisions :auxiliary)
;; 			  (push-item 1 :integer)
;; 			  (push-item 0 :integer))
;; 		     (run-push player-code)
;; 		     (top-item :integer))
;; 		push-wrapper))
;; 	  (player-logic [player-strategy player-decisions all-decisions]
;; 	    (let [pcode (:code player-strategy)
;; 		  ptype (:type player-strategy)]
;; 	      (-> pcode			; test with macroexpand
;; 		  (if (= ptype "push")
;; 		    (push-strat player-decisions all-decisions)
;; 		    (eval))))
;; 	    (if (= (:type player-strategy) "push")
;; 	      (push-strat (:code player-strategy) player-decisions all-decisions)
;; 	      (eval (:code player-strategy))))
;; 	  ;; create a list of player records
;; 	  (create-players [popsize capacity pushlist]
;; 	    (for [x (range 0 popsize)]
;; 	      (let [xloc (nth pushlist x (last pushlist))]
;; 		(->> (Strategy. (:code xloc) (:type xloc))
;; 		     (Player. x [] [] capacity)))))
;; 	  ;; get current player decisions 
;; 	  (get-decisions [playerlist]
;; 	    (map #(last (:choices %)) playerlist))
;; 	  ;; get all player decisions
;; 	  (get-all-decisions [playerlist]
;; 	    (map #(:choices %) playerlist))
;; 	  ;; get specific player decisions
;; 	  (get-player-decisions [playernum playerlist]
;; 	    (-> playerlist get-all-decisions (nth playernum)))
;; 	  ;; sum payoff value 
;; 	  (payoff-sum [decisions capacity]
;; 	    (+ 1 (* 2 (- capacity (apply + decisions)))))
;; 	  (apply-payoff [payoff player-struct]
;; 	    (->> (if (zero? (last (:choices player-struct))) *nonentry-payoff* payoff)
;; 		 (update-in player-struct [:payoffs] conj)))
;; 	  (calculate-payoff [playerlist capacity]
;; 	    (let [payoff (payoff-sum (get-decisions playerlist) capacity)]
;; 	      (map #(apply-payoff payoff %) playerlist)))
;; 	  (player-decide [playerlist]
;; 	    (let [past-decisions (get-all-decisions playerlist)]
;; 	      (map #(update-in % [:choices] conj (player-logic (:strategy %) (:choices %) past-decisions)) playerlist)))
;; 	  (play-rounds [roundnum capacity & [pushlist]]
;; 	    (if (zero? roundnum)
;; 	      (create-players *popsize* capacity pushlist)
;; 	      (-> (play-rounds (dec roundnum capacity pushlist)) ; test with macroexpand
;; 		  (player-decide)
;; 		  (calculate-payoff capacity))))
;; 	  (game [pushlist]
;; 	    (-> (for [x *capacity-list*] (play-rounds *rounds-num* x pushlist))
;; 		(flatten)))
;; 	  (return-map [pushlist]
;; 	    (for [x (range *popsize*)]
;; 	      (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) (game pushlist))))))]
;;     (return-map pushlist)))

(defn scores-map
  "return the players with their payoff scores for game"
  [pushlist]
  (let [data (game pushlist)]
    (for [x (range *popsize*)]
      (apply + (pmap #(apply + (:payoffs %)) (filter #(= (:number %) x) data))))))

;; (defn fit
;;   [prog]
;;   (list (* 10
;;      (first (scores-map (cons (Strategy. prog "push") (repeatedly 1 #(Strategy. (quote (rand-int 2)) "clj"))))))))

;; (doall
;;  (list (let [scores (scores-map (cons (Strategy. prog "push") (repeatedly 1 #(Stragegy. (quote (rand-int 2)) "clj"))))
;; 	     best (apply max scores)
;; 	     gpres (first scores)]
;; 	 (* 100 (- best gpres)))))

;; (defn fit
;;   [prog]
;;   1000)

;; (defn fit
;;   [program]
;;   (count (scores-map (lazy-cat (repeatedly 1 #(Strategy. program "push")) (take (dec *popsize*) (repeatedly #(Strategy. (quote (rand-int 2)) "clj")))))))

  (defn fit				;should wrap fn
    "preliminary fitness function"
    [program]
    (doall
     (let [scores (scores-map (cons (Strategy. program "push") (repeatedly 1 #(Strategy. (quote (rand-int 2)) "clj"))))
	   best (apply max scores)
	   gpscore (first scores)]
       (list (* 100 (- best gpscore))))))

;; (pushgp
;;  :error-function (fn [program]
;; 		   (doall
;; 		    (let [scores (scores-map (cons (Strategy. program "push") (repeatedly 1 #(Strategy. (quote (rand-int 2)) "clj"))))
;; 			  best (apply max scores)
;; 			  gpres (first scores)]
;; 		      (list (* 100 (- best gpres))))))
;;   :atom-generators (concat
;; 		   (registered-for-type :integer)
;; 		   (registered-for-type :exec)
;; 		   (registered-for-type :auxiliary))
;;   :population-size 10
;;   :mutation-probability 0.45
;;   :crossover-probability 0.45
;;   :simplification-probability 0.0
;;   :reproductive-simplifications 10)

;;; pass run a population size, capacity list, game list
;;; game list should be a set of strategies to employ  

(defn fit				;should wrap fn
  "preliminary fitness function"
  [program]
  (doall
   (let [scores (scores-map (cons (Strategy. program "push") (repeatedly 1 #(Strategy. (quote (rand-int 2)) "clj"))))
	 best (apply max scores)
	 gpscore (first scores)]
     (list (* 100 (- best gpscore))))))


(defn sm
  "builds list of push program and strategy"
  [pushprog cljstrat]
  (cons (Strategy. pushprog "push")
	(repeatedly 1 #(Strategy. cljstrat "clj")))) ; need to get quoting here to work

(defn stratmap
  "maps sm onto strategy list"
  [pushprog stratlist]
  (map #(sm pushprog %) stratlist))	; weird list stuff here (quote the list and then quote each element) 

(defn gametest
  "test games"
  [pushcode stratlist]
  (map #(scores-map %) (stratmap pushcode stratlist)))

(defn mean
  "takes the mean of a list, assumes all elements are numbers"
  [lst]
  (/ (apply + lst)
     (count lst)))

(defn fit-compare
  "compares the fitness"
  [scorelist]
  (let [pushval (first scorelist)]
    (list
      (filter #(> % pushval) scorelist)
      (filter #(= % pushval) scorelist)
      (filter #(< % pushval) scorelist))))

(defn square [n] (* n n))

(defn test-fit
  "sum the difference between every better player and the push player"
  [scorelist]
  (if (empty? (first scorelist))
    0
    (apply + (map #(- % (first (second scorelist))) (first scorelist)))))

;; (defn game-fitness
;;   "the fitness function for games"
;;   [stratlist]
;;   (fn [program]
;;     (doall
;;      (sum-games program stratlist))))
					; sum-games is not defined right now

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
	      (apply + (map #(test-fit %) (map fit-compare (gametest pushcode stratlist)))))]
      (doall
       (list (sum-games program stratlist))))))



(defn run [params]
  (let [popsize (:popsize params)
	caplist (:caplist params)
	gamelist (:gamelist params)]
    (pushgp
     :error-function (fit-fn gamelist)
     :atom-generators (concat
		       (registered-for-type :integer)
		       (registered-for-type :exec)
		       (registered-for-type :auxiliary))
     :population-size 10
     :mutation-probability 0.45
     :crossover-probability 0.45
     :simplification-probability 0.0
     :reproductive-simplifications 10)))

(run {:popsize 10
      :caplist (range 1 20 1)
      :gamelist (list (quote (rand-int 2)) 0 1)
      })

;; (fn [program]
;; 		       (doall
;; 			(let [scores (scores-map (cons (Strategy. program "push") (repeatedly 1 #(Strategy. (quote (rand-int 2)) "clj"))))
;; 			      best (apply max scores)
;; 			      gpres (first scores)]
;; 			  (list (* 100 (- best gpres))))))

;; (defn build-map
;;   "build the map"
;;   [scorelist gnum]
;;   {(keyword (str gnum))
;;    {:better (first scorelist)
;;     :equal (second scorelist)
;;     :worse (last scorelist)}})

;; (defn genmap
;;   [scorelist]
;;   (apply merge (for [x (range 0  (count scorelist))]
;; 		 (build-map (nth scorelist x) x))))

;; (defn fit-map
;;   "make a map for fitness calculation of individual"
;;   [pushcode stratlist]
;;   (genmap (map fit-compare (gametest pushcode stratlist))))

;; (defn scores-map1
;;   "return this players with their payoff scores for game"
;;   [pushlist]
;;   (let [data (game pushlist)]
;;     (for [x (range *popsize*)]
;;        (map #(apply + (:choices %)) (filter #(= (:number %) x) data)))))

;; (defn average-payoff
;;   "returns average payoff of players in game"
;;   [pushlist]
;;   (/ (apply + (scores-map pushlist)) *popsize*))

;; (defn winner-map
;;   "returns the winners of the game sorted by payoffs. Key is player number, value is payoff total"
;;   [pushlist]
;;   (sort-by last >
;; 	   (let [data (game pushlist)]
;; 	     (for [x (range *popsize*)]
;; 	       [(keyword (str x)) (apply + (map #(apply + (:payoffs %)) (filter #(= (:number %) x) data)))]))))
					; messy, clean w/ flatten 

(defn -main [& args]
  (println "test of main")

  ;; (use (symbol (first args)))
  (System/exit 0))

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

;; http://faustus.webatu.com/clj-quick-ref.html#list
;; performace http://gnuvince.wordpress.com/2009/05/11/clojure-performance-tips/