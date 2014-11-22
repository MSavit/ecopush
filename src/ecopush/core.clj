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

(defn pr [roundnum capacity pushlist]
  (loop [gamestate (create-players *popsize* capacity pushlist)
	 cnt 0
	 maxcount roundnum]
    (if (> cnt maxcount)
      gamestate
      (recur (-> gamestate (player-decide) (calculate-payoff capacity))
	     (inc cnt)
	     maxcount))))

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

(defn scores-map
  "return the players with their payoff scores for game"
  [pushlist]
  (let [data (game pushlist)]
    (for [x (range *popsize*)]
      (apply + (pmap #(apply + (:payoffs %)) (filter #(= (:number %) x) data))))))

(defn sm
  "builds list of push program and strategy"
  [pushprog cljstrat]
  (cons (Strategy. pushprog "push")
	(repeatedly 1 #(Strategy. cljstrat "clj")))) ; need to get quoting here to work

(defn stratmap
  "maps sm onto strategy list"
  [pushprog stratlist]
  (pmap #(sm pushprog %) stratlist))	; weird list stuff here (quote the list and then quote each element) 

(defn gametest
  "test games"
  [pushcode stratlist]
  (pmap #(scores-map %) (stratmap pushcode stratlist)))

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
    (apply + (pmap #(- % (first (second scorelist))) (first scorelist)))))

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
	      (apply + (pmap #(test-fit %) (pmap fit-compare (gametest pushcode stratlist)))))]
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

#_(run {:popsize 10
      :caplist (range 1 20 1)
      :gamelist (list (quote (rand-int 2)) 0 1)
      })

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
;; http://jan.rychter.com/enblog/2009/7/20/clojure-performance-tuning.html
;; http://sites.google.com/site/gpredcomp/7-baseline-models
;; http://sites.google.com/site/gpredcomp/9-references
;; http://clojure.org/java_interop#Java%20Interop-Type%20Hints