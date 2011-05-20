(ns ecopush.re

  [use [ecopush.push]
   [clojure.contrib.math :only (round)]
   [clojure.contrib.command-line]
   [incanter.core]]
(:gen-class))

(defn linear-payoff
  "linear payoff function"
  [d v r c m]
  (if (= d 0)
    v
    (+ v (* r (- c m)))))

(defn smne
  [i N c]
  (/ (- c 1)
     (- N 1)))

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

(defn decision
  [gamelist v r capacity population]
  (let [past-round (last gamelist)
	previous-entrants (apply + past-round)]
    (loop [i (count past-round) gs '()]
      (if (= 0 i)
	gs
	(recur (dec i) (conj gs (simple-reinforcement i past-round v r capacity previous-entrants)))))))

(defn simple-reinforcement
  [n past-round v r capacity previous-entrants]
  (list
   (+ (previous-propensity 1)
      (* previous-strategy
	 (+ v (* r
		 (- capacity previous-entrants)))))
   (+ (previous-propensity 0)
      (- 1 (* previous-strategy
	      v)))))

(defn hypothetical-reinforcement
  [n past-round v r c previous-entrants]
  (list
   (+ (previous-propensity 1)
      v (* r
	   (- c previous-entrants (- 1 previous-strategy))))
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
  (fn [x]
    (rand-int 2)))

(defn push-eval
  [code gamelist]
  (activation
   (top-item :integer
	     (->>
	      (->> (make-push-state)
		   (push-item gamelist :auxiliary))
	      (run-push code)))))

;v r c m
(defn play
  [gamelist push strat]
  (let [gamestate  (cons
		    (push-eval push gamelist)
		    (map (fn [x] (x gamelist)) strat))
	entered (apply + gamestate)]
    (map #(linear-payoff % 1 1 20 entered) gamestate)))




;;; include popsize
(defn play-game
  [n gamelist push strat]
  (loop [r 0 g gamelist]
    (if (= n r)
      g
      (recur (inc r) (conj g (play g push strat))))))

(defn square
  [x]
  (* x x))

(defn push-payoff-sum
  "Get payoffs of push player for game and return 1/payoff-sum"
  [game]
  (/ 1 (apply + (map first game))))

(def strat-enter
  (fn [x]
    1))

(def strat-out
  (fn [x]
    0))

(defn fitfn
  [population rounds strat]
  (fn [program]
    (let [stratlist (repeat (dec population) strat)]
      (list
       (push-payoff-sum
	(play-game rounds '[] program stratlist))))))

(defn run [params]
  (let [population (:population params)
	capacity (:capacity params)
	rounds (:rounds params)
	strat (:strategy params)]
    (pushgp
     :error-function (fitfn population rounds strat))))

#_(run {:population 4
      :capacity 3
      :rounds 4
      :strategy strat-enter})

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