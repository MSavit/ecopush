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
    (+
     v (* r (- c m)))))

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

;;; write this ;)
(defn previous-propensity
  "take the previus propensity for the player"
  []
  nil)

;;; write this ;)
(defn previous-strategy
  "return the previous strategy"
  []
  nil)

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
  [population rounds]
  (fn [program]
    (list
     (/ 1
	(+ 0.01
	   (apply + (map first
			 (play-game rounds '[] program (list strat-rand)))))))))

(defn run [params]
  (let [population (:population params)
	capacity (:capacity params)
	rounds (:rounds params)]
    (pushgp
     :error-function (fitfn population rounds))))

;; (defn -main [& args]
;;   (with-command-line args
;;     "Ecopush v.0"
;;     [[population "Population size" 10]
;;      [capacity "Capacity value " 20]
;;      [rounds "Number of rounds" 10]]
;;     (binding [*population* population
;; 	      *capacity* capacity
;; 	      *rounds* rounds]
;;       (run {:population *population*
;; 	    :capacity *capacity*
;; 	    :rounds *rounds*}))))


;; switch to clargon
;; http://jakemccrary.com/blog/2011/04/12/command-line-arguments-in-clojure.html