(ns ecopush.re
  [use [ecopush.push]])

(defn linear-payoff
  "linear payoff function"
  [d v r c m]
  (if (= d 0)
    v
    (+ v (* r (- c m)))))

(defn smne
  [i N]
  (/ (- c 1)
     (- N 1)))

(defn smne2
  [plist c]
  (shuffle plist)
  (let [N (count plist)
	j (random-nth (range (dec c))) 
	k (random-nth (range (- N c)))
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

(defn simple-reinforcement
  [n past-round]
  (list
   (+ (previous-propensity 1)
      (* previous-strategy
	 (+ v (* r
		 (- capacity previous-entrants)))))
   (+ (previous-propensity 0)
      (- 1 (* previous-strategy
	      v)))))

(defn hypothetical-reinforcement
  [n past-round]
  (list
   (+ (previous-propensity 1)
      v
      (* r
	 (- c previous-entrants (- 1 previous-strategy))))
   (+ (previous-propensity 0)
      v)))

(defn activation
  [value]
  (if (not (number? value))
    0
    (round
     (/ (exp value)
	(+ (exp value)
	   1)))))

(defn push-eval
  [code gamelist]
  (activation
   (top-item :integer
	     (->>
	      (->> (make-push-state)
		   (push-item gamelist :auxiliary))
	      (run-push code)))))

(defn play
  [gamelist push strat]
  (map #(linear-payoff %)
       (flatten
	(push-eval push gamelist)
	(strat gamelist))))

(defn play-game
  [n gamelist push strat]
  (loop [r 0 g gamelist]
    (if (= n r)
      gamelist
      (recur (inc r) (play g push strat)))))

(defn square
  [x]
  (* x x))

(defn fitfn
  [game-description]
  (fn [program]
    (/ 1
       (square (apply + (map first (play-game program)))))))








