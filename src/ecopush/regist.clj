(ns ecopush.regist
  "Micah's registered instructions"
  (:use [ecopush core push]))

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

(defn best-move
  [data game round me]
  (let [my-move (get-my-move data game round me)
        moves (get-round data game round)]
    (if (<
          (if (= my-move 0) 1
            (payoff-sum moves (- (* round 2) 1)))
          (if (= my-move 1) 1
          (payoff-sum (cons -1 moves) (- (* round 2) 1))))
      my-move
      (* (- my-move 1) -1))))

(defn pay-for-going-in
  [data game round me]
  (let[moves (get-round data game round)]
      (if (= (get-my-move data game round me) 0)
        (payoff-sum (cons -1 moves) (- (* round 2) 1))
        (payoff-sum moves (- (* round 2) 1)))))

(defn average-pay-when-in
  [data game1 game2 round me]
  (let [dataset (for [x (range game1 (inc game2))]
                  (pay-for-going-in data x round me))]
        (/ (apply + dataset) (count dataset))))

(define-registered pay-for-going-in
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (pop :integer state)
           game (if (and (> g 0) (< (mod g (stack-ref :auxiliary 2 state))
(stack-ref :auxiliary 2 state)))
                    (mod g (stack-ref :auxiliary 2 state))
                    0)
           r (pop :integer state)
           round (if (and (> r 0) (< (mod r 10) (stack-ref :auxiliary 1 state)))
                   (mod r 10)
                   0)
           me (stack-ref :auxiliary 3 state)]
          (if (or (= game 0) (= round 0))
              0
              (pay-for-going-in
                data
                game
                round
                me)))
              :boolean state)
      state)))

(define-registered average-pay-when-in
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (stack-ref :auxiliary 2 state)
           g1 (pop :integer state)
           game1 (if (and (> g1 0) (< (mod g1 g) g))
                   (mod g1 g)
                   0)
           g2 (pop :integer state)
            game2 (if (and (> g2 0) (< (mod g2 g) g))
                    (mod g2 g)
                    0)
            round (stack-ref :auxiliary  1 state)
            me (stack-ref :auxiliary 3 state)]
          (if (or (= game1 0) (= game2 0))
            0
            (average-pay-when-in
              data
              game1
              game2
              round
              me)))
      :boolean state)
    state)))

(define-registered get-game-number
  (fn [state]
    (push-item
      (stack-ref :auxiliary 2 state)
        :integer state)))

(define-registered get-round-number
  (fn [state]
    (push-item
        (stack-ref :auxiliary 1 state)
        :integer state)))

(define-registered get-my-move-any
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (pop :integer state)
           game (if (and (> g 0) (< (mod g (stack-ref :auxiliary 2 state))
(stack-ref :auxiliary 2 state)))
                    (mod g (stack-ref :auxiliary 2 state))
                    0)
           r (pop :integer state)
           round (if (and (> r 0) (< (mod r 10) (stack-ref :auxiliary 1 state)))
                   (mod r 10)
                   0)
           me (stack-ref :auxiliary 3 state)]
          (if (or (= game 0) (= round 0))
              0
              (get-my-move
                data
                game
                round
                me)))
              :boolean state)
      state)))

(define-registered get-my-move
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (pop :integer state)
           game (if (and (> g 0) (< (mod g (stack-ref :auxiliary 2 state))
(stack-ref :auxiliary 2 state)))
                    (mod g (stack-ref :auxiliary 2 state))
                    0)
           round (stack-ref :auxiliary 1 state)
           me (stack-ref :auxiliary 3 state)]
          (if (= game 0)
              0
              (get-my-move
                data
                game
                round
                me)))
              :boolean state)
      state)))

(define-registered best-move-any
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (pop :integer state)
           game (if (and (> g 0) (< (mod g (stack-ref :auxiliary 2 state))
(stack-ref :auxiliary 2 state)))
                    (mod g (stack-ref :auxiliary 2 state))
                    0)
           r (pop :integer state)
           round (if (and (> r 0) (< (mod r 10) (stack-ref :auxiliary 1 state)))
                   (mod r 10)
                   0)
           me (stack-ref :auxiliary 3 state)]
          (if (or (= game 0) (= round 0))
              0
              (best-move
                data
                game
                round
                me)))
              :boolean state)
      state)))

(define-registered best-move
  (fn [state]
    (if
      (not (empty? (rest (:integer state))))
      (push-item
        (let
          [data (stack-ref :auxiliary 0 state)
           g (pop :integer state)
           game (if (and (> g 0) (< (mod g (stack-ref :auxiliary 2 state))
(stack-ref :auxiliary 2 state)))
                    (mod g (stack-ref :auxiliary 2 state))
                    0)
           round (stack-ref :auxiliary 1 state)
           me (stack-ref :auxiliary 3 state)]
          (if (= game 0)
              0
              (best-move
                data
                game
                round
                me)))
              :boolean state)
      state)))

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