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