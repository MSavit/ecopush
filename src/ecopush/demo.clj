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

