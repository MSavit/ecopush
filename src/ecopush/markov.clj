;; Experimental Markov Chain implementation in Clojure
;; Luke VanderHart, Dec 12, 2008

;;;;;; File I/O

(def word-break-chars #{\newline \space}) ; word breaks with no semantic import
(def semantic-punct-chars #{ \. \? \! \: \,}) ; word breaks that need to be preserved as "words"

(defn word-seq [reader]
  "gets a lazy seq of words from a file, closes file when complete"
  (let [helper (fn helper [word]
		 (let [ch (.read reader)]
		   (cond
		    (= ch -1) (.close reader)
		    (contains? semantic-punct-chars (char ch)) (lazy-seq word (helper (char ch)))
		    (contains? word-break-chars (char ch)) (lazy-seq word (helper ""))
		    :else (recur (str word (char ch))))))]
    (helper "")))
	

(defn char-seq [reader]
  "gets a lazy seq of characters from a file, closes file when complete"
  (let [ch (.read reader)]
    (if (= ch -1)
      (.close reader)
      (lazy-seq (char ch) (char-seq reader)))))

(defn piece-file [fname piece-function]
  "returns a file as a lazy seq of pieces, using the supplied piecing function"
  (piece-function (java.io.BufferedReader. (java.io.FileReader. fname))))
       
;;;;;; Indexing

(defn update-index-entry [old-entry completion]
  "updates the possible completions for an index entry"
  (let [existing (old-entry completion)]
      (assoc old-entry completion (if (nil? existing)
				    1
				    (inc existing)))))
   
(defn update-index [index key completion]
  "updates the index with a key and completion possibility"
  (let [old-entry (index key)]
    (assoc index key (if (nil? old-entry)
		       (hash-map completion 1)
		       (update-index-entry old-entry completion)))))

(defn build-index [full-corpus key-length completion-length]
  "builds an index from a corpus (a seq)"
  (loop [index (hash-map) corpus full-corpus]
    (let [key (take key-length corpus)
	 completion (take completion-length (nthrest corpus key-length))]
      (cond
       (nil? corpus) index
       (nil? key) (recur index (nthrest corpus completion-length)) ;skip if it didn't find a key
       :else (recur (update-index index key completion) (nthrest corpus completion-length))))))

;;;;;; Chain generation

(defn markov-next [index key]
  "Primary markov function. Given an index and a key, picks a statistically consistent completion"
  (let [completions (index key)
	probabilities (vals completions)
	total-probabilities (reduce + probabilities)
	random (inc (rand-int total-probabilities))]
    (if (nil? completions)
      nil
      (loop [current 0 remaining (seq completions)]
	(let [next (+ current (last (first remaining)))]
	    (if (<= current random next)
	      (ffirst remaining)
	      (recur next (rest remaining))))))))

(defn markov-chain [index key]
  "Lazily creates a markov chain"
    (let [completion (markov-next index key)
	  next-key (nthrest (concat key completion) (count completion))]
      (if (nil? completion)
	'()
	(lazy-seq (first completion) (markov-chain index next-key)))))

(defn get-random-key [index]
  "Determines a random key from the supplied index with which to start generation"
  (let [random (inc (rand-int (dec (count index))))]
    (nth (keys index) random)))

;;;;;; Execution

(def key-size 2)
(def prediction-size 1)

(def result-length 300)

;; You may use either the char-based or word-based version

;;(def file (piece-file "/home/luke/dev/projects/markov/corpus/short.txt" char-seq))
(def file (piece-file "/home/luke/dev/projects/markov/corpus/pride-and-prejudice.txt" word-seq))

(def index (build-index file key-size prediction-size))

(def chain (markov-chain index (get-random-key index)))

;; This is suited to a char-based chain
(comment
(do
  (println "-------------------------------------------")
  (println (apply str (take result-length chain)))
  (println "-------------------------------------------"))
)

;; This is suited to a word-based chain
(do
  (println "-------------------------------------------")
  (println (apply str (interpose \space (take result-length chain))))
  (println "-------------------------------------------"))

;;;;;;; Run whole file

(comment
  (load-file "/home/luke/dev/projects/markov/markov.clj")
)

