;;; Ecopush Project Definition for use with Lein
(defproject ecopush "1.0.0-SNAPSHOT"
  :description "Robust Market Entry Game Players using Genetic Programming"
  :url "https://github.com/fconcklin/ecopush"
  :license {:name "GNU General Public License (GPL)"
	    :url "http://gnus.org/licenses/gpl.html"
	    :distribution :repo
	    :comments "License same as Clojush"}
  :mailing-list {:name "Ecopush mailing list"
		 :post "ecopush@googlegroups.com"}
  :min-lein-version "1.5.0"
  :dependencies [[org.clojure/clojure "1.2.0"]
		 [org.clojure/clojure-contrib "1.2.0"]]
  :dev-dependencies [[swank-clojure "1.2.1"]
		     [marginalia "0.5.0"]
		     [lein-clojars "0.6.0"]]
  :main ecopush.core)
