(ns scrumble.main
  (:require [scrumble.scrumble :as s]))

(defn -main 
  [& args]
  (println "Scrumble!")
  (s/display-board-ascii))