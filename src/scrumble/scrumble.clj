(ns scrumble.scrumble)

; Width and heigh of the board
(def WIDTH 15)
(def HEIGHT 15)

; Data structure for letter, and all letters + point values
(defrecord Letter [char value])
(def A (Letter. \A 1))
(def E (Letter. \E 1))
(def I (Letter. \I 1))
(def L (Letter. \L 1))
(def N (Letter. \N 1))
(def O (Letter. \O 1))
(def R (Letter. \R 1))
(def S (Letter. \S 1))
(def T (Letter. \T 1))
(def U (Letter. \U 1))
(def D (Letter. \D 2))
(def G (Letter. \G 2))
(def M (Letter. \M 2))
(def B (Letter. \B 3))
(def C (Letter. \C 3))
(def P (Letter. \P 3))
(def F (Letter. \F 4))
(def V (Letter. \V 4))
(def H (Letter. \H 4))
(def J (Letter. \J 8))
(def Q (Letter. \Q 8))
(def K (Letter. \K 10))
(def W (Letter. \W 10))
(def X (Letter. \X 10))
(def Y (Letter. \Y 10))
(def Z (Letter. \Z 10))
(def JOKER (Letter. nil 0))

; A board.
; FIXME: use defrecord or something ?
; FIXME: do not use def
(def board {
            [0,0] :triple_word
            [7,0] :triple_word
            [14,0] :triple_word
            [0,7] :triple_word
            [0,14] :triple_word
            [14,7] :triple_word
            [14,14] :triple_word
            [1,1] :double_word
            [2,2] :double_word
            [3,3] :double_word
            [4,4] :double_word
            [7,7] :double_word
            [10,10] :double_word
            [11,11] :double_word
            [12,12] :double_word
            [13,13] :double_word
            [1,13] :double_word
            [2,12] :double_word
            [3,11] :double_word
            [4,10] :double_word
            [10,4] :double_word
            [11,3] :double_word
            [12,2] :double_word
            [13,1] :double_word
            [1,5] :triple_letter
            [5,5] :triple_letter
            [9,5] :triple_letter
            [13,5] :triple_letter
            [1,9] :triple_letter
            [5,9] :triple_letter
            [9,9] :triple_letter
            [13,9] :triple_letter
            [5,1] :triple_letter
            [9,1] :triple_letter
            [5,13] :triple_letter
            [9,13] :triple_letter
            [0,3] :double_letter
            [0,11] :double_letter
            [14,3] :double_letter
            [14,11] :double_letter
            [3,0] :double_letter
            [11,0] :double_letter
            [3,14] :double_letter
            [11,14] :double_letter
            [6,6] :double_letter
            [6,8] :double_letter
            [8,6] :double_letter
            [8,8] :double_letter
            [2,6] :double_letter
            [2,8] :double_letter
            [3,7] :double_letter
            [12,6] :double_letter
            [12,8] :double_letter
            [11,7] :double_letter
            [6,2] :double_letter
            [8,2] :double_letter
            [7,3] :double_letter
            [6,12] :double_letter
            [8,12] :double_letter
            [7,11] :double_letter
            })

; Already installed letters
; FIXME: do not use def
(def letters {
              [7 8] E
              [7 9] S
              [7 10] T
              [7 11] E
              [7 12] Z
              [8 8] S
              [9 8] S
              [10 8] A
              [11 8] I
              })

; New letters input by player
; FIXME: do not use def
(def new-letters {
                  [6 7] C
                  [7 7] U
                  [8 7] Q
                  })

; Inverse of nil function
(def not-nil? (complement nil?))

; FIXME: make private
(defn get-letter-multiplier
  "Return the times a letter's score must be multiplied"
  [pos] 
  (let [label (board pos)]
    (cond 
     (= label :triple_letter) 3
     (= label :double_letter) 2
     :else 1)))

; FIXME: make private
(defn get-word-multiplier
  "Return the times a word's score must be multiplied"
  [pos] 
  (let [label (board pos)]
    (cond 
     (= label :triple_word) 3
     (= label :double_word) 2
     :else 1)))

; FIXME: make private
(defn any-letter?
  "Return true if there is a letter (old or new) at pos"
  [pos]
  (if (letters pos)
    true
    (if (new-letters pos)
      true
      false)))

; FIXME: make private
(defn get-char-from-pos
  "Return the char of any (old or new) letter"
  [pos]
  (let [l (letters pos)]
    (if l
      (:char l)
      (let [l (new-letters pos)]
        (if l
          (:char l)
          nil)))))

; FIXME: make private
(defn get-value-from-pos
  "Return the value of any (old or new) letter, multiplied if needs be"
  [pos]
  (let [l (letters pos)]
    (if l
      (:value l)
      (let [l (new-letters pos)]
        (if l
          (* (:value l) (get-letter-multiplier pos))
          nil)))))

; FIXME: make private
(defn get-word-multiplier-from-pos
  "Return multiplier bonus only on new letter"
  [pos]
  (let [l (letters pos)]
    (if l
      1
      (get-word-multiplier pos))))

; FIXME: make private    
(defn letters-aligned?
  "Return :horizontal if letters are on same row, :vertical if on same column, :all if only one letter, nil else"
  ([letters] (cond
              (= (count letters) 0) (throw (Exception. "Error: no letters"))
              (= (count letters) 1) :all
              (letters-aligned? letters :horizontal) :horizontal
              (letters-aligned? letters :vertical) :vertical
              :else nil))
  ([letters alignment] (let [index (cond
                                    (= alignment :horizontal) second
                                    (= alignment :vertical) first
                                    :else (throw (Exception. "Error: wrong alignment")))]
                         (if (reduce #(if (= %1 %2) %1 false) (map #(index (first %1)) letters))
                           alignment
                           nil))))

; FIXME: make private
(defn word-next-letters
  "Only used by word-formed-by-letter; take a pos and return current string, current score and current
   multiplier factor"
  [pos direction] 
  (if (any-letter? pos)
    (let [x (first pos)
          y (second pos)
          prev-pos (cond 
                    (= direction :up) [x (- y 1)]
                    (= direction :down) [x (+ y 1)]
                    (= direction :left) [(- x 1) y]
                    (= direction :right) [(+ x 1) y]
                    :else (throw (Exception. "Error: wrong direction")))
          [word score mult] (word-next-letters prev-pos direction)
          new-word (cond
                    (or (= direction :up) (= direction :left)) (str word (get-char-from-pos pos))
                    (or (= direction :down) (= direction :right)) (str (get-char-from-pos pos) word)
                    :else (throw (Exception. "Error: wrong direction")))]
      [new-word 
       (+ score (get-value-from-pos pos))
       (* mult (get-word-multiplier-from-pos pos))])
    ["" 0 1]))

; FIXME: make private                                    
(defn word-formed-by-letter
  "Return the new word, in a direction, formed by a letter"
  [letter alignment] 
  (let [x (first (first letter))
        y (second (first letter))
        [prev-pos next-pos prev-direction next-direction] (cond
                                                           (= alignment :vertical) [[x (- y 1)] [x (+ y 1)] :up :down]
                                                           (= alignment :horizontal) [[(- x 1) y] [(+ x 1) y] :left :right]
                                                           :else (throw (Exception. "Error: alignment must be vertical or horizontal")))
        [word-prev score-prev mult-prev] (word-next-letters prev-pos prev-direction)
        [word-next score-next mult-next] (word-next-letters next-pos next-direction)
        lmult (get-letter-multiplier [x y])
        wmult (get-word-multiplier [x y])]
    (if (and (= "" word-prev) (= "" word-next))
      nil ;; a one-letter-word is not a word, nope
      (let [return
            [(str word-prev (:char (second letter)) word-next) 
             (* wmult mult-prev mult-next (+ score-prev score-next (* lmult (:value (second letter)))))]]
        return))))

(defn words-formed-by-letters
  "Return all words formed by new letters"
  [letters]
  (let [alignment (letters-aligned? letters)]
    (cond
     (= alignment nil) (throw (Exception. "Error: letters not aligned or no letters"))
     (= alignment :all) (filter not-nil? (vector(word-formed-by-letter (first letters) :horizontal) 
                                                (word-formed-by-letter (first letters) :vertical)))
     (= alignment :horizontal) (filter not-nil? (apply vector 
                                                       (word-formed-by-letter (first letters) :horizontal) 
                                                       (map #(word-formed-by-letter %1 :vertical) letters)))
     (= alignment :vertical) (filter not-nil? (apply vector 
                                                     (word-formed-by-letter (first letters) :vertical) 
                                                     (map #(word-formed-by-letter %1 :horizontal) letters))))))

;; (defn display-case-html
;;   "Display HTML code for the board's case"
;;   [x y]
;;   (let [letter (letters [x y])
;;         z (board [x y])
;;         background (cond 
;;                     (nil? z) ["green" "_"]
;;                     (= z :triple_word) ["red" " "]
;;                     (= z :double_word) ["fuschsia" " "]
;;                     (= z :triple_letter) ["blue" " "]
;;                     (= z :double_letter) ["aqua" " "])]
;;     [:td {:id (str x "-" y) :bgcolor (first background)} 
;;      (if (nil? letter)
;;        (second background)
;;        (str (:char letter)":" (:value letter)))]))

; FIXME: move to another namespace
(defn display-case-ascii
  "Display ASCII for the board's case"
  [x y]
  (let [letter (letters [x y])
        z (board [x y])
        background (condp = z 
                    nil? " "
                    :triple_word "*"
                    :double_word "."
                    :triple_letter "-"
                    :double_letter "_"
                    " ")]
    (str (if (nil? letter)
           background
           (:char letter))
         "|")))


(defn display-board-ascii
  "Display the boards (and letter on it) in ASCII 'art'"
  []
  (println (str
            "New words formed : \n"
            (reduce str (map #(str (first %) ":" (second %) "\n") (words-formed-by-letters new-letters)))
            "\n"))
  (println "board:")
  (doall 
   (doseq [y (range 0 HEIGHT)]
     (print "|")
     (doseq [x (range 0 WIDTH)]
       (print (str (display-case-ascii x y))))
     (print "\n"))))


;; (defn index-page []

;;   (html5
;;     [:head
;;       [:title "Hello World"]]
;; ;      (include-js "/js/main.js")]
;;     [:body
;;      [:h1 (str "Hello World! " (swap! my-count inc))]
;;      [:p (map #(let [[word score] %1] (str word ":" score";"))
;;       (words-formed-by-letters new-letters))]
;;      [:table {:border "solid black line"}
;;       (for [y (range 0 HEIGHT)]
;;                [:tr (for [x (range 0 WIDTH)]
;;                       (display-case x y))])]]))
