(import re)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 1))

(setv labels {"one" 1 "two" 2 "three" 3 "four" 4 "five" 5
              "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn rev [s]
  (cut s None None -1))

(defn num [val]
  (if (in val labels)
      (get labels val)
      (int val)))

(defn make-regex [level [bwd False]]
  (re.compile
    (if (= 1 level) "[1-9]"
        (.join "|"
               ["[1-9]" #*
                (map (if bwd rev (fn [x] x)) (labels.keys)) ]))))

(defn search-result [regex line]
  (.group (regex.search line) 0))

(defn soln [puzzle-input [level 1]]
  (setv fwd-regex (make-regex level :bwd False)
        bwd-regex (make-regex level :bwd True))
  (sum
    (gfor
      line (.split (puzzle-input.strip))
      (+
        (* 10 (num (search-result fwd-regex line)))
        (num (rev (search-result bwd-regex (rev line))))))))

(defmain []
  (print
    (soln puzzle-input :level 1)
    (soln puzzle-input :level 2)))
