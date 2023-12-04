(import parsy :as p)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 4))


(defn [p.generate] input-line []
  (setv num (.map (p.regex r"[0-9]+") int))
  (setv numlist (num.sep_by p.whitespace))

  (yield (>> (p.string "Card") p.whitespace))
  (setv card-index (yield num))
  (yield (>> (p.string ":") p.whitespace))
  (setv winners (yield numlist))
  (yield (>> p.whitespace (p.string "|") p.whitespace))
  (setv my-numbers (yield numlist))

  #(card-index (set winners) (set my-numbers)))


(defn soln [puzzle-input]
  (setv points 0
        cards 0
        multipliers {})

  (for [line (.split (puzzle-input.strip) "\n")]
    (setv #(card-index winners my-numbers)
          (input-line.parse line))
    (when (setx common (len (& winners my-numbers)))
      (+= points (** 2 (- common 1))))

    (setv card-copies (+ 1 (multipliers.pop card-index 0)))
    (+= cards card-copies)

    (for [i (range common)]
      (setv index (+ i 1 card-index))
      (setv (get multipliers index)
            (+ card-copies (multipliers.get index 0)))))

  #(points cards))

(defmain []
  (print #* (soln puzzle-input)))
