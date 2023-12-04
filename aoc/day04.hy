(import collections [defaultdict])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 4))

(defn soln [puzzle-input]
  (sum
    (gfor line (.split (puzzle-input.strip) "\n")
          (do
            (setv #(card-name numbers) (line.split ":"))
            (setv #(winners my-numbers)
                  (tuple (map (fn [x] (list (map int (x.split))))
                             (numbers.split "|"))))
            (setv common (len (& (set winners) (set my-numbers))))
            (if (> common 0)
                (** 2 (- common 1)) 0)))))

(defn soln2 [puzzle-input]
  (setv card-counts {})

  (for [line (.split (puzzle-input.strip) "\n")]
    (do
      (setv #(card-name numbers) (line.split ":"))
      (setv card-index (int (get (card-name.split) 1)))
      (setv #(winners my-numbers)
            (tuple (map (fn [x] (list (map int (x.split))))
                        (numbers.split "|"))))
      (setv card-winners (len (& (set winners) (set my-numbers))))

      (setv
        (get card-counts card-index)
        (+ (card-counts.get card-index 0) 1))
      (for [x (range (+ 1 card-index) (+ card-winners card-index 1))]
        (setv
          (get card-counts x)
          (+ (card-counts.get x 0) (get card-counts card-index))))))
  (sum (card-counts.values)))


(defmain []
  (print (soln puzzle-input)
         (soln2 puzzle-input)))
