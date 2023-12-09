(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 9))

(defn soln [puzzle-input]

  (setv lines (.split (puzzle-input.strip) "\n")
        result 0
        result2 0)

  (for [line lines]
    (setv cur (list (map int (line.split))))
    (setv flag True)

    (while (any (gfor x cur (!= 0 x)))
      (+= result (get cur -1))
      (if flag
          (+= result2 (get cur 0))
          (-= result2 (get cur 0)))

      (setv cur
            (lfor #(a b) (zip cur (cut cur 1 None))
                  (- b a)))
      (setv flag (not flag))))

  #(result result2))


(defmain []
  (print #* (soln puzzle-input)))
