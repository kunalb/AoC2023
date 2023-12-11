(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 11))
(setv sample-inputs (bzn.get-sample-inputs 2023 11))


(defn soln [puzzle-input]
  (setv grid
        (lfor line (.split (puzzle-input.strip) "\n") (list line)))

  (setv x-inserts
        (lfor i (range (len (get grid 0)))
              :if (all (gfor line grid (= "." (get line i))))
              i))

  (setv y-inserts
        (lfor #(y line) (enumerate grid)
          :if (all (gfor x line (= x ".")))
          y))

  (setv galaxies [])
  (for [#(y line) (enumerate grid)]
    (for [#(x pt) (enumerate line)]
      (when (= pt "#")
        (galaxies.append #(x y)))))

  (setv result 0)
  (setv result2 0)

  (for [i (range (- (len galaxies) 1))
        j (range (+ i 1) (len galaxies))]
    (setv #(x1 y1) (get galaxies i)
          #(x2 y2) (get galaxies j)
          dist (+ (abs (- x2 x1)) (abs (- y2 y1))))

    (setv x-expansions
          (sum (gfor x x-inserts
                     :if (> (* (- x x1) (- x2 x)) 0) 1)))
    (setv y-expansions
          (sum (gfor y y-inserts
                     :if (> (* (- y y1) (- y2 y)) 0) 1)))

    (+= result dist x-expansions y-expansions)
    (+= result2 dist (* (- 1000000 1) (+ x-expansions y-expansions))))

  #(result result2))


(defmain []
  (print #* (soln puzzle-input)))
