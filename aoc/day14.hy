(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 14))
(setv sample-inputs (bzn.get-sample-inputs 2023 14))


(defn transpose [grid]
  (for [c (range 0 (len (get grid 0)))]
    (yield (lfor r grid (get r c)))))


(defn soln [puzzle-input [level 1]]
  (setv grid (.split (puzzle-input.strip) "\n"))

  (setv result 0)
  (setv height (len grid))

  (setv rolls 0)
  (print grid)

  (while (< rolls 1)
    (for [#(dir1 dir2)
          (zip
            [(range 0 (len (get grid 0)))
             (range 0 height)]
            [(fn [] (enumerate grid))
             (fn [] (enumerate (transpose grid)))])]

      (for [col dir1]
        (setv base 0)
        (for [#(r row) (dir2)]
          (setv c (get row col))
          (cond
            (= "O" c) (do
                        (+= result (- height base))
                        (+= base 1))
            (= "#" c) (setv base (+ 1 r))))))

    (+= rolls 1))

  result)


(defmain []
  (print (soln (get sample-inputs 0))))
