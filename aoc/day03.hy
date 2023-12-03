(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 3))

(defn soln [puzzle-input]
  (setv numbers [])
  (setv parts (set))
  (setv gears (set))
  (setv number-positions {})

  (setv num-index 0)
  (setv x 0)
  (for [line (.split (puzzle-input.strip) "\n")]
    (setv number-active? False)

    (setv y 0)
    (for [ch line]
      (cond
        (= ch ".") :
        (not (ch.isdigit)) (parts.add #(x y)))

      (when (= ch "*")
        (gears.add #(x y)))

      (if (ch.isdigit)
          (do
            (when (not number-active?)
              (setv number-active? True)
              (numbers.append 0))
            (setv (get number-positions #(x y)) (- (len numbers) 1))
            (setv new-val (+ (* 10 (get numbers -1)) (int ch)))
            (setv (get numbers -1) new-val))
          (setv number-active? False))

      (+= y 1))
    (+= x 1))

  ; (print parts)
  ; (print number-positions)
  ; (print numbers)

  (setv result 0)
  (setv gear-ratios 0)

  ; (print gears)

  (for [#(x y) gears]
    (setv adj-nums (set))
    (for [dx [-1 0 1]
          dy [-1 0 1]]
      (try
        (adj-nums.add (get number-positions #((+ x dx) (+ y dy))))
        (except [KeyError] :)))
    (when (= (len adj-nums) 2)
      (+= gear-ratios (* #* (map (fn [n] (get numbers n)) adj-nums)))))

  (for [#(x y) parts]
    (for [dx [-1 0 1]
          dy [-1 0 1]]
      (try
        (setv num-at-pos (get number-positions
                              #((+ x dx) (+ y dy))))
        (+= result (get numbers num-at-pos))
        (setv (get numbers num-at-pos) 0)
        (except [KeyError] : ))))
  #(result gear-ratios))


(defmain []
  (let [ans (soln puzzle-input)]
    (print #* ans)))
