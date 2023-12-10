(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 10))
(setv sample-inputs (bzn.get-sample-inputs 2023 10))

(defn soln [puzzle-input]
  (setv grid
        (.split (puzzle-input.strip) "\n"))
  (setv dist [])

  (setv front #{})
  (for [#(row vals) (enumerate grid)]
    (dist.append (* [0] (len vals)))
    (for [#(col vals2) (enumerate vals)]
      (when (= vals2 "S")
        (setv (get dist row col) 0)
        (front.add #(row col)))))

  (setv max-rows (len grid))
  (setv max-cols (len (get grid 0)))
  (setv visited #{})

  (while front
    (setv next-front #{})

    (for [#(row col) front]
      (when (in #(row col) visited)
        (continue))
      (visited.add #(row col))

      (setv cur (get grid row col))
      (setv cur-dist (get dist row col))

      (assert (!= cur-dist "x") #(col row))

      (when (and (> row 0) (in cur "S|LJ"))
        (setv nx #((- row 1) col))
        (setv above (get grid #* nx))
        (when (and (in above "F7|") (not-in nx visited))
          (setv (get dist (- row 1) col) (+ 1 cur-dist))
          (next-front.add nx)))

      (when (and (> max-rows row) (in cur "SF7|"))
        (setv nx #((+ row 1) col))
        (setv below (get grid #* nx))
        (when (and (in below "LJ|") (not-in nx visited))
          (setv (get dist (+ row 1) col) (+ 1 cur-dist))
          (next-front.add #((+ row 1) col))))

      (when (and (> col 0) (in cur "S-7J"))
        (setv nx #(row (- col 1)))
        (setv left (get grid #* nx))
        (when (and (in left "-LF") (not-in nx visited))
          (setv (get dist row  (- col 1)) (+ 1 cur-dist))
          (next-front.add #(row (- col 1)))))

      (when (and (> max-cols col) (in cur "S-LF"))
        (setv nx #(row (+ col 1)))
        (setv right (get grid #* nx))
        (when (and (in right "-7J") (not-in nx visited))
          (setv (get dist row (+ col 1)) (+ 1 cur-dist))
          (next-front.add #(row (+ col 1))))))

    (setv front next-front))

  ; (print puzzle-input)
  ; (print (.join "\n" (gfor l dist (str l))))
  (max (gfor x dist (max x))))


(defn getc [grid pt]
  (try
    (get grid (int pt.real) (int pt.imag))
    (except []
      ".")))

(defn in-range? [grid pt]
  (setv max-real (len grid))
  (setv max-imag (len (get grid 0)))
  (setv r (int pt.real))
  (setv i (int pt.imag))

  (and
    (>= r 0) (>= i 0)
    (> max-real r) (> max-imag i)))


(defn debug-grid [grid pts [sym "*"]]
  (for [#(real row) (enumerate grid)]
    (for [#(imag col) (enumerate row)]
      (if (in (complex real imag) pts)
          (print sym :end "")
          (print col :end "")))
    (print)))


; Hunt clockwise
(setv to-dirs {"|" [ -1 1 ]
               "-" [ 1j -1j ]
               "L" [ -1 1j ]
               "J" [ -j -1 ]
               "7" [ 1 -j ]
               "F" [ 1j 1 ]
               "." []
               "S" [ -1 1j 1 -1j ]})


(defn soln2 [puzzle-input]
  (setv grid
        (.split (puzzle-input.strip) "\n"))
  (setv max-rows (len grid))
  (setv max-cols (len (get grid 0)))

  (setv colors [])

  (setv start None)
  (for [#(row vals) (enumerate grid)]
    (colors.append (* [0] max-cols))
    (for [#(col vals2) (enumerate vals)]
      (when (= vals2 "S")
        (setv start (complex row col)))))

  (setv visited #{})

  (setv cur start)
  (setv d None)
  (setv counter 0)

  (setv left #{})
  (setv right #{})

  (setv pipe #{})
  (while True
    (pipe.add cur)
    (setv cur-g (getc grid cur))

    (for [d (get to-dirs cur-g)]
      (setv nex (+ cur d))
      (when (and (in nex pipe) (!= start nex))
          (continue))

      (setv nex-g (getc grid nex))

      (when (in (* -1 d) (get to-dirs nex-g))
        (setv cur nex)
        (break)))

    (when (= start cur)
      (break)))

  (debug-grid grid pipe "P")

  (setv cur start)
  (while True
    (visited.add cur)
    ; (print "->" cur)

    (setv prev-cur cur)
    (setv cur-g (getc grid cur))
    ; (print d cur cur-g)

    (for [d (get to-dirs cur-g)]
      (setv nex (+ cur d))
      (when (and (in nex visited) (!= start nex))
          (continue))

      (setv nex-g (getc grid nex))
      ; (print "Testing" d nex nex-g )

      (when (in (* -1 d) (get to-dirs nex-g))
        ; color the grid according to d
        (setv right-dir (* 1j d))
        (setv color-pt (+ right-dir cur))
        ; (print "right" cur d right-dir color-pt)
        (setv next-right #{})
        (for [color-pt #( (+ right-dir cur) (+ right-dir nex) )]
          (while (and (in-range? grid color-pt)
                      (not-in color-pt pipe))
            (next-right.add color-pt)
            (+= color-pt right-dir)))
        (setv right (| right next-right))
        ; (print "Right")
        ; (debug-grid grid right "R")

        (setv left-dir (* -1j d))
        ; (print "left" cur d left-dir color-pt)
        (setv color-pt (+ left-dir cur))
        (setv next-left #{})
        (for [color-pt #( (+ left-dir cur) (+ left-dir nex))]
          (while (and (in-range? grid color-pt)
                      (not-in color-pt pipe))
            (next-left.add color-pt)
            (+= color-pt left-dir)))
        (setv left (| left next-left))

        ; (print "Left")
        ; (debug-grid grid left "L")

        ; (print "Soln")
        ; (debug-grid grid (- left right visited))

        (setv cur nex)
        (break)))

    (when (= start cur)
      (break))
    (+= counter 1))


  (debug-grid grid (-  right left visited))
  (print (- right left visited))
  (len (- right left visited)))


(defmain []
  #_ (print (soln2 (get sample-inputs 9))) ; 9
  (print (soln2 puzzle-input)))
