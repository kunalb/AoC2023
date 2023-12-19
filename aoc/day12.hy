(import collections [deque Counter])
(import re)
(import functools [cache])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 12))
(setv sample-inputs (bzn.get-sample-inputs 2023 12))

(defn w [x]
  f"{x :5}")

(defn print-grid [ps cs grid]
  (setv sep " | ")
  (print (w "/") :end sep)
  (print (.join sep (map w ps)))

  (for [#(c g) (zip cs grid)]
    (print (w c) :end sep)
    (print (sep.join (map w g)))))

(defn check [p c]
  (cond
    (= p "?") True
    True (= p c)))

(defn lookup [grid row col]
  (cond
    (and (= 0 row) (= -1 col)) 1
    (= -1 row) 0
    (= -1 col) 0
    True (get grid row col)))

(defn count-matches [pattern constraints]
  (while True
    (setv pl (len pattern))
    (setv pattern (pattern.strip "."))
    (setv pattern (pattern.replace ".." "."))
    (when (= pl (len pattern)) (break)))

  (setv cs (list (+ "." (.join "." (gfor constraint constraints (* "#" constraint))) ".")))
  (setv ps (list (+ "." pattern ".")))

  (setv grid (lfor c cs (lfor p ps 0)))

  (for [#(row c) (enumerate cs)]
    (for [#(col p) (enumerate ps)]
      (setv val 0)

      (when (not (check p c))
        (continue))

      (cond
        (= c "#")
        (+= val
            (lookup grid (- row 1) (- col 1)))

        (= c ".")
        (+= val
            (lookup grid (- row 1) (- col 1))
            (lookup grid row (- col 1))))

      (setv (get grid row col) val)))

  ; (print-grid ps cs grid)
  (get grid -1 -1))


(defn transform [problem]
  (setv #(pattern constraints) problem)
  #((.join "?" (* 5 [pattern])) (* 5 constraints)))


(defn soln [puzzle-input [level 1]]
  (setv problems
        (lfor line (.split (puzzle-input.strip) "\n")
              (do
                (setv #(pattern constraints) (line.split))
                (setv constraints (list (map int (constraints.split ","))))
                #(pattern constraints))))

  (sum (gfor problem problems
             (do
               (when (= level 2)
                 (setv problem (transform problem)))
               ; (print #* problem)
               (setv result (count-matches #* problem))
               ; (print "-> " result)
               result))))


(defmain []
  (print
    (soln puzzle-input)
    (soln puzzle-input :level 2)))
