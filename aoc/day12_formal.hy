(import functools)
(import logging)
(import math [factorial])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 12))
(setv sample-inputs (bzn.get-sample-inputs 2023 12))

(setv *TESTING* False)

(setv IMPOSSIBLE #("" [1]))

;; Utilities
(setv logger (logging.getLogger "day12"))


(defn info [#* args #** kwargs]
  (when *TESTING* (return (get args 0)))

  (logger.info #* args #** kwargs)
  (get args 0))


(defn debug [#* args #** kwargs]
  (when (or *TESTING* (not (logger.isEnabledFor logging.DEBUG)))
    (return (get args 0)))

  (logger.debug #* args #** kwargs)
  (get args 0))


(defn debug-fn [f]
  (defn [(functools.wraps f)] wrapper [#* args #** kwargs]
    (when (and (not *TESTING*) (logger.isEnabledFor logging.DEBUG))
      (debug f"{f.__name__} START: {args} {kwargs}"))
    (setv result (f #* args #** kwargs))
    (when (and (not *TESTING*) (logger.isEnabledFor logging.DEBUG))
      (debug f"{f.__name__} END -> {result}"))
    result)

  wrapper)

;; Input handling
(defn parse-line [line]
  (setv #(pattern constraints) (line.split))
  (setv constraints (list (map int (constraints.split ","))))
  #(pattern constraints))


(defn level2 [pattern constraints]
  (setv pattern2 (.join "?" (* 5 [pattern])))
  (setv constraints2 (* 5 constraints))
  #(pattern2 constraints2))


;; Boilerplate
(defn solve [puzzle-input [level 1]]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv parsed (lfor line lines (parse-line line)))
  (when (= level 2)
    (setv parsed (lfor p parsed (level2 #* p))))

  (sum (gfor p parsed
             (do
               (setv result (count-possibilities #* p))
               (debug "** %d ** \n" result)
               result))))


;; Actual solution

(defn print-grid [pieces constraints grid]
  ; (setv col-width (+ 1 (max (map len pieces))))
  (print (.join " | " (map (fn [c] f"{c :10}")
                           (+ ["xxx"] pieces))))
  (for [#(c line) (zip constraints grid)]
    (print f"{c :10}" :end " | ")
    (print (.join " | " (map (fn [x] f"{x :10}") line)))))

(defn count-matches [piece constraint]
  (cond
    (= (set piece) #{ "?" })
    (max 0 (+ (- (len piece) constraint) 1))

    (= (set piece) #{ "#" })
    (if (= (len piece) constraint) 1 0)

    (piece.startswith "?")
    (+ (count-matches (cut piece 1 None) constraint)
       (count-matches (+ "#" (cut piece 1 None)) constraint))

    (piece.endswith "?")
    (+ (count-matches (cut piece 0 -1) constraint)
       (count-matches (+ (cut piece 0 -1) "#") constraint))

    True
    (count-matches (piece.replace "?" "#") constraint)))

(defn edit-distance-matching [pattern constraints]
  ; run after simplify only
  (setv pieces (pattern.split "."))
  (setv grid (lfor _x (range 0 (len constraints))
                   (* [0] (len pieces))))
  (for [row (range 0 (len constraints))]
    (for [col (range 0 (len pieces))]
      (setv (get grid row col)
            (try
              (count-matches (get pieces col) (get constraints row))
              (except [Exception]
                (+ (get pieces col)
                   "-"
                   (str (get constraints row))))))))

  (print-grid pieces constraints grid)

  (setv options 0)
  (for [col (range 0 (+ 1 (- (len pieces) (len constraints))))]
    (setv diagonal 1)
    (for [row (range 0 (len constraints))]
      (*= diagonal (get grid row (+ col row)))
      (when (= diagonal 0) (break)))
    (+= options diagonal))

  (print options)
  options)


(defn table-matching [pattern constraints]
  ; run after simplify only
  (setv pieces (list pattern))
  (setv grid (lfor _x (range 0 (sum constraints))
                   (* [0] (len pieces))))
  (for [row (range 0 (len constraints))]
    (for [col (range 0 (len pieces))]
      (setv (get grid row col)
            (try
              (count-matches (get pieces col) (get constraints row))
              (except [Exception]
                (+ (get pieces col)
                   "-"
                   (str (get constraints row))))))))

  (print-grid pieces constraints grid)

  ; (setv options 0)
  ; (for [col (range 0 (+ 1 (- (len pieces) (len constraints))))]
  ;   (setv diagonal 1)
  ;   (for [row (range 0 (len constraints))]
  ;     (*= diagonal (get grid row (+ col row)))
  ;     (when (= diagonal 0) (break)))
  ;   (+= options diagonal))
  ; (print options)
  options)


(defn count-permutations [pattern constraints]
  (setv pattern-len (len pattern))
  (setv floating (- pattern-len
                    (sum constraints)
                    (- (len constraints) 1)))
  (setv constraints-len (len constraints))
  (if (>= floating 0)
      (// (factorial (+ floating constraints-len))
          (* (factorial floating) (factorial constraints-len)))
      0))


(defn log-counts [label pattern constraints]
  (when (not (logger.isEnabledFor logging.DEBUG))
    (return))

  (setv ?s (sum (gfor c pattern :if (= c "?") 1)))
  (debug "%s: %5d %5d / %s %s"
         label
         (** 2 ?s)
         (count-permutations pattern constraints)
         pattern
         constraints))


(defn simplify-edges [pattern constraints]
  (when (and constraints (pattern.startswith "#"))
    (setv constraint (constraints.pop 0))
    (setv remove-prefix (cut pattern 0 constraint))
    (when
      (or
        (!= (len remove-prefix) constraint)
        (not (.issubset (set remove-prefix) (set "#?"))))
      (return IMPOSSIBLE))
    (setv pattern (cut pattern constraint None))

    (when (pattern.startswith "#")
      (return IMPOSSIBLE))
    (setv pattern (+ "." (cut pattern 1 None))))

  (when (and constraints (pattern.endswith "#"))
    (setv constraint (constraints.pop -1))
    (setv remove-suffix (cut pattern (- constraint) None))
    (when
      (or
        (!= (len remove-suffix) constraint)
        (not (.issubset (set remove-suffix) (set "#?"))))
      (return IMPOSSIBLE))
    (setv pattern (cut pattern 0 (- constraint)))

    (when (pattern.endswith "#")
      (return IMPOSSIBLE))
    (setv pattern (+ (cut pattern 0 -1) ".")))

  #(pattern constraints))


(defn [debug-fn] simplify [pattern constraints]
  (while True
    (setv original-size (len pattern))
    (setv pattern (pattern.strip "."))
    (setv pattern (pattern.replace ".." "."))

    (setv #(pattern constraints) (simplify-edges pattern constraints))

    (when (= (len pattern) original-size) (break))
    (log-counts "S*" pattern constraints))
  #(pattern constraints))


(defn test-only-permutation [pattern constraints]
  (setv test-string
        (.join "." (map (fn [x] (* "#" x)) constraints)))
  (assert (= (len test-string) (len pattern)) "Test only permutation called incorrectedly")

  (all (gfor #(c1 c2) (zip test-string pattern)
             (cond
               (= c2 "?") True
               True (= c1 c2)))))


(defn [debug-fn] count-possibilities [pattern constraints [depth 0]]
  (log-counts "S0" pattern constraints)
  (setv #(pattern constraints) (simplify pattern (list constraints)))

  (return (table-matching pattern constraints))

  (setv pattern-set (set pattern))
  (setv count-perms (count-permutations pattern constraints))

  (cond
    (not constraints)
    (do
      (setv result (if (in "#" pattern-set) 0 1))
      (debug "C+: No constraints left!"))

    (= count-perms 1)
    (do
      (setv result (test-only-permutation pattern constraints))
      (debug "C+: Only one possible permutation to test"))

    (= pattern-set (set "?"))
    (do
      (setv result count-perms)
      (debug "C+: All ? detected"))

    (pattern-set.issubset (set "#."))
    (do
      (setv result (if (= constraints
                          (list (map len (filter None (pattern.split ".")))))
                       1 0))
      (debug "C+: No ? detected"))

    (> (+ (sum constraints) (len constraints) -1)
       (len pattern))
    (do
      (setv result 0)
      (debug "C+: Constraints too large for pattern"))

    (> (sum constraints)
       (sum (gfor c pattern :if (in c #{"#" "?"}) 1)))
    (do
      (setv result 0)
      (debug "C+: Can't fit #s into pattern"))


    (and (> (len pattern) 1)
         (!= (get pattern -2) "?"))
    (do
      (debug "R+: Recursing from the back")
      (setv result (+
                     (count-possibilities (+ (cut pattern 0 -1) ".") constraints :depth (+ 1 depth))
                     (count-possibilities (+ (cut pattern 0 -1) "#") constraints :depth (+ 1 depth)))))

    True
    (do
      (debug "R+: Recursing from the front")
      (setv result (+
                     (count-possibilities (+ "." (cut pattern 1 None)) constraints :depth (+ 1 depth))
                     (count-possibilities (+ "#" (cut pattern 1 None)) constraints :depth (+ 1 depth)))))

    True
    (do
      (setv result 0)
      (debug "R+: NOT HANDLED YET %s %s" pattern constraints)))

  result)


(defn run-tests []
  (global *TESTING*)
  (setv *TESTING* True)

  ; (assert (= (edit-distance-matching "#.?##?.?.??.??" [1 3 1 1]) 4))

  (assert (= (simplify ".##?????" [8]) IMPOSSIBLE))
  (assert (= (simplify "...?..." [1]) #("?" [1])))
  (assert (= (parse-line "##..# 1,2") #("##..#" [1 2])))
  (assert (= (level2 "#." [1]) #("#.?#.?#.?#.?#." [1 1 1 1 1])))

  (setv *TESTING* False))


(defmain [#* args]
  (when args
    (logging.basicConfig :level logging.DEBUG)
    (run-tests))
  (print (solve #_ puzzle-input (get sample-inputs 1) :level 1)))
