(import itertools)
(import itertools [permutations])
(import math [factorial])
(import re)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 12))
(setv sample-inputs (bzn.get-sample-inputs 2023 12))


;; Still has a bug, shows up at input 1 < 30 cut size

(defn test-pattern [pattern value]
  (when (not (= (len pattern) (len value)))
    (return False))
  (all (gfor #(c1 c2) (zip pattern value)
             (cond
               (= c1 "?") True
               True (= c1 c2)))))


(defn trim-pattern [pattern constraints]
  (while (and constraints pattern)
    (setv prev-len (len pattern))

    (setv pattern (pattern.strip "."))

    (when (pattern.startswith "#")
      (setv first-constraint (get constraints 0))
      (when
        (not (.issubset (set (cut pattern 0 first-constraint)) (set "#?")))
        (return #("" [1])))
      (setv pattern (cut pattern first-constraint None))
      (when pattern
        (cond
          (= (get pattern 0) "?") (setv pattern (+ "." (cut pattern 1 None)))
          (= (get pattern 0) "#") (return #("" [1]))))
      (setv constraints (cut constraints 1 None)))

    (when (pattern.endswith "#")
      (setv last-constraint (get constraints -1))
      (when
        (not (.issubset (set (cut pattern (- last-constraint) None)) (set "#?")))
        (return #("" [1])))
      (setv pattern (cut pattern 0 (- last-constraint)))
      (when pattern
        (cond
          (= (get pattern -1) "?") (setv pattern (+ (cut pattern 0 -1) "."))
          (= (get pattern -1) "#") (return #("" [1]))))


      (setv constraints (cut constraints 0 -1)))
    (when (= prev-len (len pattern)) (break)))

  #(pattern constraints))


(defn count-? [pattern]
  (** 2 (sum (gfor c pattern :if (= "?" c) 1))))


(defn recursive-perms [value spots]
  (cond
    (= value 0) (do
                  (yield (* spots [0]))
                  (return))
    (= spots 1) (do
                  (yield [value])
                  (return)))

  (for [i (range 0 (+ 1 value))]
    (for [res (recursive-perms (- value i) (- spots 1))]
      (yield (+ [i] res)))))


(defn solve-enumerate [pattern constraints]
  ; (print "solve-enumerate:" pattern constraints)
  (setv buffer-len (- (len pattern)
                      (+ (sum constraints) (- (len constraints) 1))))
  (cond
    (< buffer-len 0) (return 0)
    (= buffer-len 0) (return 1))

  (setv perms (// (factorial (+ buffer-len (len constraints)))
                  (* (factorial buffer-len) (factorial (len constraints)))))
  ; (print ">> total perms " perms)

  #_ (when (> perms 1) (return -1))

  (setv matches 0)
  (for [posns (recursive-perms buffer-len (+ 1 (len constraints)))]
    (setv test-str (* "." (get posns 0)))
    (for [#(c p) (zip constraints (cut posns 1 None))]
      (+= test-str (* "#" c) ".")
      (+= test-str (* "." p)))
    (setv test-str (cut test-str 0 -1))
    ; (print test-str :end " ")
    (when (test-pattern pattern test-str)
      ; (print "T" :end "")
      (+= matches 1))
                                ;(print)
    )

  ; (print "solved:" pattern constraints matches)
  matches)


(defn recurse [pattern constraints]
  ; (print "Recurse: " pattern constraints (count-? pattern))
  (setv #(pattern constraints)
        (trim-pattern pattern constraints))

  ; (print "Trimmed: " pattern constraints (count-? pattern))

  (let [l (len pattern)
        sc (sum constraints)]
    (when (or (<= l sc) (= sc 0))
      (return (solve-enumerate pattern constraints))))

  (when (and (> (len pattern) 1)
             (= (get pattern -1) "?")
             (!= (get pattern -2) "?"))
    (return (+ (recurse (+ (cut pattern 0 -1) ".") constraints)
               (recurse (+ (cut pattern 0 -1) "#") constraints))))

  (+ (recurse (+ "." (cut pattern 1 None)) constraints)
     (recurse (+ "#" (cut pattern 1 None)) constraints)))


(defn log [x]
  (print "Result:" x)
  (print)
  x)

(defn count-solns [puzzle]
  (print puzzle)

  (setv #(pattern constraints) (puzzle.split))
  (setv constraints (list (map int (constraints.split ","))))

  (log (recurse pattern constraints)))


(defn solve [puzzle]
  (setv lines (.split (puzzle.strip) "\n"))
  (sum (gfor line lines
             (count-solns line))))


(defn transform [line]
  (setv #(pattern constraints) (line.split))
  (+ (.join "?" (* 5 [pattern])) " " (.join "," (* 5 [constraints]))))


(defn solve2 [puzzle]
  (setv lines (.split (puzzle.strip) "\n"))
  (sum (gfor line (cut lines 0 None)
             (count-solns (transform line)))))

(defmain []
  (print (solve2  puzzle-input #_ (get sample-inputs 1))))
