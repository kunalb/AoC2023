(import collections [deque Counter])
(import re)
(import functools [cache])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 12))
(setv sample-inputs (bzn.get-sample-inputs 2023 12))


(defn resolve [pattern constraints]
  (setv const (deque (map int (constraints.split ","))))
  (setv pattern (deque (filter None (pattern.split "."))))

  (while (all (gfor x (get pattern 0) (= x "#")))
    (when (!= (len (get pattern 0)) (get const 0))
      (return 0))
    (pattern.popleft)
    (const.popleft))

  (while (all (gfor x (get pattern -1) (= x "#")))
    (when (!= (len (get pattern -1)) (get const -1))
      (return 0))
    (pattern.pop)
    (const.pop))


  (setv cts (list const))
  (setv ptn (list pattern))
  (recurse ptn cts))


(defn recurse [ptn cts]
  (print (.join "." ptn) (.join "." (map (fn [x] (* x "#")) cts)))
  (setv options (** 2 (sum (gfor p ptn x p :if (= x "?") 1))))
  (print options)
  (print (setx av (sum (map len ptn) (- (len ptn) 1)))
         (setx us (+ (sum cts) (- (len cts) 1)))
         (- (len cts) 1)
         (- av us)
         (** (- (len cts) 1) (- av us)))


  (setv ptns (+ (.join "." ptn) "$"))
  (setv ctss (+ (.join "." (map (fn [x] (* x "#")) cts)) "$"))
  (setv pi 0)
  (setv ci 0)

  (setv seqs [])

  (while (and (< pi (len ptns))
              (< ci (len ctss)))

    (print seqs)

    (print (* " " pi) "v pi" :sep "")
    (print ptns)

    (print (* " " ci) "v ci" :sep "")
    (print ctss)
    (print)

    (setv p? 0)
    (while (and (< pi (len ptns))
                (= "?" (get ptns pi)))
      (+= p? 1)
      (+= pi 1))

    (setv pto-match "")
    (while (and (< pi (len ptns))
                (!= (get ptns pi) "?"))
      (+= pto-match (get ptns pi))
      (+= pi 1))

    (setv cskip "")
    (while (and (< ci (len ctss))
                (!= pto-match (cut ctss ci (+ ci (len pto-match)))))
      ; (print "Hunting for" pto-match)
      ; (print (cut ctss ci (+ ci (len pto-match))))

      (+= cskip (get ctss ci))
      (+= ci 1))

    (+= ci (len pto-match))

    (seqs.append #(p? cskip)))
  (print seqs)

  options)


(defn solve [pattern constraints]
  (setv new-pattern None)
  (setv orig-pattern pattern)
  (setv orig-constraints constraints)
  (setv cons (deque (map int (constraints.split ","))))

  (while (!= new-pattern pattern)
    (when (is-not new-pattern None)
      (setv pattern new-pattern))
    (setv new-pattern (pattern.strip "."))

    (when (and cons (new-pattern.startswith
                      (+ (* (get cons 0) "#") ".")))
      (setv new-pattern (cut new-pattern (get cons 0) None))
      (cons.popleft))


    (when (and cons (new-pattern.endswith
                      (+ "." (* (get cons -1) "#"))))
      (setv new-pattern (cut new-pattern
                             0 (- (len new-pattern) (get cons -1))))
      (cons.pop)))


  (when (!= orig-pattern pattern)
    (setv constraints
          (if cons (.join "," (map str cons)) "")))

  (setv result (solve-pattern pattern constraints))
  ;(print f">> |{orig-pattern}| |{orig-constraints}| |{pattern}| |{constraints}|"  result)
  result)


(defn [cache] solve-pattern [pattern constraints]
  (when (not constraints)
    (return (if (in "#" pattern) 0 1)))

  (when (not-in "?" pattern)
    (return
      (if (= (.join "," (map (fn [x] (str (len x)))
                             (filter None (pattern.split "."))))
             constraints)
          1 0)))

  (setv cons (list (map int (constraints.split ","))))
  (setv counthashes (sum (gfor x pattern :if (= x "#") 1)))
  (setv countopen (sum (gfor x pattern :if (= x "?") 1)))
  (setv countcons (sum cons))

  (when (or (< (len pattern) (+ countcons (len cons) -1))
            (< countcons counthashes)
            (< (+ counthashes countopen) countcons))
    (return 0))

  (setv groups (list (filter None (.split pattern "."))))
  (for [#(expressed constraint) (zip groups cons)]
    ; (print expressed constraint)
    (when (all (gfor x expressed (= x "?")))
      (break))
    (when (and (all (gfor x expressed (= x "#")))
               (!= (len expressed) constraint))
      (return 0)))

  (setv cur (Counter (map len (filter None (re.split r"[.?]" pattern)))))
  (when cur
    (setv con (Counter cons))
    (when (or
            (> (max (cur.keys)) (max (con.keys)))
            (> (cur.get (max (con.keys)) 0) (con.get (max (con.keys)))))
      (return 0)))

  ; (print "% " pattern constraints)
  (+ (solve (pattern.replace "?" "." 1) constraints)
     (solve (pattern.replace "?" "#" 1) constraints)))




(defn soln2 [puzzle-input [level 1]]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv result 0)
  (setv multiplier (if (= 1 level) 1 5))

  (for [line (cut lines 0 None)]
    (setv #(pattern nums) (line.split))

    (setv pattern5 (.join "?" (* multiplier [pattern])))
    (setv nums5 (.join "," (* multiplier [nums])))
    ; (print pattern5 nums5)
    (setv local (resolve pattern5 nums5))
    ; (print local)
    (+= result local))

  result)


(defn soln [puzzle-input]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv result 0)

  (for [line lines]
    (setv #(pattern nums) (line.split))
    (setv nums (list (map int (nums.split ","))))
    (setv posns
          (lfor #(i c) (enumerate pattern)
                :if (= c "?") i))

    (setv test-pattern
          (re.compile
            (+ r"^\.*" (.join r"\.+" (gfor n nums (+ "#{" (str n) "}"))) r"\.*$")))

    (setv options (** 2 (len posns)))
    (print options)
    (for [i (range options)]
      (setv example pattern)
      (for [#(jj pos) (enumerate posns)]
        (setv example (+ (cut bah 0 pos)
                     (if (& (** 2 jj) i) "." "#")
                     (cut example (+ pos 1) None))))
      ;(print example pattern test-pattern)
      (when (test-pattern.match example)
        ;(print "matched")
        (+= result 1)))
    #_ (print result "\n"))
  result)



(defmain []
  #_ (print puzzle-input)
  ; (print (recurse ["#" "#"] [1 1]))
  ; (print (soln2 (get sample-inputs 1) 2))
  (print (soln2 "?.?.?.?.? 1"))

  ; (print (solve.cache-info))

  #_ (print (soln2 puzzle-input 2))
  #_ (print (solve-pattern.cache-info)))
