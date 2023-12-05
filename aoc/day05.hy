(import collections [defaultdict])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 5))


(defn soln [puzzle-input]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv seeds (list (map int (.split (cut (get lines 0) 7 None)))))

  (setv conversion (defaultdict dict))
  (setv i 2 l (len lines))

  (setv curset seeds)

  (while (< i l)
    (setv line (get lines i))
    (setv #(from to) (.split (get (line.split) 0) "-to-"))
    (+= i 1)

    (setv options [])
    (while (and (< i l) (setx rangeline (.strip (get lines i))))
      (options.append (list (map int (rangeline.split))))
      (+= i 1))

    (options.sort :key (fn [x] (get x 0)))
    (setv (get conversion from to) options)

    ; (print line)
    (setv nextset [])
    (for [x curset]
      ; (print "Looking for " x)
      (setv found None)
      ; (print options)
      (for [y options]
        (setv start (get y 1))
        (setv end (+ start (get y 2)))

        (when (and (>= x start) (<= x end))
          (setv found (+ (- x start) (get y 0)))))
      (when (is found None)
        (setv found x))
      ; (print "Found " found)
      (nextset.append found))
    (setv curset nextset)
    ; (print nextset)

    (+= i 1))

  (min curset))


(defn soln2 [puzzle-input]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv seeds (list (map int (.split (cut (get lines 0) 7 None)))))

  (setv conversion (defaultdict dict))
  (setv i 2 l (len lines))

  (setv curset seeds)

  (while (< i l)
    (setv line (get lines i))
    ; (print line)
    (+= i 1)

    (setv options [])
    (while (and (< i l) (setx rangeline (.strip (get lines i))))
      (options.append (list (map int (rangeline.split))))
      (+= i 1))

    (options.sort :key (fn [x] (get x 1)))
    ; (print options)
    ; (print curset)

    (setv nextset [])

    (setv pairs (lfor i (range 0 (len curset) 2)
                      #((get curset i) (get curset (+ i 1)))))
    (for [#(s1 l1) pairs]
      (setv e1 (+ s1 l1))  ; exclusive
      (setv pieces [#(s1 e1)])

      (for [y options]
        (setv start (get y 1))
        (setv end (+ start (get y 2)))
        ; (print start end)
        ; (print pieces)

        (setv nextpieces [])
        (for [#(ps pe) pieces]
          (setv overlap #((max start ps) (min end pe)))
          ; (print overlap)
          (when (> (get overlap 1) (get overlap 0))
            (setv temp (get overlap 0))
            (nextset.append (+ (- temp start) (get y 0)))
            (nextset.append (- (get overlap 1) (get overlap 0))))
          (when (and (> start ps) (> pe start))
            (nextpieces.append #(ps start)))
          (when (and (> pe end) (> end ps))
            (nextpieces.append #(end pe)))
          (when (or (< pe start) (> ps end))
            (nextpieces.append #(ps pe))))

        (setv pieces nextpieces))

      (for [#(ps pe) pieces]
        (nextset.append ps)
        (nextset.append (- pe ps))))

    (setv curset nextset)
    (+= i 1))

  (min (gfor #(i x) (enumerate curset) :if (= 0 (& i 1)) x)))


(defmain []
  (print (soln puzzle-input)(soln2 puzzle-input)))
