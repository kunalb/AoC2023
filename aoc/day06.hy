(import math)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 6))

(defn soln [puzzle-input [level 1]]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv times-pieces (cut (.split (get lines 0)) 1 None))
  (setv distances-pieces (cut (.split (get lines 1)) 1 None))

  (setv parse-fn
        (if (= level 1)
            (fn [xs] (list (map int xs)))
            (fn [xs] [(int (.join "" xs))])))

  (setv times (parse-fn times-pieces))
  (setv distances (parse-fn distances-pieces))

  (setv result 1)
  (for [#(T D) (zip times distances)]
    (setv inner (- (* T T) (* 4 D)))
    (setv lower-bound
          (* .5 (- T (** inner .5))))
    (setv upper-bound
          (* .5 (+ T (** inner .5))))

    (setv solns
          (+ 1
             (- (min T (math.floor (math.nextafter upper-bound (- math.inf))))
                (max 0 (math.ceil (math.nextafter lower-bound math.inf))))))
    (*= result solns))

  result)


(defmain []
  (print (soln puzzle-input 1)
         (soln puzzle-input 2)))
