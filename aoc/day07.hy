(import itertools)
(import collections [Counter])
(import functools)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 7))

(defn playsort [val level]
  (setv card-str
        (if (= 1 level) "AKQJT98765432" "AKQT98765432J"))
  (setv ordering
      (dict (zip (cut card-str None None -1)
                 (itertools.count :start 1))))

  (setv hand (get val 0))
  (setv counter (Counter hand))
  (setv commons (counter.most_common))

  (when (!= 1 level)
    (setv replacement
          (cond
            (= (get commons 0) #("J" 5)) "J"
            (= (get commons 0 0) "J") (get commons 1 0)
            True (get commons 0 0)))

    (setv alt-hand (hand.replace "J" replacement))
    (when (!= alt-hand hand)
      (setv counter (Counter alt-hand))
      (setv commons (counter.most_common))))

  (setv most-common (get commons 0 1))

  (setv key
      (cond
        (= 5 most-common) 7
        (= 4 most-common) 6
        (and (= 3 most-common) (= 2 (get commons 1 1))) 5
        (= 3 most-common) 4
        (and (= 2 most-common) (= 2 (get commons 1 1))) 3
        (= 2 most-common) 2
        (= 1 most-common) 1))

  (setv result
        #(key #* (map (fn [x] (get ordering x)) hand)))
  result)


(defn soln [puzzle-input level]
  (setv hands (list (map (fn [x] (let [results (x.split)]
                                   #((get results 0)
                                      (int (get results 1)))))
                         (.split (puzzle-input.strip) "\n"))))

  (hands.sort :key (functools.partial playsort :level level))
  (sum (gfor #(i hand) (enumerate hands)
             (* (+ i 1) (get hand 1)))))


(defmain []
  (print (soln puzzle-input 1)
         (soln puzzle-input 2)))
