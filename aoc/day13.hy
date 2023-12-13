(import collections [defaultdict])

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 13))
(setv sample-inputs (bzn.get-sample-inputs 2023 13))


(defn test-num [n]
  (= 0 (& n (- n 1))))

(defn solve-smudged [lines]
  (setv nums (list (map (fn [x]
                          (int (.replace (x.replace "." "0") "#" "1") :base 2))
                        lines)))

  (setv mirrors [])
  (for [#(i #(p1 p2)) (enumerate (zip nums (cut nums 1 None)))]
    (when (or (= p1 p2) (test-num (^ p1 p2)))
        (mirrors.append i)))

  (setv results [])
  (for [mirror mirrors]
    (setv diff (sum (gfor #(p1 p2)
                          (zip (cut nums (+ 1 mirror) None)
                               (cut nums mirror None -1))
                          (cond
                            (= p1 p2) 0
                            (test-num (^ p1 p2)) 1
                            True 10))))
    (when (= diff 1)
      (results.append (+ 1 mirror))))

  (assert (>= 1 (len results)) f"Too many {results =}")
  (if results (get results 0) 0))


(defn solve-horiz [lines]
  (setv indexes (defaultdict list))

  (setv mirrors [])
  (for [#(i line) (enumerate lines)]
    (when (= 1 (- i (get (indexes.get line [-1000]) -1)))
        (mirrors.append #((- i 1) i)))
    (.append (get indexes line) i))



  (when (not mirrors)
    (return 0))

  (setv results [])
  (for [mirror mirrors]
    (when (all (gfor #(l1 l2)
                     (zip (cut lines (get mirror 1) None)
                          (cut lines (get mirror 0) None -1))
                     (= l1 l2)))
      (results.append (+ 1 (get mirror 0)))))

  (assert (<= (len results) 1) f"Results: {results} {mirrors}")
  (if results (get results 0) 0))


(defn solve-maze [maze [level 1]]
  (setv lines (maze.split "\n"))

  (setv transposed [])
  (for [i (range (len (get lines 0)))]
    (transposed.append
      (.join "" (gfor line lines (get line i)))))

  (setv solve-fn (if (= 1 level) solve-horiz solve-smudged))

  (setv rows (solve-fn lines))
  (setv cols (solve-fn transposed))
  (setv result (+ (* 100 rows) cols))

  (assert (or (= rows 0) (= cols 0)) f"{rows} {cols}")

  result)


(defn soln [puzzle-input [level 1]]
  (setv mazes (.split (puzzle-input.strip) "\n\n"))
  (sum (gfor maze mazes (solve-maze maze level))))


(defmain []
  (print (soln puzzle-input) (soln puzzle-input 2)))
