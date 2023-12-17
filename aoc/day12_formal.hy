(import logging)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 12))
(setv sample-inputs (bzn.get-sample-inputs 2023 12))


;; Utilities
(setv logger (logging.getLogger __name__))
(defn slog [#* args #** kwargs]
  (logger.info #* args #** kwargs)
  (get args 0))


;; Input handling
(defn parse-line [line]
  (setv #(pattern constraints) (line.split))
  (setv constraints (list (map int (constraints.split ","))))
  #(pattern constraints))


(defn level2 [pattern constraints]
  (setv pattern2 (.join "?" (* 5 [pattern])))
  (setv constraints2 (* 5 constraints))
  #(pattern2 constraints2))


;; Solve puzzle
(defn solve [puzzle-input]
  (setv lines (.split (puzzle-input.strip) "\n"))
  (setv parsed (lfor line lines (parse-line line)))
  )


(defn run-tests []

  (assert (= ))



  )


(defmain [[tests False]]
  (if tests
      (run-tests))

  (print (solve (get sample-inputs 1))))
