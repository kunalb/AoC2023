(require hyrule.control [defmain])
(import bzn)

(import re)

(setv puzzle-input (bzn.get-input 2023 2))

(setv max-counts {"red" 12 "green" 13 "blue" 14})

(defn soln [puzzle-input]
  (setv sum 0)
  (setv power-sum 0)

  (for [line (.split (puzzle-input.strip) "\n")]
    (setv #(game rest) (line.split ":"))
    (setv #(_ game-id) (game.split))
    (setv runs (rest.split ";"))
    (setv safe True)

    (setv min-counts { "red" 0 "green" 0 "blue" 0})
    (for [run runs]
      (setv pieces (run.split ","))
      (for [piece pieces]
        (setv #(count color) (piece.split))
        (when (< (get max-counts color) (int count))
          (setv safe False))
        (when (< (get min-counts color) (int count))
          (setv (get min-counts color) (int count)))))
    (+= power-sum (* #* (min-counts.values)))
    (when safe
      (+= sum (int game_id))))
  #(sum power-sum))

(defmain []
  (print #* (soln puzzle-input)))
