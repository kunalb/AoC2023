(import math)
(import re)

(require hyrule.control [defmain])
(import bzn)

(setv puzzle-input (bzn.get-input 2023 8))


(setv NODES (re.compile r"^([^ ]+?) = \(([^ ,]+?), ([^ ]+?)\)$"))


(defn soln [puzzle-input [level 1]]
  (setv inp (.split (puzzle-input.strip) "\n"))
  (setv instr (get inp 0))

  (setv lookup (cut inp 2 None))
  (setv lookup-map {})
  (for [line lookup]
    (setv matches (NODES.match line))
    (setv (get lookup-map (matches.group 1))
          #((matches.group 2) (matches.group 3))))

  (setv start-set
        (if (= level 1)
            #{"AAA"}
            (sfor x (lookup-map.keys)
                  :if (= "A" (get x -1)) x)))
  (setv lens [])

  (for [pt start-set]
    (setv counter 0)
    (setv ip 0)
    (setv instr-len (len instr))
    (setv steps 0)

    (while (if (= level 1)
               (!= pt "ZZZ")
               (!= (get pt -1) "Z"))
      (setv vals (get lookup-map pt))
      (setv pt
            (if (= "L" (get instr ip))
                (get vals 0)
                (get vals 1)))

      (+= steps 1)
      (+= ip 1)
      (when (>= ip instr-len)
        (setv ip 0)))

    (lens.append steps))

  (math.lcm #* lens))


(defmain []
  (print (soln puzzle-input 1)
         (soln puzzle-input 2)))
