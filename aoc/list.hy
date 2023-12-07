;; Run all the solutions and print outputs, with timing information

(import dataclasses [dataclass])
(import re)
(import runpy)
(import subprocess)
(import time)
(import os)

(import rich [box])
(import rich.table [Column Table])
(import rich.console [Console])
(import rich.live [Live])

(require hyrule.control [defmain])


(setv SOLN-PATH (re.compile r"day(\d\d).hy$"))


(defclass [dataclass] Soln []
  #^ str part1
  #^ str part2
  #^ float cpu-s
  #^ float wall-s
  #^ float mem-mb)


(defn find-modules []
  (let [parent-dir (os.path.dirname __file__)]
    (lfor module-path (os.listdir parent-dir)
          :if (SOLN-PATH.match module-path)
          (os.path.join parent-dir module-path))))


(defn run-soln [path]
  (setv process (subprocess.run
                 f"env time -f '%e %S %U %M' hy {path}"
                 :shell True
                 :check True
                 :capture_output True))
  (setv #(part1 part2)
        (.split (process.stdout.decode "utf-8")))
  (setv #(wall-s user_s system_s mem_kb)
        (.split (process.stderr.decode "utf-8")))
  (Soln
    :part1 part1
    :part2 part2
    :wall-s (float wall-s)
    :cpu-s (+ (float user_s) (float system_s))
    :mem-mb (/ (float mem_kb) 1000)))


(defn run-solns []
  (setv results {})
  (for [module (tqdm (find-modules))]
    (setv (get results module) (run-soln module)))
  results)


(defn display-solns []
  (setv table
        (Table "Day"
               "Level 1"
               "Level 2"
               (Column "Wall Time (s)" :justify "right")
               (Column "CPU Time (s)" :justify "right")
               (Column "Memory (mb)" :justify "right")
               :title "Advent of Code 2023 Solutions"
               :box box.SIMPLE_HEAD
               :expand True
               :title_style "bold green"
               :show_header True))
  (setv total-wall-s 0)
  (setv total-cpu-s 0)
  (with [_ (Live table)]
    (for [module (find-modules)]
      (setv soln (run-soln module))
      (+= total-wall-s soln.wall-s)
      (+= total-cpu-s soln.cpu-s)
      (table.add_row (.group (SOLN-PATH.search module) 1)
                     (or soln.part1 "")
                     (or soln.part2 "")
                     f"{soln.wall-s :.2f}"
                     f"{soln.cpu-s :.2f}"
                     f"{soln.mem-mb :.2f}"))
    (table.add_row)
    (table.add_row ""
                   ""
                   "Total"
                   f"{total-wall-s :.2f}"
                   f"{total-cpu-s :.2f}"
                   "")))


(defmain []
  (display-solns))
