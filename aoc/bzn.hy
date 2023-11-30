:;; Lightweight reimplementation of Blitzen as a library.
:;; (Original https://github.com/kunalb/blitzen/)
:;;
:;;                           20'23
:;;                        (         )
:;;                         \(     )/
:;;                          \(   )/
:;;                        (\ )---( /)
:;;                          / a c \
:;;                          (  o  )
:;;                           \ ‿ /
:;;
:;;
:;; Usage:
:;;   (set-key <private key from 'session' cookie>)
:;;   (get-input <year> <day>)
:;;   (submit-answer <year> <day> <level> <answer>)

(import
  pathlib [Path]
  textwrap)

(import
  bs4 [BeautifulSoup]
  requests)


(defn ensure-dir [path]
  (setv path (.expanduser (Path path)))
  (path.mkdir :parents True :exist_ok True)
  path)


(defn cache-dir []
  (ensure-dir "~/.cache/bzn"))


(defn config-dir []
  (ensure-dir "~/.config/bzn"))


(defn key-file []
  (/ (config-dir) "key"))


(defn set-key [key]
  "Save private key to cache"
  (with [keyfile (open (key-file) "w")]
    (keyfile.write key)))


(defn get-key []
  (with [keyfile (open (key-file))]
    (keyfile.read)))


(defn open-url [url]
  (setv cookie-jar (CookieJar))
  (setv opener
        (urllib.request.build_opener urllib.request.HTTPCookieProcessor cookie-jar))
  (opener.addheaders.append #("Cookie" "session={(get-key)}"))
  (with [response (opener.open url)]
    (. (response.read) text)))


(defn get-input [year day]
  "Fetch problem input"
  (setv cache-path (/ (cache-dir) f"{year}_{day}"))
  (if (cache-path.exists)
      (with [cached (open cache-path)]
        (cached.read))
      (do
        (setv
          fetched-input
          (. (requests.get f"https://adventofcode.com/{year}/day/{day}/input"
                           :cookies {"session" (get-key)})
             text))
        (with [cached (open cache-path "w")]
          (cached.write fetched-input))
        fetched-input)))


(defn submit-answer [year day level answer]
  "Submit solution to problem"
  (setv result
        (. (requests.post f"https://adventofcode.com/{year}/day/{day}/answer"
                          :cookies {"session" (get-key)}
                          :data {"level" (str level)
                                 "answer" (str answer)})
           text))
  (.get_text
    (. (BeautifulSoup result "html.parser") main)))


(defn display-response [text]
  (print (textwrap.fill text :width 70 :tabsize 4)))
