
(in-package :chess-engine-parse)


(defparameter *stockfish-options*
  "option name Use Debug Log type check default false
option name Use Search Log type check default false
option name Search Log Filename type string default SearchLog.txt
option name Book File type string default book.bin
option name Best Book Move type check default false
option name Contempt Factor type spin default 0 min -50 max 50
option name Mobility (Middle Game) type spin default 100 min 0 max 200
option name Mobility (Endgame) type spin default 100 min 0 max 200
option name Passed Pawns (Middle Game) type spin default 100 min 0 max 200
option name Passed Pawns (Endgame) type spin default 100 min 0 max 200
option name Space type spin default 100 min 0 max 200
option name Aggressiveness type spin default 100 min 0 max 200
option name Cowardice type spin default 100 min 0 max 200
option name Min Split Depth type spin default 4 min 4 max 12
option name Max Threads per Split Point type spin default 5 min 4 max 8
option name Threads type spin default 2 min 1 max 64
option name Use Sleeping Threads type check default true
option name Hash type spin default 32 min 1 max 8192
option name Clear Hash type button
option name Ponder type check default true
option name OwnBook type check default false
option name MultiPV type spin default 1 min 1 max 500
option name Skill Level type spin default 20 min 0 max 20
option name Emergency Move Horizon type spin default 40 min 0 max 50
option name Emergency Base Time type spin default 200 min 0 max 30000
option name Emergency Move Time type spin default 70 min 0 max 5000
option name Minimum Thinking Time type spin default 20 min 0 max 5000
option name Slow Mover type spin default 100 min 10 max 1000
option name UCI_Chess960 type check default false
option name UCI_AnalyseMode type check default false"
  
  "Example stockfish options")

(defparameter *sample-info*
  "info depth 3 seldepth 2 score cp 12 nodes 253 nps 253000 time 1 multipv 2 pv b1c3 g8f6 g1f3 b8c6"
  
  "Sample stockfish analysis info")

(defun split-string (string &optional (sep #\Newline))
  "Split a string on each occurence of the sep character"
  (unless (string= "" string)  
    (do* ((prev 0 (+ 1 space))
          (space (search `(,sep) string) (search `(,sep) string :start2 prev))
          (lines (list (subseq string prev space)) (cons (subseq string prev space) lines)))
         (nil)
      (when (null space)
        (return (reverse lines))))))


(defun prefixedp (prefix string)
  (string-equal prefix string :end2 (length prefix)))


(defun find-option-name (string)
  "Find the name of an option, which is a special case because it can contain spaces"
  (let ((start (search " name " string)))
    (when start
      (incf start (length " name "))
      (subseq string start (search " type " string :start2 start)))))


(defun parse-option (string)
  (let ((name (find-option-name string))
        (type (find-value-string string "type"))
        (default (find-value-string string "default"))
        (min (find-value string "max"))
        (max (find-value string "min")))
    (ecase (find type '(:check :spin :string :button) :test #'string-equal)

      (:button
       (make-instance 'button :name name))

      (:spin
       (assert (and min max default))
       (make-instance 'spin :name name :value (parse-integer default) :max max :min min))

      (:check
       (assert (and default))
       (make-instance 'check :name name :value (string-equal "true" default)))

      (:string
       (assert (and default))
       (make-instance 'string-option :name name :value default)))))


(defun find-value-string (string name)
  "Find the value of the property \"name\" within \"string\""
  (let ((match (search (format nil " ~a " name) string)))
    (when match
      
      ;; Skip the name plus two space
      (incf match (+ 2 (length name)))

      (cond ((string-equal "score" name)
             (let ((first-space (1+ (search " " string :start2 match))))
               (return-from find-value-string (subseq string match (search " " string :start2 first-space)))))

            ((string-equal "pv" name)
             (return-from find-value-string (subseq string match)))

            (t
             (subseq string match (search " " string :start2 match)))))))


(defun find-value (string name &optional (numericp t))
  "Find the value of the property \"name\" within \"string\""
  (let ((v (find-value-string string name)))
    (if (and numericp v)
        (parse-integer v)
        v)))


(defun parse-score (score)
  (cond ((prefixedp "cp" score)
         `(:centipawns . ,(parse-integer (subseq score (length "cp ")))))

        ((prefixedp "mate" score)
         `(:mate . ,(parse-integer (subseq score (length "mate ")))))

        (t
         (warn "Unknown score format ~s" score))))


(defun parse-info-pv (string)
  (let ((depth (find-value string "depth"))
        (seldepth (find-value string "seldepth"))
        (score (find-value string "score" nil))
        (nps (find-value string "nps"))
        (nodes (find-value string "nodes"))
        (multipv (find-value string "multipv"))
        (time (find-value string "time"))
        (pv (find-value string "pv" nil)))
    
    (make-instance 'pv
                   :score (parse-score score)
                   :moves (split-string pv #\Space)
                   :index multipv
                   :nps nps
                   :nodes nodes
                   :depth depth
                   :seldepth seldepth
                   :time time)))
