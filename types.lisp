
;;
;; Engine Definitions
;;

(defclass analysis ()
  ((bestmove :documentation "The suggested best move in the current position"
             :accessor analysis-bestmove
             :initform :unknown)
   (pvs :documentation "A list of variations"
        :accessor analysis-pvs
        :initform nil))
  (:documentation "An analysis is started with the engine command 'go' and stops when reciving 'bestmove'"))

(defclass engine ()
  ((process :documentation "Handle to external engine process"
            :type process
            :accessor engine-process
            :initarg :process)
   (reader :documentation "Java class Buffered Reader (for reading from the engine)"
           :accessor engine-reader
           :initform :nil)
   (writer :documentation "Java class Buffered Writer (for writing to from engine)"
           :accessor engine-writer
           :initform nil)
   (state :documentation "The last known state of the engine"
           :type keyword
           :accessor engine-state
           :initform :not-initialized)
   (analysis :documentation "The current 'analysis'-report"
             :type analysis
             :accessor engine-analysis
             :initform nil)
   (options :documentation "The list of engine options parsed during UCI init process"
            :accessor engine-options
            :initform nil))

  (:documentation "Everything you need to talk to a UCI engine"))

(defclass pv ()
  ((depth :documentation "Search depth"
          :accessor pv-depth
          :initarg :depth)
   (seldepth :documentation "SelectiveSearch depth"
          :accessor pv-seldepth
          :initarg :seldepth)
   (score :documentation "score (centipawns or MATE)"
          :accessor pv-scoe
          :initarg :score)
   (nodes :documentation "nodes processed"
          :accessor pv-nodes
          :initarg :nodes)
   (nps :documentation "nodes processed per second"
        :accessor pv-nps
        :initarg :nps)
   (time :documentation "Time thinking"
        :accessor pv-time
        :initarg :time)
   (index :documentation "Multipv index"
          :accessor pv-index
          :initarg :index)
   (moves :documentation "List of moves"
          :accessor pv-moves
          :initarg :moves))
  (:documentation "Represents a primary variation (or whatever PV stands for..)"))



;;
;;  Engine Options
;;
(defclass option ()
  ((name :type string
         :documentation "Option name"
         :accessor option-name
         :initarg :name)))


(defclass button (option)
  ())

(defclass check (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)))

(defclass string-option (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)))

(defclass spin (option)
  ((value :documentation "Option value"
          :accessor option-value
          :initarg :value)
   (min :documentation "Min Value"
        :accessor option-min-value
        :initarg :min)
   (max :documentation "Max Value"
        :accessor option-max-value
        :initarg :max)))



