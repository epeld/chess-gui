
(defpackage :chess.engine.types
  (:use :cl)
  (:export :engine-process :engine-reader :engine-writer
           :engine-options :engine-state :engine-analysis
           :analysis-bestmove))

(defpackage :chess.engine.parse
  (:use :cl)
  (:export :parse-option :prefixedp))

(defpackage :chess.engine
  (:use :cl :chess.engine.types :chess.engine.parse)
  (:export :engine-go :engine-stop :engine-handle-message
           :all-input-lines))

(defpackage :chess.engine.model
  (:use :cl)
  (:use :cl :chess.engine.types :chess.engine))

(defpackage :chess.engine.gui
  (:use :cl))
