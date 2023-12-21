(in-package :aoc-2023)

(define-condition queue-empty (condition) ())
(define-condition recieved-value (condition) ((name :initarg :name)))

;; PARSING

(defun parse-type ()
  (either (with-monad
            (assign type (parse-character "%&"))
            (unit (case type (#\% :flip-flop) (#\& :conjunction))))
          (unit :other)))

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign type (parse-type))
     (assign name (parse-keyword))
     (parse-string " -> ")
     (assign destinations (parse-list (parse-keyword #'alpha-char-p) ", "))
     (unit (list name type destinations)))))


;; MODULES

;; Each MODULE has a name, a list of (input) terminals, and a list of terminals
;; of other modules to output to.
(defclass module ()  
  ((name :initarg :name)
   (terminals :initform '())
   (outputs :initform '())))

;; Flip flop modules have a STATE that can be :off or :on.
(defclass flip-flop-module (module)
  ((state :initform :off)))

(defclass conjunction-module (module) ())

;; A button module keeps track of how many times it's been pushed (pulsed).
(defclass button-module (module)
  ((pushes :initform 0 :accessor button-pushes)))

;; A watcher contains a value to watch for. 
(defclass watcher-module (module)
  ((watch-for :initform '())))


;; TERMINALS

;; A terminal keeps track of which module created it.
(defclass terminal ()  
  ((module :initarg :module :initform nil)))

;; Terminals of conjunction modules keep track of the last pulse they recieved.
(defclass conjunction-terminal (terminal)
  ((last-pulse :initform :low :accessor last-pulse)))


;; MAKE-TERMINAL

(defgeneric make-terminal (module))

(defmethod make-terminal ((module module))
  (make-instance 'terminal :module module))

;; Store the newly created terminal.
(defmethod make-terminal :around ((module module))
  (let ((new-terminal (call-next-method)))
    (push new-terminal (slot-value module 'terminals))
    new-terminal))

(defmethod make-terminal ((module conjunction-module))
  (make-instance 'conjunction-terminal :module module))


;; PULSE

;; Sends THING a PULSE. Returns (possibly updated) QUEUE.
(defgeneric pulse (thing pulse queue))

;; TERMINALS send the pulse to their module.
(defmethod pulse ((terminal terminal) pulse queue)
  (pulse (slot-value terminal 'module) pulse queue))

;; MODULES queue pulses for all of their outputs.
(defmethod pulse ((module module) pulse queue)
  (with-slots (name outputs) module
    (iter
      (for (output-name output) in (reverse outputs))
      (queue-push-backf (list name output-name output pulse) queue)
      (finally (return queue)))))

;; A FLIP-FLOP-MODULE changes state on a low pulse, and then calls PULSE on the
;; base module to send a pulse to all its outputs. 
(defmethod pulse ((module flip-flop-module) pulse queue)
  (if (eq pulse :low)
      (with-slots (state) module
        (case state
          (:off
           (setf state :on)
           (call-next-method module :high queue))
          (:on
           (setf state :off)
           (call-next-method module :low queue))))
      queue))

;; A CONJUNCTION-TERMINAL stores the pulse before forwarding it to its module.
(defmethod pulse ((terminal conjunction-terminal) pulse queue)
  (with-slots (module last-pulse) terminal
    (setf last-pulse pulse)
    (pulse module pulse queue)))

;; A CONJUNCTION-MODULE tests whether all it's inputs are high, then calls PULSE
;; on the base module to send the appropriate pulse to its outputs. 
(defmethod pulse ((module conjunction-module) pulse queue)
  (declare (ignore pulse))
  (with-slots (terminals) module
    (let ((all-high (every (lambda (terminal)
                             (eq (last-pulse terminal) :high))
                           terminals)))
      (call-next-method module (if all-high :low :high) queue))))

;; A BUTTON-MODULE counts how many times it's been pulsed.
(defmethod pulse ((module button-module) pulse queue)
  (incf (slot-value module 'pushes))
  (call-next-method))

;; A WATCHER-MODULE watches for a specific :low/:high pulse. Raises 'RECIEVED-VALUE
;; when it see it. 
(defmethod pulse ((module watcher-module) pulse queue)
  (when (eq pulse (slot-value module 'watch-for))
    (cerror "Continue" 'recieved-value))
  (call-next-method))


;; OTHER FUNCTIONS 

(defun connect (source destination)
  "Create input terminal at DESTINATION then add it to SOURCE's outputs. "
  (let ((input-terminal (make-terminal destination)))
    (push (list (slot-value destination 'name) input-terminal)
          (slot-value source 'outputs))))

(defun make-module (type name)
  "Make module of appropriate type. "
  (make-instance (case type
                   (:flip-flop 'flip-flop-module)
                   (:conjunction 'conjunction-module)
                   (:button 'button-module)
                   (:watcher 'watcher-module)
                   (otherwise 'module))
                 :name name))

(defun make-modules (input button)
  "Parse INPUT, create and connect all modules, connect BUTTON to broadcaster. "
  (let* ((parsed (run-parser (parse-file) input))
         (modules (iter
                    (with ret = (make-hash-table))
                    (for (name type outputs) in parsed)
                    (setf (gethash name ret) (make-module type name))
                    (finally (return ret)))))
    (iter
      (for (name type outputs) in parsed)
      (for source = (gethash name modules))
      (iter
        (for destination-name in outputs)
        (for destination = (or (gethash destination-name modules)
                               (make-module :other destination-name)))
        (connect source destination)))
    (connect button (gethash :broadcaster modules))
    modules))

(defun add-watchers (names watch-for modules)
  "Connect a watcher to each of the modules in NAMES. "
  (iter
    (for name in names)
    (for watcher = (make-module :watcher :watcher))
    (setf (slot-value watcher 'watch-for) watch-for)
    (connect (gethash name modules) watcher)))

(defun run-simulation ()
  (iter
    (with queue = (make-queue))
    (with results = (make-hash-table))
    (restart-case
        (when (queue-empty-p queue) (error 'queue-empty))
      (push-button (button)
        (setf queue (pulse button :low queue)))
      (exit () (finish)))
    (until (queue-empty-p queue))
    (for (src dest terminal pulse) = (queue-pop-frontf queue))
;;    (format t "~a ~a -> ~a~%" src pulse dest)
    (incf (gethash pulse results 0))
    (restart-case
        (setf queue (pulse terminal pulse queue))
      (exit () (finish)))
    (finally (return results))))

(defun day20 (input &key (part 1))
  (let* ((button (make-module :button :button))
         (modules (make-modules input button))         
         (num-button-pushes '()))
    (when (= part 2) (add-watchers '(:sr :sn :rf :vq) :high modules))
    (handler-bind
        ((queue-empty (lambda (c)
                        (declare (ignore c))             
                        (if (or (= part 2) (< (button-pushes button) 1000))
                            (invoke-restart 'push-button button)
                            (invoke-restart 'exit))))
         (recieved-value (lambda (c)
                           (declare (ignore c))
                           (push (button-pushes button) num-button-pushes)
                           (if (< (length num-button-pushes) 4)
                               (invoke-restart 'continue)
                               (invoke-restart 'exit)))))      
      (let ((pulses (run-simulation)))
        (if (= part 1)
            (apply #'* (alexandria:hash-table-values pulses))
            (apply #'lcm num-button-pushes))))))
