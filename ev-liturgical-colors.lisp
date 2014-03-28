;;;; ev-liturgical-colors.lisp
;;;; The idea is based on http://liturgicalcolour.appspot.com/

(in-package #:ev-liturgical-colors)

(defclass region ()
  ((name :type keyword
         :initarg :name
         :reader region-name)
   (start :type timestamp
          :initarg :start
          :reader region-start)
   (color :type keyword
          :initarg :color
          :reader region-color)))

(defparameter *print-format*
  '((:day 2) #\. (:month 2) #\. (:year 4) #\Space (:hour 2) #\: (:min 2)))

(defvar *default-translation*
  :german)

(defparameter *translations*
  (alexandria:plist-hash-table
   (list
    :german
    (alexandria:plist-hash-table
     (list
      :advent "Adventszeit"
      :christmas "Weihnachten"
      :christmas-time "Weihnachtszeit"
      :epiphany "Epiphanias"
      :epiphany-time "Epiphaniaszeit"
      :last-after-epiphany "Letzter Sonntag nach Epiphanias"
      :pre-passion "Vor-Passion"
      :passion "Passionszeit"
      :maundy-thursday "Gründonnerstag"
      :good-friday "Karfreitag"
      :holy-saturday "Karsamstag"
      :easter "Ostern"
      :easter-time "Österliche Freudenzeit"
      :ascension "Christi Himmelfahrt"
      :pentecost "Pfingsten"
      :trinity-sunday "Trinitatis"
      :trinity-time "Trinitatiszeit"
      :reformation "Reformationstag"
      :buss-und-bettag "Buß- und Bettag"
      :ewigkeitssonntag "Ewigkeits-/Totensonntag")))))

(defmethod print-object ((object region) stream)
  (print-unreadable-object (object stream)
    (format stream "region ~a, starts: " (region-name object))
    (format-timestring stream (region-start object)
                       :format *print-format*)
    (format stream ", color: ~a" (region-color object))))

(defun make-region (name start color)
  (make-instance 'region :name name :start start :color color))

(defun make-date (day month year)
  (encode-timestamp 0 0 0 0 day  month year))

(defun easter-date (year)
  "=> date of easter sunday"
  ;; formula by Heiner Lichtenberg
  ;; http://www.ptb.de/cms/fachabteilungen/abt4/fb-44/ag-441/darstellung-der-gesetzlichen-zeit/wann-ist-ostern.html
  (let* ((k (floor year 100))
         (m (- (+ 15 (floor (+ (* 3 k) 3) 4)) (floor (+ (* 8 k) 13) 25)))
         (s (- 2 (floor (+ (* 3 k) 3) 4)))
         (a (mod year 19))
         (d (mod  (+ (* 19 a) m) 30))
         (r (floor (+ d (floor a 11)) 29))
         (og (- (+ 21 d) r))
         (sz (- 7 (mod (+ year (floor year 4) s)  7)))
         (oe (- 7 (mod (- og sz) 7)))
         (os (+ og oe)))
    (timestamp+ (make-date 1 3 year) (1- os) :day)))

(defun first-advent-date (year)
  "=> date of first advent"
  (timestamp- (adjust-timestamp (make-date 25 12 year)
                (offset :day-of-week :sunday))
              21 :day))

(defun calculate-regions (year)
  (let ((easter (easter-date year))
        (first-advent (first-advent-date year)))
    (list
     (make-region :christmas-time (make-date 1 1 year) :white)
     (make-region :epiphany (make-date 6 1 year) :white)
     (make-region :epiphany-time (make-date 7 1 year) :green)
     (make-region :last-after-epiphany (timestamp- easter 70 :day) :white)
     (make-region :pre-passion (timestamp- easter 63 :day) :green)
     (make-region :passion (timestamp- easter 46 :day) :purple)
     (make-region :maundy-thursday (timestamp- easter 3 :day) :white)
     (make-region :good-friday (timestamp- easter 2 :day) :black)
     (make-region :holy-saturday (timestamp- easter 2 :day) :black)
     (make-region :easter easter :white)
     (make-region :easter-time (timestamp+ easter 1 :day) :white)
     (make-region :ascension (timestamp+ easter 39 :day) :white)
     (make-region :easter-time (timestamp+ easter 40 :day) :white)
     (make-region :pentecost (timestamp+ easter 49 :day) :red)
     (make-region :trinity-sunday (timestamp+ easter 56 :day) :white)
     (make-region :trinity-time (timestamp+ easter 57 :day) :green)
     ;;Johannis/Michaelis?
     (make-region :reformation (make-date 31 10 year) :red)
     (make-region :trinity-time (make-date 1 11 year) :green)
     (make-region :buss-und-bettag (timestamp- first-advent 11 :day) :purple)
     (make-region :trinity-time (timestamp- first-advent 10 :day) :green)
     (make-region :ewigkeitssonntag (timestamp- first-advent 7 :day) :green)
     (make-region :advent first-advent :purple)
     (make-region :christmas (make-date 24 12 year) :white)
     (make-region :christmas-time (make-date 27 12 year) :white))))

(defun get-region (&optional (date (now)))
  "=> a color keyword denoting the liturgical color for the date"
  (let ((regions (calculate-regions (timestamp-year date)))
        last)
    (loop for region in regions
       do (if (timestamp> (region-start region) date)
              (return last)
              (setf last region)))))

(defun region-real-name (region &optional (tranlation *default-translation*))
  (gethash (region-name region)
           (gethash tranlation *translations*)))
