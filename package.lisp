;;;; package.lisp

(defpackage #:ev-liturgical-colors
  (:use #:cl #:local-time)
  (:nicknames #:lit-colors)
  (:export #:get-region
           #:region-name #:region-start #:region-color
           #:region-real-name))

