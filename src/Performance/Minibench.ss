;; -*- mode: scheme -*-

(library (Performance.Minibench foreign)
  (export timeNs
          gc
          toFixed
  )

  (import (only (rnrs base) define let lambda quote)
          (only (chezscheme)
            collect format time-difference current-time time-nanosecond fixnum->flonum)
  )

  ;; timeNs :: (Unit -> a) -> Effect Number
  ;; run a function accepting a unit and obtain the
  ;; execution time in nanoseconds.
  (define timeNs
    (lambda (closure)
      (lambda ()
        (let
          ([start (current-time 'time-monotonic)])
          (closure 'unit) ;; evaluate the closure
          (let ([end (current-time 'time-monotonic)])
            (fixnum->flonum (time-nanosecond (time-difference end start))))))))

   ;; instantiate the garbage collection.
  (define gc collect)

  ;; convert a number to a fixed-point string.
  (define toFixed
    (lambda (n)
      (format #f "~,2F" n)))
)