;; -*- mode: scheme -*-

(library (Performance.Minibench foreign)
  (export time
          gc
          toFixed)
  (import (only (rnrs base)
                define lambda quote let vector)
          (only (chezscheme)
                collect format current-time time-second time-nanosecond))

  (define time
    (lambda ()
      (let ([t (current-time 'time-monotonic)])
        (vector (time-second t)
                (time-nanosecond t)))))

  (define gc collect)

  (define toFixed
    (lambda (n)
      (format #f "~,2F" n)))

)
