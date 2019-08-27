#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr

(define (quadratic a b c)
    (let ((-b (- b))
          (n2a (* 2 a))
          (b24ac (sqrt (- (expt b 2) (* 4 a c)))))
    (cons (/ (+ -b b24ac) n2a)
          (/ (- -b b24ac) n2a))))

(define equation '(quadratic 2 2 2))

(display equation) (newline)
(display (eval equation)) (newline)
