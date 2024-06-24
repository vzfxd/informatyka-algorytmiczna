(defun binomial (n k)
  (cond
    ((= k 0) 1)
    ((= k n) 1)
    ((> k n) 0)
    (t (+ (binomial (- n 1) k)
          (binomial (- n 1) (- k 1))))))

(defun pascal-row (n)
  (if (zerop n)
      '(1)
      (let* ((prev-row (pascal-row (1- n)))
             (extended-prev-row (append prev-row '(0)))
             (shifted-prev-row (cons 0 prev-row)))
        (mapcar #'+ extended-prev-row shifted-prev-row))))
  
(defun binomial2 (n k)
  (nth k (pascal-row n)))

(defun merge (left right)
  (cond ((null left) right)
        ((null right) left)
        ((<= (car left) (car right))
         (cons (car left) (merge (cdr left) right)))
        (t (cons (car right) (merge left (cdr right))))))

(defun merge-sort (list)
  (if (or (null list) (null (cdr list)))
      list
      (let* ((mid (floor (/ (length list) 2)))
             (left (subseq list 0 mid))
             (right (subseq list mid)))
        (merge (merge-sort left) (merge-sort right)))))

;; de 
(defun extended_gcd (a b)
  ( cond 
      ((= b 0) (list a 1 0))
      (t (let* (
            (lst (extended_gcd b (mod a b)))
            (d (car lst))
            (x (cadr lst))
            (y (caddr lst)))
          (list d y (- x (* y (floor (/ a b)))))
        )
      )
  )
)

(defun de (a b)
  (extended_gcd a b)
)


;; prime factors

(defun prime_factors (n)
  ( labels ( 
    (_prime_factors (n d acc)
      (cond
        ((= n 1) acc)
        ((= (mod n d) 0) (_prime_factors (/ n d) d (cons d acc)))
        (t (_prime_factors n (+ d 1) acc))
      )
    ))
    (_prime_factors n 2 nil)
  )
)

;; totient

(defun totient (n)
  (labels (
    (_totient (n k acc)
        (cond
        ((= k n) acc)
        ((= (gcd n k) 1) (_totient n (+ k 1) (+ acc 1)))
        (t (_totient n (+ k 1) acc))
      )
    ))
    (_totient n 1 0)
  )

)

;; phi(n) = (p1^(k1 -1)) * (p1 - 1) * (p2^(k2 -1)) * (p2 - 1) *    ... * (pr^(kr -1)) * (pr - 1)

(defun totient2 (n)
  (labels (
    (_totient2 (acc divisors)
      (let (
        (first (car divisors))
        (second (cadr divisors))
        (rest (cdr divisors)))
      (cond 
        ((null rest) (* acc (- first 1)))
        ((equal first second) 
          (_totient2 (* acc first) rest))
        (t (_totient2 (* acc (- first 1)) rest))))
    ))
    (_totient2 1 (prime_factors n))
))


;; prime

(defun range (a b)
  (labels (
    (_range (a b acc)
      (cond
        ((= a b) (reverse (cons a acc)))
        (t (_range (+ a 1) b (cons a acc)))
    )))
    (_range a b nil)
))


(defun sito (n)
  (labels ((_sito (numbers it)
    (cond  
      ((null numbers) nil)
      ((=(* it it) n) numbers)
      (t (cons (car numbers) 
               (_sito (remove-if (lambda (x) (= 0 (mod x (car numbers)))) (cdr numbers)) (+ it 1))))
    )))
  (_sito (range 2 n) 0)))

(defun main ()
  (let ((n 5)
        (k 2)
        (list '(5 1 9 3 7 6 2 8 4)))
    (let* ((start-time-binomial (get-internal-real-time))
           (binomial-result (binomial n k))
           (end-time-binomial (get-internal-real-time))
           (elapsed-time-binomial (- end-time-binomial start-time-binomial)))
      (format t "binomial 5 2: ~d (took ~d microseconds)~%" binomial-result elapsed-time-binomial))
    
    ;; Pomiar czasu dla funkcji binomial2
    (let* ((start-time-binomial2 (get-internal-real-time))
           (binomial2-result (binomial2 n k))
           (end-time-binomial2 (get-internal-real-time))
           (elapsed-time-binomial2 (- end-time-binomial2 start-time-binomial2)))
      (format t "binomial2 5 2: ~d (took ~d microseconds)~%" binomial2-result elapsed-time-binomial2))
      
    (format t "merge-sort (5 1 9 3 7 6 2 8 4): ~a~%" (merge-sort list))
    (format t "de 15 4: ~a~%" (de 15 4))
    (format t "prime_factors 21: ~a~%" (prime_factors 21))
    (let* ((start-time-totient (get-internal-real-time))
           (totient-result (totient 21))
           (end-time-totient (get-internal-real-time))
           (elapsed-time-totient (- end-time-totient start-time-totient)))
      (format t "totient 21: ~d (took ~d microseconds)~%" totient-result elapsed-time-totient))
    
    ;; Pomiar czasu dla funkcji totient2
    (let* ((start-time-totient2 (get-internal-real-time))
           (totient2-result (totient2 21))
           (end-time-totient2 (get-internal-real-time))
           (elapsed-time-totient2 (- end-time-totient2 start-time-totient2)))
      (format t "totient2 21: ~d (took ~d microseconds)~%" totient2-result elapsed-time-totient2))
    (format t "primes 21: ~a~%" (sito 21))
    ))

(main)