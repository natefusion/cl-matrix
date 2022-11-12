(defun get-atom-matrix (matrix row col)
  (nth col (nth row matrix)))

(defun get-row-matrix (matrix row)
  (nth row matrix))

(defun square-matrix? (matrix)
  (= (length matrix) (length (car matrix))))

(defun nxn-matrix? (matrix n)
  (= n (length matrix) (length (car matrix))))

(defun row-len (matrix)
  (length matrix))

(defun col-len (matrix)
  (length (car matrix)))

(defun row->col-matrix (matrix)
  "Flips rows and columns"
  (loop :for col :below (col-len matrix)
        :collect (mapcar (lambda (x) (nth col x)) matrix)))

(defun gen-matrix (matrix exclude-row exclude-col)
  (loop :for row :below (row-len matrix)
        :if (/= exclude-row row)
        :collect
        (loop :for col :below (col-len matrix)
              :if (/= exclude-col col)
              :collect (get-atom-matrix matrix row col))))

(defun get-determinant-matrix (matrix &optional (n 0))
  (flet ((gen-atom (matrix n)
           `(* ,(get-atom-matrix matrix 0 n)
               ,(get-determinant-matrix (gen-matrix matrix 0 n)))))
    (cond ((not (square-matrix? matrix)) nil)
          ((nxn-matrix? matrix 1) (get-atom-matrix matrix 0 0))
          ((nxn-matrix? matrix 2)
           (let ((a (get-atom-matrix matrix 0 0))
                 (b (get-atom-matrix matrix 0 1))
                 (c (get-atom-matrix matrix 1 0))
                 (d (get-atom-matrix matrix 1 1)))
             `(- (* ,a ,d) (* ,b ,c))))
          ((>= n (1- (row-len matrix)))
           (when (oddp (row-len matrix))
               (list (gen-atom matrix (1- (row-len matrix))))))
          (t (append (when (zerop n) '(+))
                     (cons (list '- (gen-atom matrix n) (gen-atom matrix (1+ n)))
                           (get-determinant-matrix matrix (+ 2 n))))))))

(defun .* (vec1 vec2)
  (cons '+ (mapcar (lambda (x y) `(* ,x ,y)) vec1 vec2)))

(defun multiply-matrix (matrix1 matrix2)
  (when (= (col-len matrix1) (row-len matrix2))
      (loop :for row :in matrix1
            :collect
            (loop :for col :in (row->col-matrix matrix2)
                  :collect (.* row col)))))

(defun get-inverse-matrix (matrix)
  (cond ((nxn-matrix? matrix 1) (get-atom-matrix matrix 0 0))
        ((square-matrix? matrix)
         (loop :for row :below (row-len matrix)
               :collect
               (loop :for col :below (col-len matrix)
                     :collect
                     `(* (/ ,(get-determinant-matrix matrix))
                         ,(list (if (evenp (+ row col)) '+ '-)
                                (get-determinant-matrix (gen-matrix matrix col row)))))))))

(defun op? (exp)
  (or (eq exp '+)
      (eq exp '*)
      (eq exp '/)
      (eq exp '-)))

(defun inv? (exp)
  (when (and (eq (car exp) '/) (eql (length exp) 2)) '(1)))

(defun neg? (exp)
  (when (and (eq (car exp) '-) (eql (length exp) 2)) '(0)))

(defun prefix->infix (exp &optional op)
  (cond ((atom exp) exp)
        ((op? (car exp))
         (prefix->infix (append (inv? exp) (neg? exp) (cdr exp)) (car exp)))
        (t (let ((next (prefix->infix (cdr exp) op)))
             (cons (prefix->infix (car exp)) (when next (cons op next)))))))

(defparameter *t* (get-determinant-matrix '((i j k)
                                            (3 0 2)
                                            (-1 4 2))))


(defun sump (exp)
  (and (listp exp) (eql (car exp) '+)))

(defun differencep (exp)
  (and (listp exp) (eql (car exp) '-)))

(defun negationp (exp)
  (and (listp exp) (eql (car exp) '-) (= (length exp) 2)))

(defun productp (exp)
  (and (listp exp) (eql (car exp) '*)))

(defun divisionp (exp)
  (and (listp exp) (eq (car exp) '/)))

(defun make-sum (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((eql 0 a) b)
        ((eql 0 b) a)

        (t (list '+ a b))))

(defun make-difference (a b)
  (cond ((and (numberp a) (numberp b)) (- a b))
        ((eql 0 a) (list '- b))
        ((eql 0 b) (list '- a))

        ((negationp b)
         (list* '+ a (cdr b)))

        (t (list '+ a (make-negation b)))))

(defun make-negation (a)
  (cond ((numberp a) (- a))

        ((or (productp a) (divisionp a))
         (let ((pa (second a))
               (pb (third a)))
           (cond ((numberp pa) (list (first a) (make-negation pa) pb))
                 ((numberp pb) (list (first a) pa (make-negation pb)))
                 (t (list '- a)))))

        (t (list '- a))))

(defun make-product (a b)
  (cond ((and (numberp a) (numberp b)) (* a b))
        ((eql 0 a) 0)
        ((eql 0 b) 0)
        ((eql 1 a) b)
        ((eql 1 b) a)

        ((and (symbolp a) (symbolp b))
         (read-from-string (format nil "~a~a" a b)))

        (t (list '* a b))))

(defun make-division (a b)
  (cond ((and (numberp a) (numberp b)) (/ a b))
        ((eql 0 a) 0)
        (t (list '/ a b))))

(defun simplify (exp)
  (cond ((sump exp)
         (make-sum (simplify (second exp))
                   (simplify (third exp))))

        ((negationp exp)
         (make-negation (simplify (second exp))))
        
        ((differencep exp)
         (make-difference (simplify (second exp))
                          (simplify (third exp))))
        
        ((productp exp)
         (make-product (simplify (second exp))
                       (simplify (third exp))))
        
        ((divisionp exp)
         (make-division (simplify (second exp))
                        (simplify (third exp))))

        
        (t exp)))


(defun format-math-notation (var-name maff)
  (format t "~a = ~a~%~%" var-name maff))

(defun format-matrix (matrix)
  (let ((rlen (row-len matrix))
        (clen (col-len matrix)))
    (dotimes (row rlen)
      (format t (cond ((zerop row) "┏ ")
                      ((eql row (1- rlen)) "┗ ")
                      (t "┃ ")))
      (dotimes (col clen)
        (format t "~a " (get-atom-matrix matrix row col)))
      (format t (cond ((zerop row) "┓~%")
                      ((eql row (1- rlen)) "┛~%")
                      (t "┃~%"))))))

(defun prefix->infix-matrix (m)
  (mapcar (lambda (x) (mapcar (lambda (y) (prefix->infix y)) x)) m))

(defun eval-matrix (m)
  (mapcar (lambda (x) (mapcar (lambda (y) (eval y)) x)) m))

(defun matrix-algebra-method (eq1 eq2)
  (let* ((x `((,(nth 1 eq1))
              (,(nth 4 eq1))))
         
         (a `((,(nth 0 eq1) ,(nth 3 eq1))
              (,(nth 0 eq2) ,(nth 3 eq2))))
         
         (b `((,(nth 6 eq1))
              (,(nth 6 eq2))))
         
         (inv-a (get-inverse-matrix a))
         (determinate (get-determinant-matrix a))
         (inv-a*b (multiply-matrix inv-a b)))

    (format t "~a * ~a = ~a~%" a x b)
    (format-math-notation "Δ" (eval determinate))
    (format-math-notation "a⁻¹" (prefix->infix-matrix inv-a))
    (format-math-notation "a⁻¹*b" (prefix->infix-matrix inv-a*b))
    (format-matrix (eval-matrix inv-a*b))))



(defun cramers-method (eq1 eq2)
  (let* ((x `((,(nth 1 eq1))
              (,(nth 4 eq1))))
         
         (a `((,(nth 0 eq1) ,(nth 3 eq1))
              (,(nth 0 eq2) ,(nth 3 eq2))))
         
         (b `((,(nth 6 eq1))
              (,(nth 6 eq2))))
    
         (num `(((,(get-atom-matrix b 0 0) ,(get-atom-matrix a 0 1))
                 (,(get-atom-matrix b 1 0) ,(get-atom-matrix a 1 1)))
                
                ((,(get-atom-matrix a 0 0) ,(get-atom-matrix b 0 0))
                 (,(get-atom-matrix a 1 0) ,(get-atom-matrix b 1 0)))))
         
         (dem (eval (get-determinant-matrix a))))
    
    (loop :for var :in x
          :for ans :in num :do
            (format t "~a = determinate of ~a / ~a ~%= ~a / ~a~%~%"
                    (car var)
                    (format-matrix ans) dem
                    (get-determinant-matrix ans) dem))
    (format t "~%")
    (loop :for var :in x
          :for ans :in num :do
            (format t "~a = ~a~%"
                    (car var) (/ (eval (get-determinant-matrix ans)) dem)))))

(defun main ()
  (if (>= (length *posix-argv*) 2)
      (let* ((a (read-from-string (nth 1 *posix-argv*)))
             (b (read-from-string (nth 2 *posix-argv*)))
             (r (multiply-matrix a b)))
        (format t "a = ~%")
        (format-matrix a)
        (format t "~%b = ~%")
        (format-matrix b)
        (format t "~%a * b =~%")
        (format-matrix (prefix->infix-matrix r))
        (format t "=~%")
        (format-matrix (eval-matrix r)))
        (format t "please supply at least two arguments")))
