(defun get-atom-matrix (matrix row col)
  (nth col (nth row matrix)))

(defun get-row-matrix (matrix row)
  (nth row matrix))

(defun get-col-matrix (matrix col)
  (mapcar (lambda (x) (nth col x)) matrix))

(defun row->col-matrix (matrix)
  "Flips rows and columns"
  (loop :for x :from 0 :to (1- (length (car matrix)))
        :collect (get-col-matrix matrix x)))

(defun square-matrix? (matrix)
  (= (length matrix) (length (car matrix))))

(defun nxn-matrix? (matrix n)
  (= n (length matrix) (length (car matrix))))

(defun row-len (matrix)
  (length matrix))

(defun col-len (matrix)
  (length (car matrix)))

(defun gen-matrix (matrix exclude-row exclude-col)
  (loop :for row :from 0 :to (1- (row-len matrix))
        :if (/= exclude-row row)
          :collect
          (loop :for col :from 0 :to (1- (col-len matrix))
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
          ((>= n (1- (row-len matrix))) (list (gen-atom matrix (1- (row-len matrix)))))
          (t  (let ((m (cons (list '- (gen-atom matrix n) (gen-atom matrix (1+ n)))
                             (get-determinant-matrix matrix (+ 2 n)))))
                (if (zerop n) (cons '+ m) m))))))
  

(defun .* (vec1 vec2)
  (cons '+ (mapcar (lambda (x y) `(* ,x ,y)) vec1 vec2)))

(defun multiply-matrix (matrix1 matrix2)
  (cond ((= (col-len matrix1)) (row-len matrix2)
         (loop :for row :in matrix1
               :collect
               (loop :for col :in (row->col-matrix matrix2)
                     :collect (.* row col))))
        (t nil)))

(defun get-inverse-matrix (matrix)
  (cond ((nxn-matrix? matrix 1) (get-atom-matrix matrix 0 0))
        ((square-matrix? matrix)
         (loop :for row :from 0 :to (1- (row-len matrix))
               :collect
               (loop :for col :from 0 :to (1- (col-len matrix))
                     :collect
                     `(* (/ ,(get-determinant-matrix matrix))
                         ,(list (if (evenp (+ row col)) '+ '-)
                                (get-determinant-matrix (gen-matrix matrix col row)))))))
        (t nil)))


(defun op? (exp)
  (or (eq exp '+)
      (eq exp '*)
      (eq exp '/)
      (eq exp '-)))

(defun inv? (exp)
  (if (and (eq (car exp) '/) (eql (length exp) 2)) '(1)))

(defun neg? (exp)
  (if (and (eq (car exp) '-) (eql (length exp) 2)) '(0)))

(defun prefix->infix (exp &optional op)
  (cond ((atom exp) exp)
        ((op? (car exp))
         (prefix->infix (append (inv? exp) (neg? exp) (cdr exp)) (car exp)))
        (t (let ((next (prefix->infix (cdr exp) op)))
             (cons (prefix->infix (car exp)) (if next (cons op next)))))))
    
(defun format-math-notation (var-name maff)
  (format t "~a = ~a~%~%" var-name maff))

(defun format-matrix (matrix)
  (format nil "~%~{┃~A┃~%~}" matrix))

(defun prefix->infix-matrix (m)
  (mapcar (lambda (x) (mapcar (lambda (y) (prefix->infix y)) x)) m))

(defun eval-matrix (m)
  (mapcar (lambda (x) (mapcar (lambda (y) (eval y)) x)) m))

(defun matrix-algebra-method (eq1 eq2)
  (let ((x `((,(nth 1 eq1))
             (,(nth 4 eq1))))
        
        (a `((,(nth 0 eq1) ,(nth 3 eq1))
             (,(nth 0 eq2) ,(nth 3 eq2))))
        
        (b `((,(nth 6 eq1))
             (,(nth 6 eq2)))))

    (let ((inv-a (get-inverse-matrix a))
          (determinate (get-determinant-matrix a)))
      (progn
        (format-math-notation "Δ" (eval determinate))
        (format-math-notation "a⁻¹" (prefix->infix inv-a)))
        ;;(format-math-notation "a⁻¹" (format-matrix (eval-matrix inv-a)))
        ;;(format t "~a = ~a * ~a~%~%" x (format-matrix (eval-matrix inv-a)) (format-matrix b))))

        (let ((inv-a*b (multiply-matrix inv-a b)))
          (progn
            (format-math-notation "a⁻¹*b" (prefix->infix inv-a*b))
            (format-matrix inv-a*b)
            ;;(loop :for var :in x
            ;;      :for ans :in (eval-matrix inv-a*b) :do
            ;;    (format t "~a = ~a~%" (car var) (car ans)))))))))
            )))))


(defun cramers-method (eq1 eq2)
  (let ((x `((,(nth 1 eq1))
             (,(nth 4 eq1))))

        (a `((,(nth 0 eq1) ,(nth 3 eq1))
             (,(nth 0 eq2) ,(nth 3 eq2))))
        
        (b `((,(nth 6 eq1))
             (,(nth 6 eq2)))))
    
    (let ((num `(((,(get-atom-matrix b 0 0) ,(get-atom-matrix a 0 1))
                  (,(get-atom-matrix b 1 0) ,(get-atom-matrix a 1 1)))
                 
                 ((,(get-atom-matrix a 0 0) ,(get-atom-matrix b 0 0))
                  (,(get-atom-matrix a 1 0) ,(get-atom-matrix b 1 0)))))
          (dem (eval (get-determinant-matrix a))))
      (progn
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
                        (car var) (/ (eval (get-determinant-matrix ans)) dem)))))))

(defun main ()
  (if (>= (length *posix-argv*) 2)
    (format t "~a~%" (multiply-matrix (get-inverse-matrix (read-from-string (nth 1 *posix-argv*))) (read-from-string (nth 2 *posix-argv*))))
    nil))
(main)
