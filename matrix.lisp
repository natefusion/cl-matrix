(defpackage :matrix
  (:use :cl))

(in-package :matrix)

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

;; (defun pretty-print (exp)
;;   (loop with op = (first exp)
;;         with len = (length exp)
        
;;         for subexp in (rest exp)
;;         for index from 0
;;         with result = (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
        
;;         do (case op
;;              ((exp) (vector-push-extend #\e result) (vector-push-extend #\^ result))
;;              (t (error "pretty-print: case op")))
;;            (typecase subexp
;;              (number (vector-push-extend (digit-char subexp) result))
;;              (symbol (vector-push-extend (char-downcase (char (string subexp) 0)) result))
;;              (list (loop initially (vector-push-extend #\( result)
;;                          for x across (pretty-print subexp)
;;                          do (vector-push-extend x result)
;;                          finally (vector-push-extend #\) result))))
;;            (when (or (<= len 2) (< index (- len 2)))
;;              (case op
;;                ((*))
;;                ((expt) (vector-push-extend #\^ result))
;;                (t (vector-push-extend (char-downcase (char (string op) 0)) result)) ))
                         
;;         finally (return result)))

(defun sump (exp)
  (and (listp exp) (eql (car exp) '+)))

(defun differencep (exp)
  (and (listp exp) (eql (car exp) '-)))

(defun negationp (a)
  (or (and (numberp a) (< a 0))
      (and (differencep a) (null (third a)))))

(defun productp (exp)
  (and (listp exp) (eql (car exp) '*)))

(defun divisionp (exp)
  (and (listp exp) (eq (car exp) '/)))

(defun funp (exp)
  (and (listp exp) (eq (car exp) 'funcall)))

(defun powp (exp)
  (and (listp exp) (eq (car exp) 'expt)))

(defun expp (exp)
  (and (listp exp) (eq (car exp) 'exp)))

(defun lnp (exp)
  (and (listp exp) (eq (car exp) 'ln)))

(defun sinp (exp)
  (and (listp exp) (eq (car exp) 'sin)))

(defun cosp (exp)
  (and (listp exp) (eq (car exp) 'cos)))

(defun tanp (exp)
  (and (listp exp) (eq (car exp) 'tan)))

(defun asinp (exp)
  (and (listp exp) (eq (car exp) 'asin)))

(defun acosp (exp)
  (and (listp exp) (eq (car exp) 'acos)))

(defun atanp (exp)
  (and (listp exp) (eq (car exp) 'atan)))

(defun atrigp (exp)
  (or (asinp exp) (acosp exp) (atanp exp)))

(defun ln (number)
  (typecase number
    (number (log number (exp 1)))
    (t `(function ln ,number))))

(defun flatten (lst rule)
  (labels ((rflatten (lst1 acc)
             (dolist (el lst1)
               (if (funcall rule el)
                   (setf acc (rflatten (cdr el) acc))
                   (push el acc)))
             acc))
    (reverse (rflatten (cdr lst) nil))))

(defun exp-equal (x y)
  (cond ((and (atom x) (atom y)) (eql x y))
        ((and (productp x) (productp y))
         (let ((a (flatten x #'productp))
               (b (flatten y #'productp)))
           (not (set-exclusive-or a b))))
        ((and (sump x) (sump y))
         (let ((a (flatten x #'sump))
               (b (flatten y #'sump)))
           (not (set-exclusive-or a b))))
        ((and (differencep x) (differencep y))
         (and (exp-equal (second x) (second y))
              (exp-equal (third x) (third y))))))

(defun make-product (a b)
  (cond ((or (eql 0 a) (eql 0 b)) 0)
        ((eql 1 a) b)
        ((eql 1 b) a)
        ((and (numberp a) (numberp b)) (* a b))

        ((and (numberp a) (productp b))
         (let ((b1 (second b))
               (b2 (third b)))
           (if (numberp b1)
               (make-product (* a b1) b2)
               (list '* a b))))

        ((and (eql a b) (symbolp a))
         (list 'expt a 2))

        ((and (powp a) (powp b))
         (if (eql (second a) (second b))
             (list 'expt (second a) (+ (third a) (third b)))
             (list '* a b)))

        ((and (powp a) (eql (second a) b))
         (list 'expt b (1+ (third a))))

        ((and (productp a) (productp b))
         (if (and (numberp (second a)) (numberp (second b)))
             (make-product (* (second a) (second b))
                           (make-product (third a) (third b)))
             (list '* a b)))

        ((productp a)
         (let ((a1 (second a))
               (a2 (third a)))
           (if (numberp a1)
               (list '* a1 (make-product a2 b))
               (list '* b a))))

        ((productp b)
         (let ((b1 (second b))
               (b2 (third b)))
           (if (numberp b1)
               (list '* b1 (make-product b2 a))
               (list '* a b))))

        ((divisionp a)
         (let ((a1 (second a))
               (a2 (third a)))
           (make-division (make-product b a1) a2)))

        ((divisionp b)
         (let ((b1 (second b))
               (b2 (third b)))
           (make-division (make-product a b1) b2)))
        
        ((numberp a) (list '* a b))
        (t (list '* b a))))

(defun make-sum (a b)
  (cond ((eql 0 a) b)
        ((eql 0 b) a)
        ((and (numberp a) (numberp b)) (+ a b))
        ((and (negationp b) (exp-equal a (second b))) 0)
        ((exp-equal a b) (make-product 2 a))

        ((and (productp a) (productp b))
         (let ((a1 (second a))
               (a2 (third a))
               (b1 (second b))
               (b2 (third b)))
           (cond ((and (numberp a1) (numberp b1) (exp-equal a2 b2))
                  (make-product (+ a1 b1) a2))
                 
                 ((and (numberp a1) (exp-equal a2 b))
                  (make-product (1+ a1) b))

                 ((and (numberp b1) (exp-equal b2 a))
                  (make-product (1+ b1) a))
                 
                 (t (list '+ a b)))))

        ((productp a)
         (let ((a1 (second a))
               (a2 (third a)))
           (if (exp-equal a2 b)
               (make-product (make-sum 1 a1) b)
               (list '+ a b))))

        ((and (divisionp a) (divisionp b)
              (exp-equal (third a) (third b)))
         (make-division (make-sum (second a) (second b)) (third a)))
        
        (t (list '+ a b))))

(defun make-negation (a)
  (cond ((numberp a) (- a))
        ((and (differencep a) (null (third a))) (second a))
        ((and (productp a) (negationp (second a)))
         (make-product (make-negation (second a)) (third a)))
        ((and (productp a) (negationp (third a)))
         (make-product (second a) (make-negation (third a))))
        (t (list '- a))))

(defun make-difference (a b)
  (cond ((null b) (make-negation a))
        ((eql 0 a) (make-negation b))
        ((eql 0 b) a)
        ((eql a b) 0)
        ((and (numberp a) (numberp b)) (- a b))
        ((and (differencep a)
              (eql (second a) b))
         (make-negation (third a)))
        ((exp-equal a b) 0)
        ((productp b) (make-sum a (make-negation b)))
        (t (list '- a b))))

(defun make-division (a b)
  (cond ((and (numberp a) (numberp b)) (/ a b))
        ((eql 0 a) 0)
        ((eql 1 b) a)
        ((eql -1 b) (make-negation a))
        ((exp-equal a b) 1)
        
        ((and (productp a) (productp b))
         (let* ((a1 (second a))
                (a2 (third a))
                (b1 (second b))
                (b2 (third b)))
           (cond ((exp-equal a1 b1) (make-division a2 b2))
                 ((exp-equal a1 b2) (make-division a2 b1))
                 ((exp-equal a2 b1) (make-division a1 b2))
                 ((exp-equal a2 b2) (make-division a1 b1))
                 (t (list '/ a b)))))
        
        ((productp a)
         (let ((a1 (second a))
               (a2 (third a)))
           (cond ((exp-equal a1 b) a2)
                 ((exp-equal a2 b) a1)
                 (t (list '/ a b)))))

        ((productp b)
         (let ((b1 (second b))
               (b2 (third b)))
           (cond ((exp-equal b1 a) (make-division 1 b2))
                 ((exp-equal b2 a) (make-division 1 b1))
                 (t (list '/ a b))))) 
        (t (list '/ a b))))

(defun simplify (exp)
  (cond ((sump exp)
         (make-sum (simplify (second exp))
                   (simplify (third exp))))
        ((differencep exp)
         (make-difference (simplify (second exp))
                          (simplify (third exp))))
        
        ((productp exp)
         (make-product (simplify (second exp))
                       (simplify (third exp))))
        ((divisionp exp)
         (make-division (simplify (second exp))
                        (simplify (third exp))))

        ((lnp exp) (if (expp (second exp))
                       (second (second exp))
                       exp))

        ((powp exp) (if (expp (second exp))
                        `(exp ,(third exp))
                        exp))
        ((funp exp)
         (list (first exp) (second exp) (simplify (third exp))))
        (t exp)))

(defun diff (wrt exp)
  (cond ((null exp) nil)
        ((numberp exp) 0)
        ((symbolp exp) (if (eql wrt exp) 1 0))
        ((sump exp)
         (make-sum (diff wrt (second exp)) (diff wrt (third exp))))
        ((differencep exp)
         (make-difference (diff wrt (second exp)) (diff wrt (third exp))))
        ((powp exp)
         (if (eql (second exp) 'e)
             (make-product (diff wrt (third exp)) exp)
             (error "did not implement x^n other than e^n")))
        ((funp exp)
         (case (second (second exp))
           ((ln) (make-product (diff wrt (third exp)) (make-division 1 (third exp))))
           ((sin) `(funcall 'cos ,(third exp)))
           ((cos) `(- (funcall 'sin ,(third exp))))
           (t (error "Tried to differentiate unknown function: ~a" (second exp)))))
        ((productp exp)
         (let ((f (second exp))
               (g (third exp)))
           (make-sum (make-product (diff wrt f) g)
                     (make-product f (diff wrt g)))))
        ((divisionp exp)
         (let ((f (second exp))
               (g (third exp)))
           (make-division (make-difference (make-product (diff wrt f) g)
                                           (make-product f (diff wrt g)))
                          (make-product g g))))


        (t (error "wot: ~a" exp))))

(defun indefinite-integral (wrt exp)
  (let ((exp (simplify exp)))
    (cond ((null exp) nil)
          ((numberp exp) (make-product exp wrt))
          ((symbolp exp) (if (eql wrt exp)
                             (make-division (make-product exp exp) 2)
                             (make-product exp wrt)))
          ((sump exp) (make-sum (indefinite-integral wrt (second exp)) (indefinite-integral wrt (third exp))))
          ((differencep exp) (make-difference (indefinite-integral wrt (second exp)) (indefinite-integral wrt (third exp))))
          ((expp exp)
           (simplify `(/ ,exp ,(diff wrt (second exp)))))
          ((powp exp)
           (let ((base (second exp))
                 (power (third exp)))
             (if (eql base wrt)
                 (simplify `(/ (expt ,base (+ ,power 1)) (+ ,power 1)))
                 (simplify `(/ ,exp (* ,(diff wrt power) (ln ,base)))))))
          ((productp exp)
           (let (u du/dx vdx)
             (flet ((function-order (func)
                      (typecase func
                        (list (cond
                                ;; arcsin, arccos, arctan
                                ((atrigp func) 0)
                                ;; ln, log
                                ((or (eql (first func) 'ln) (eql (first func) 'log)) 1)
                                ;; x^1, x^2, ...
                                ((and (powp func) (eql (third func) wrt)) 2)
                                ;; sin, cos, tan
                                ((trigp func) 3)
                                ;; e^x, 3^x, ...
                                ((powp func) 4)))
                        ;; x
                        (t 2))))
               (if (< (function-order (second exp)) (function-order (third exp)))
                   (setf u (second exp)
                         du/dx (diff wrt (second exp))
                         vdx (indefinite-integral wrt (third exp)))
                   (setf u (third exp)
                         du/dx (diff wrt (third exp))
                         vdx (indefinite-integral wrt (second exp))))
               (make-difference (make-product u vdx) (indefinite-integral wrt (make-product du/dx vdx)))))))))

(defun d/dx (exp) (diff 'x exp))
(defun d/dy (exp) (diff 'y exp))
(defun d/dz (exp) (diff 'z exp))

(defparameter *nabla* (list #'d/dx #'d/dy #'d/dz))

(defun x* (v1 v2)
  (get-determinant-matrix `((i j k) ,v1 ,v2)))

(defparameter *binary-operators* '((+ 1) (- 1) (* 2) (/ 2) (funcall 3) (expt 3)))
(defparameter *unary-operators* '((+ 4) (- 4) (quote 4) (exp 4)))

(defun weight (c) (second (assoc c *binary-operators*)))
(defun binary-opcode (c) (first (assoc c *binary-operators*)))
(defun unary-opcode (c) (first (assoc c *unary-operators*)))

(defun inf-iter (exp operators operands)
  (cond ((and (null exp) (null operators))
         (first operands))

        ;; implicit multiplication
        ((and exp (or (listp (first exp))
                      (null (weight (first exp)))))
         (inf-iter (cons '* exp) operators operands))

        ((and exp (or (null operators)
                      (> (weight (first exp)) (weight (first operators)))))
         (inf-aux (rest exp) (cons (first exp) operators) operands))

        (t
         (inf-iter exp (rest operators)
                   (cons (list (binary-opcode (first operators))
                               (cadr operands) (first operands))
                         (cddr operands))))))

(defun inf-aux (exp operators operands)
  (if (and (atom (first exp)) (assoc (first exp) *unary-operators*))
      (inf-iter (cddr exp) operators
                (cons (list (unary-opcode (first exp))
                            (infix->prefix (cadr exp)))
                      operands))
      (inf-iter (rest exp) operators (cons (infix->prefix (first exp)) operands))))

(defun infix->prefix (exp)
  (if (atom exp)
      exp
      (inf-aux exp nil nil)))

(defun recursive-reverse (l)
  (when l (append (recursive-reverse (cdr l))
                  (list (if (listp (car l))
                            (recursive-reverse (car l))
                            (car l))))))

;; (defun notation (exp)
;;   (let (final
;;         stack
;;         variables
;;         (number (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
;;         (func (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
;;         (special nil))
;;     (flet ((push-number ()
;;              (when (> (length number) 0)
;;                (push (read-from-string number) final)
;;                (loop repeat (length number) do (vector-pop number)))))
;;       (loop for x across exp
;;             do (cond ;; (special
;;                      ;;  (case x
;;                      ;;    ((#\`)
;;                      ;;     (setf special nil)
;;                      ;;     (push 'quote final)
;;                      ;;     (push (read-from-string func) final)
;;                      ;;     (push 'funcall final)
;;                      ;;     (loop repeat (length func) do (vector-pop func)))
;;                      ;;    (t (vector-push-extend x func))))
;;                      ((digit-char-p x) (vector-push-extend x number))
;;                      ((alpha-char-p x) (push (read-from-string (string x)) final) (pushnew x variables))
;;                      (t (push-number)
;;                         (case x
;;                           ;; ((#\`) (setf special t))
;;                           ((#\+) (push '+ final))
;;                           ((#\-) (push '- final))
;;                           ((#\*) (push '* final))
;;                           ((#\/) (push '/ final))
;;                           ((#\^) (push 'expt final))
;;                           ((#\() (push final stack) (setf final nil))
;;                           ((#\)) (push final (first stack)) (setf final (pop stack)))
;;                           ((#\space))
;;                           (t (error "wot in tarnation is '~a' doing here" x)))))
;;             finally (push-number)
;;                     (return (infix->prefix (recursive-reverse final)))))))

(defun notation (exp)
  (let (final stack variables)
    (loop for x across exp
          do (cond ((char= x #\e) (push 'exp final) (push 1 final))
                   ((digit-char-p x) (push (read-from-string (string x)) final))
                   ((alpha-char-p x) (push (read-from-string (string x)) final) (pushnew x variables))
                   (t (case x
                        ((#\+) (push '+ final))
                        ((#\-) (push '- final))
                        ((#\*) (push '* final))
                        ((#\/) (push '/ final))
                        ((#\^) (push 'expt final))
                        ((#\() (push final stack) (setf final nil))
                        ((#\)) (push final (first stack)) (setf final (pop stack)))
                        ((#\space))
                        (t (error "wot in tarnation is '~a' doing here" x)))))
          finally (return (infix->prefix (recursive-reverse final))))))

(defun equation (e1 e2)
  (let ((e1 (simplify e1))
        (e2 (simplify e2)))
    (cond ((exp-equal e1 e2) t)
          (t nil))))

;; (defun laplace (f)
;;   (integral 't (simplify (* (exp (* -1 s t)) f)) 0 ∞))

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
  (if (>= (length sb-ext:*posix-argv*) 2)
      (let* ((a (read-from-string (nth 1 sb-ext:*posix-argv*)))
             (b (read-from-string (nth 2 sb-ext:*posix-argv*)))
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
