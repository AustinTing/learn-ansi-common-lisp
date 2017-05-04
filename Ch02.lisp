;;2.6

;; 全真，返回最後一個
(and 'a 'b (+ 1 2) 'c 'd)	       	;D
;; 有一假，立刻停止求值，返回nil
(and 'a 'b (+ 1 2) 'c (> 1 2) 'd)	;nil

;; 有一真，立刻停止求值，返回真的那個
(or 'a 'b (+ 1 2) 'c (> 1 2) 'd)	;A
;; 全假，返回nil
(or (> 1 2) (> 2 3))			;nil

;;2.7
(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(defun ask-number()
  (format t "Enter number~%")
  (let ((val (read)))
    (if (numberp val)
	(format t "yes")
	(ask-number))))

(defun my-show-squares (start end)
  (do ((i start (+ i 1)))		;(var init (finish one round give this value to var))
      ((> i end)			;(if true (in final), execute next line to the end, and return the last one)
       (format t "will I be printed every time?~%")
       (format t "NO! ,you will be printed in the final."))
    (format t "~a ~a~%" i (* i i))))	;(function body)

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (format t "~a~%" obj)
      (setf len (+ len 1)))
    len))
;; exercises
;; 1. d.
(list (and (listp 3 ) T) (+ 1 2))
;; 2.
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
;; 3.
(defun my-fourth (lst)
  (car (cdr (cdr (cdr lst)))))
;; 4.
(defun my-bigger (first-test-number second-test-number)
  (if (> first-test-number second-test-number)
      first-test-number
      second-test-number))
;; 5.a
(defun nil-in-list-p (x)
  (and (not (null x))
       (or (null (car x))
	   (nil-in-list-p (cdr x)))))
;; 5.b
(defun where-is-x (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (where-is-x x (cdr y))))
	    (and z (+ z 1))))))
;; 7
(defun list-in-list-p (x)
  (if x
      (if (listp (car x))
	  T
	  (list-in-list-p (cdr x)))
      nil))
;; 7 other answer. Use 'or', 'and' can pure the code  
(defun nest-p (ls)
  (if ls
      (or (listp (car ls))
          (nest-p (cdr ls)))))
;; 8.a
(defun print-point-loop (num)
  (do ((i 1 (+ i 1)))
      ((> i num))
    (format t ".")))
(defun print-point-recursive (num)
  (if (< num 1)
      nil
      (progn
	(format t ".")
	(print-point-recursive (- num 1)))))
;; 8.b
(defun how-many-a-loop (ls)
  (let ((num-a 0))
    (dolist (obj ls)
      (if (eql obj 'a)
	  (setf num-a (+ num-a 1))))
    num-a))
(defun how-many-a-recursive (ls)
  (if (null ls)
      0
      (if (eql (car ls) 'a)
	  (+ 1 (how-many-a-recursive (cdr ls)))
	  (how-many-a-recursive (cdr ls)))))
;; 9.a
(defun summit-a (lst)
  (setf lst (remove nil lst))		;remove 不會set
  (apply #'+ lst))
;; 9.b ??
(defun summit-b (lst)
  (let ((x (car lst)))
    (if (null x)
	(summit (cdr lst))
	(+ x (summit (cdr lst))))))
