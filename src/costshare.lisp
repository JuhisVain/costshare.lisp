(defvar *bills* nil)
(defvar *payers* nil)

(defun test-script ()
  (setf *bills* nil)
  (setf *payers* nil)
  (make-payer 'tarzan)
  (make-payer 'jane)
  (make-payer 'cheeta)
  (make-bill 'foodstore 'jane)
  (make-bill 'bongostore 'tarzan)
  (make-item 'bananas 'foodstore)
  (make-item 'apples 'foodstore)
  (make-item '24packofbeer 'foodstore)
  (make-item 'bongos 'bongostore)
  (make-item 'kongas 'bongostore)
  (make-item 'ocarina 'bongostore)
  (set-item-price 'bananas 'foodstore 5.5)
  (set-item-price 'apples 'foodstore 4)
  (set-item-price '24packofbeer 'foodstore 22.75)
  (set-item-price 'bongos 'bongostore 99.90)
  (set-item-price 'kongas 'bongostore 150.00)
  (set-item-price 'ocarina 'bongostore 15)
  (set-weights-of-to nil 1)
  (set-all-item-weights '24packofbeer 'foodstore '(4 2 18)))
  
(defun make-bill (bill-name bill-payer-name)
  (let ((bill-payer (payer-name-exists bill-payer-name))) ;pointer to bill's payer in *payers*
    (if (and (not (bill-name-exists bill-name)) bill-payer)
	(push (list :name bill-name :payer bill-payer :items ()) *bills* )
	(format t "Unable to comply!~%"))))

(defun make-item (item-name in-bill-name)
  (let ((in-bill (bill-name-exists in-bill-name)))
    (if in-bill
	(push (list :weights (make-list (length *payers*)) :price 0 :name item-name)
	      (getf in-bill :items))
	(format t "There is no bill named ~S~%" in-bill-name))))

(defun find-item-from-bill (item-name bill-name)
  (dolist (element (getf (bill-name-exists bill-name) :items))
	(if (equal item-name (getf element :name)) (return element))))

(defun set-all-item-weights (item-name bill-name weights)
  (if (equal (length weights) (length *payers*))
      (let ((item-weights (find-item-from-bill item-name bill-name)))
	(setf (getf item-weights :weights) weights))
  (format t "Wrong length of weights!~%")))

(defun set-item-price (item-name bill-name price)
  (let ((item (find-item-from-bill item-name bill-name)))
    (if item
	(setf (getf item :price) price))))

(defun make-payer (payer-name)
  (cond ((payer-name-exists payer-name)
	 (format t "Payer with name ~S already exists!~%" payer-name))
	(t
	 (do-for-all-items #'(lambda (x)
			       (setf (getf x :weights) (cons nil (getf x :weights)))))
	 (push (list payer-name) *payers*))))

(defun delete-payer (payer-name)
  (let ((index (payer-name-index payer-name)))
    (cond (index
	   (dolist (bill *bills*) ;make bill's payers empty
	     (if (equal (getf bill :payer) (payer-name-exists payer-name))
		 (setf (getf bill :payer) nil)))
	   (setf *payers* (delete-index-from-list index *payers*))
	   (do-for-all-items
	       #'(lambda (x)
		   (setf (getf x :weights)
			 (delete-index-from-list index (getf x :weights))))))
	  (t nil))))

;;Returns list with whatever was at index missing. If index > length fills with NIL.
(defun delete-index-from-list (index list)
  (cond ((zerop index)
	 (cdr list))
	(t
	 (cons (car list)
	       (delete-index-from-list (decf index) (cdr list))))))

(defun set-weights-of-to (of to)
  (do-for-all-items #'(lambda (x)
			(setf (getf x :weights) (substitute to of (getf x :weights))))))

(defun do-for-bills (fun)
  (dolist (bill *bills*)
    (funcall fun bill)))

(defun do-for-items-in-bill (bill fun)
  (dolist (item (getf bill :items))
    (funcall fun item)))

(defun do-for-all-items (fun)
  (dolist (bill *bills*)
    (dolist (item (getf bill :items))
      (funcall fun item))))

(defun payer-name-exists (bill-payer-name)
  (car
   (member bill-payer-name *payers* :test #'(lambda (x y)
					      (string= x (car y))))))

;;returns index (0..n) of payer-name within *payers*. NIL if not found.
(defun payer-name-index (payer-name &optional (payers-list *payers*) (index 0))
  (cond ((eql (caar payers-list) payer-name)
	 index)
	((not (caar payers-list))
	 nil)
	(t
	(payer-name-index payer-name (cdr payers-list) (incf index)))))

(defun bill-name-exists (bill-name)
  (car
   (member bill-name *bills* :test #'(lambda (x y)
				       (string= x (getf y :name))))))

(defun compute-item (item)
  (let ((total-weight (reduce #'+ (getf item :weights)))
	(price (getf item :price)))
    (mapcar #'(lambda (x)
		(/ (* x price) total-weight))
	    (getf item :weights))))

(defun compute-bill (bill)
  (let ((computed-bill ())) 
    (dolist (item (getf bill :items))
      (push (list :shares (compute-item item) :name (getf item :name)) computed-bill))
    computed-bill))
    
(defun compute-bills ()
  (let ((bill-results ()))
    (dolist (bill *bills*)
      (push (list :items (compute-bill bill) :name (getf bill :name)) bill-results))
    bill-results))

;;(defun compute-all ()
;;  (let ))

(defun input (message)
  (format *query-io* "~S:" message)
  (force-output *query-io*)
  (read-line *query-io*))
