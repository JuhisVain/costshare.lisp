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
  (make-bill 'sofastore 'jane)
  (make-item 'bananas 'foodstore)
  (make-item 'apples 'foodstore)
  (make-item '24packofbeer 'foodstore)
  (make-item 'bongos 'bongostore)
  (make-item 'kongas 'bongostore)
  (make-item 'ocarina 'bongostore)
  (make-item 'red-sofa 'sofastore)
  (make-item 'blue-chair 'sofastore)
  (make-item 'tv-set 'sofastore)
  (make-item 'junk 'sofastore)
  (set-item-price 'bananas 'foodstore 5.5)
  (set-item-price 'apples 'foodstore 4)
  (set-item-price '24packofbeer 'foodstore 22.75)
  (set-item-price 'bongos 'bongostore 99.90)
  (set-item-price 'kongas 'bongostore 150.00)
  (set-item-price 'ocarina 'bongostore 15)
  (set-item-price 'red-sofa 'sofastore 799.99)
  (set-item-price 'blue-chair 'sofastore 15)
  (set-item-price 'tv-set 'sofastore 500)
  (set-item-price 'junk 'sofastore 123.12)
  (set-weights-of-to nil 1)
  (set-all-item-weights '24packofbeer 'foodstore '(4 2 18))
  (set-all-item-weights 'red-sofa 'sofastore '(0 1 1))
  (set-all-item-weights 'blue-chair 'sofastore '(0 1 1))
  (set-all-item-weights 'tv-set 'sofastore '(0 1 5))
  (set-all-item-weights 'junk 'sofastore '(1 7 1)))

(defun test-computation ()
  (compute-final-totals (compute-owes-totals (compute-bills))))
  
(defun make-bill (bill-name bill-payer-name)
  (let ((bill-payer (payer-name-exists bill-payer-name))) ;pointer to bill's payer in *payers*
    (if (and (not (bill-name-exists bill-name)) bill-payer)
	(push (list :name bill-name :payer bill-payer :items ()) *bills* )
	(format t "Unable to comply!~%"))))

(defun make-item (item-name in-bill-name)
  (let ((in-bill (bill-name-exists in-bill-name)))
    (if in-bill
	(push (list :weights (make-list (length *payers*)) :price nil :name item-name)
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

(defun set-bill-payer (payer-name bill-name)
  (let ((payer (payer-name-exists payer-name))
	(bill (bill-name-exists bill-name)))
    (if (and payer bill)
	(setf (getf bill :payer) payer))))


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

(defun do-for-bills (fun &optional (bills *bills*))
  (dolist (bill bills)
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

(defun compute-totals (computed-bill)
  (let ((share-list ()))
    (dolist (item computed-bill)
      (push (getf item :shares) share-list))
    (apply #'mapcar #'+ share-list)))

(defun compute-owes (total-bill-shares payer)
  (let* ((payer-index (apply #'payer-name-index payer))
	 (total-bill-owes
	  (do-to-index payer-index total-bill-shares
		       #'(lambda (x)
			   (* -1 (- (reduce #'+ total-bill-shares) x))))))
    total-bill-owes))

(defun compute-bill (bill)
  (let ((computed-bill ())) 
    (dolist (item (getf bill :items))
      (push (list :shares (compute-item item)
		  :name (getf item :name)
		  :price (getf item :price))
	    computed-bill))
    (reverse computed-bill)))
  
(defun compute-bills ()
  (cond ((data-validp)
	 (let ((bill-results ()))
	   (dolist (bill *bills*)
	     (push (list :name (getf bill :name)
			 :payer (getf bill :payer)
			 :items (compute-bill bill))
		   bill-results))
	   (reverse bill-results)))
	(t (format t "NIL values found in data.~%"))))

(defun data-validp ()
  (and (no-nilsp *payers*) (no-nilsp *bills*)))

(defun no-nilsp (tree)
  (cond ((atom tree) t)
	((not tree) nil)
	((and (car tree)
	      (no-nilsp (car tree))
	      (no-nilsp (cdr tree)))
	 t)))

(defun compute-owes-totals (computed-bills)
  (cond ((not computed-bills) nil)
	(t
	 (push (compute-totals (getf (car computed-bills) :items))
	       (car computed-bills))
	 (push :total-shares (car computed-bills))
	 (push (compute-owes (getf (car computed-bills) :total-shares)
			     (getf (car computed-bills) :payer))
	       (car computed-bills))
	 (push :total-owes (car computed-bills))
	 (cons (car computed-bills)
	       (compute-owes-totals (cdr computed-bills))))))

(defun compute-final-totals (bill-list)
  (cond ((not bill-list) nil)
	(t
	 (let ((all-total-shares (get-all-of-key :total-shares bill-list))
	       (all-total-owes (get-all-of-key :total-owes bill-list)))
	   (list :final-shares (apply #'mapcar #'+ all-total-shares)
		 :final-owes (apply #'mapcar #'+ all-total-owes)
		 bill-list)))))
	
(defun get-all-of-key (key list)
  (if list (cons (getf (car list) key) (get-all-of-key key (cdr list)))))

(defun do-to-index (index list fun)
  (cond ((eq list nil ) nil)
	((not (zerop index)) (cons (car list) (do-to-index (decf index) (cdr list) fun)))
	((zerop index)
	 (cons (funcall fun (car list))
	       (cdr list)))))

(defun do-to-all-but-index (index list fun)
  (cond ((eq list nil) nil)
	((not (zerop index))
	 (cons (funcall fun (car list))
	       (do-to-all-but-index (decf index) (cdr list) fun)))
	((zerop index)
	 (cons (car list)
	       (do-to-all-but-index (decf index) (cdr list) fun)))))



(defun input (message)
  (format *query-io* "~S:" message)
  (force-output *query-io*)
  (read-line *query-io*))
