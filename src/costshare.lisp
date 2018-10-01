(defvar *bills* nil)
(defvar *payers* nil)

(defun test-script () ;ok, produces same results as c++ version
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

(defun test-script-2 () ;ok, same as above
  (setf *bills* nil)
  (setf *payers* nil)
  (make-payer "tarzan")
  (make-payer "jane")
  (make-payer "cheeta")
  (make-bill "foodstore" "jane")
  (make-bill "bongostore" "tarzan")
  (make-bill "sofastore" "jane")
  (make-item "bananas" "foodstore")
  (make-item "apples" "foodstore")
  (make-item "24packofbeer" "foodstore")
  (make-item "bongos" "bongostore")
  (make-item "kongas" "bongostore")
  (make-item "ocarina" "bongostore")
  (make-item "red-sofa" "sofastore")
  (make-item "blue-chair" "sofastore")
  (make-item "tv-set" "sofastore")
  (make-item "junk" "sofastore")
  (set-item-price "bananas" "foodstore" 5.5)
  (set-item-price "apples" "foodstore" 4)
  (set-item-price "24packofbeer" "foodstore" 22.75)
  (set-item-price "bongos" "bongostore" 99.90)
  (set-item-price "kongas" "bongostore" 150.00)
  (set-item-price "ocarina" "bongostore" 15)
  (set-item-price "red-sofa" "sofastore" 799.99)
  (set-item-price "blue-chair" "sofastore" 15)
  (set-item-price "tv-set" "sofastore" 500)
  (set-item-price "junk" "sofastore" 123.12)
  (set-weights-of-to nil 1)
  (set-all-item-weights "24packofbeer" "foodstore" '(4 2 18))
  (set-all-item-weights "red-sofa" "sofastore" '(0 1 1))
  (set-all-item-weights "blue-chair" "sofastore" '(0 1 1))
  (set-all-item-weights "tv-set" "sofastore" '(0 1 5))
  (set-all-item-weights "junk" "sofastore" '(1 7 1)))

(defun test-computation ()
  (compute-final-totals (compute-owes-totals (compute-bills))))

(defun cli-start ()
  (cli-make-payers)
  (cli-make-bills))

(defun cli-session ()
  (do () (nil)
    (format t "~&What would you like to do?")
    (format t "~& 1 : Make new payers")
    (format t "~& 2 : Make new bills")
    (if *payers* (progn (format t "~& 3 : Modify payers") (cli-warn-if-nonvalid *payers*)))
    (if *bills* (progn (format t "~& 4 : Modify bills") (cli-warn-if-nonvalid *bills*)))
    (if (and *payers* *bills* (data-validp)) (format t "~& 5 : Calculate"))
    (format t "~& 9 : Quit")
    (let ((input (parse-integer (input "input: ") :junk-allowed t))) ;Why parse integer?
      (cond
	((eql input 1) (cli-make-payers))
	((eql input 2) (cli-make-bills))
	((and (eql input 3) *payers*) (cli-payer-mode (cli-choose-payer)))
	((and (eql input 4) *bills*) (cli-bill-mode (cli-choose-bill)))
	((and (eql input 5) *bills* *payers*)
	 (cli-print-total)
	 )
	((eql input 9) (return-from cli-session))))))

(defun cli-warn-if-nonvalid (list)
  (unless (no-nilsp list)
    (format t " [ERRONEOUS DATA]")))

(defun cli-string-if-nonvalid (list)
  (if (no-nilsp list)
      ""
      " [ERRONEOUS DATA]"))

(defun cli-print-total ()
  (let ((colw 8)
	(totals (compute-final-totals (compute-owes-totals (compute-bills)))))
    (format t "~&PAYER   ")
    (dolist (bill *bills*) ;; print bills' names at top row
      (format t " : ~8a " (let ((bill-name (getf bill :name)))
			   (cli-string-cut bill-name colw))))
    (format t ": Total paid : Owes")
    (dolist (payer *payers*)
      (format t "~&~8a" (cli-string-cut (car payer) colw))
      (dolist (bill (getf totals :bills))
	(format t " : ~8a " (nth (payer-name-index (car payer)) (getf bill :total-shares))))
      (format t ": ~8a   " (nth (payer-name-index (car payer)) (getf totals :final-shares)))
      (format t ": ~8a" (nth (payer-name-index (car payer)) (getf totals :final-owes)))))
  (format t "~%~%"))
      

(defun cli-string-cut (string end)
  (let ((length (length string)))
    (subseq string 0 (if (> length end) end length))))

(defun cli-payer-mode (payer)
  (if (null payer) (return-from cli-payer-mode))
  (do () (nil)
    (format t "~&~s" (car payer))
    (format t "~&What would you like to do?")
    (format t "~& 1 : Rename")
    (format t "~& 2 : Delete")
    (format t "~& Other : Cancel")
    (let ((input (parse-integer (input "Input:") :junk-allowed t)))
      (cond ((eql input 1)
	     (setf (car payer)
		   (let ((new-name (input "New name: ")))
		     (if (and (payer-name-exists new-name) (not (equal new-name (car payer))))
			 (progn (format t "~&Someone with that name already exists!")
				(car payer))
			 new-name))))
	    ((eql input 2)
	     (if (y-or-n-p "Are you sure?")
		 (progn (delete-payer (car payer))
			(return))))
	    (t (return))))))

(defun cli-make-payers ()
  (do () (nil)
    (let ((name (input "Type in name of person (leave empty to proceed): ")))
      (cond ((zerop (length name))
	     (if (> (length *payers*) 0) (return t)))
	    (t (make-payer name))))))

(defun cli-display-indexes (list fun)
  (do ((element list (cdr element))
       (index 0 (1+ index)))
      ((not element))
    (format t "~&~A / ~A" index (funcall fun element))))

(defun cli-make-bills ()
  (do () (nil)
    (let ((name (input "Type in name of bill (leave empty to proceed): ")))
      (cond ((zerop (length name))
	     (if (> (length *bills*) 0) (return t)
		 (format t "~&At least one bill required!")))
	    (t (format t "~&Type index on this bill's payer.")
	       (let ((payer
		      (cli-choose-payer)))
		 (if payer
		     (make-bill name (car payer))
		     (format t "~&Faulty index given!"))))))))

(defun cli-choose-payer ()
  (format t "~&Choose payer")
  (cli-display-indexes *payers* #'caar)
  (let ((index (parse-integer (input "input: ") :junk-allowed t)))
    (if index (nth index *payers*))))

(defun cli-choose-bill ()
  (format t "~&Choose bill")
  (cli-display-indexes
   *bills* #'(lambda (x)
	       (let ((x (car x)))
		 (concatenate 'string
			      (cli-bill-header-to-string x)
			      (cli-string-if-nonvalid x)))))
  (let ((index (parse-integer (input "input: ") :junk-allowed t)))
    (if index (nth index *bills*))))

(defun cli-modify-bill ()
  (format t "Choose bill index to modify:")
  (cli-display-indexes
   *bills* #'(lambda (x)
	       (let ((x (car x))) (cli-bill-header-to-string x)
		    (cli-warn-if-nonvalid x))))
  (let* ((bill-index (parse-integer (input "Bill's index: ") :junk-allowed t)))
    (if bill-index (cli-bill-mode (nth bill-index *bills*)))))

;;;(defun cli-bill-mode (bill-name bill-index)
(defun cli-bill-mode (bill)
  (if (null bill) (return-from cli-bill-mode))
  (do () (nil)
    (format t "~&~a" (cli-bill-header-to-string bill))
    (format t "~&What would you like to do?")
    (format t "~& 1 : Make new items")
    (format t "~& 2 : Modify an item")
    (format t "~& 3 : Rename this bill")
    (format t "~& 4 : Change bill's payer")
    (format t "~& 5 : Delete this bill")
    (format t "~& other : Cancel")
    (let ((input (parse-integer (input "input: ") :junk-allowed t)))
      (cond ((eql input 1) (cli-make-items (getf bill :name)))
	    ((eql input 2) (cli-itemlist-mode (getf bill :name)))
	    ((eql input 3) (modify-bill-name bill (input "New name for bill: ")))
	    ((eql input 4) (modify-bill-payer bill (cli-choose-payer)))
	    ((eql input 5) (setf *bills* (delete-index-from-list (bill-index bill) *bills*))
	     (return))
	    (t (return))))))

(defun cli-bill-header-to-string (bill)
  (concatenate 'string
	       (getf bill :name) ", paid by "
	       (or (car (getf bill :payer)) "EMPTY")))

(defun cli-make-items (bill-name)
  (do () (nil)
    (let* ((item-name (input "Please input name of new item: "))
	   (item-price (progn (if (zerop (length item-name)) (return-from cli-make-items))
			      (parse-integer (input "Price: ") :junk-allowed t)))
	   (item-weights (cli-weight-input)))
      (make-item item-name bill-name)
      (set-item-price item-name bill-name item-price)
      (set-all-item-weights item-name bill-name item-weights))))

(defun cli-weight-input (&optional (payers *payers*))
  (cond (payers
	 (format t "~&~A's weight" (caar payers))
	 (cons (parse-integer (input " : ") :junk-allowed t)
	       (cli-weight-input (cdr payers))))))

(defun cli-itemlist-mode (bill-name)
  (do () (nil)
    (format t "~&Which item do you want to modify?")
    (let* ((bill (bill-name-exists bill-name))
	  (item-list (getf bill :items)))
      (cli-display-indexes item-list
			   #'(lambda (x)
			       (cli-print-item (car x))))
      (let ((index (parse-integer (input "Input: ") :junk-allowed t)))
	(cond ((not index)
	       (return nil))
	      ((and (>= index 0) (< index (length bill)))
	       (cli-item-mode bill item-list index))
	      (t (format t "~&Index out of bounds")))))))

(defun cli-item-mode (bill item-list index)
  (do () (nil)
    (let ((item (nth index item-list)))
      (format t "~&What do you want to do?")
      (format t "~& 1 : Modify name    : ~a" (getf item :name))
      (format t "~& 2 : Modify price   : ~a" (getf item :price))
      (format t "~& 3 : Modify weights : ~a" (getf item :weights))
      (format t "~& 4 : Delete this item")
      (format t "~& other : Cancel")
      (let ((input (input "input: ")))
	(cond ((equal input "1")
	       (modify-item-name item (input "New name: ")))
	      ((equal input "2")
	       (modify-item-price item
				  (parse-integer (input "New price: ") :junk-allowed t)))
	      ((equal input "3")
	       (setf (getf item :weights) (cli-weight-input)))
	      ((equal input "4")
	       (setf (getf bill :items) (delete-index-from-list index item-list))
	       (return))
	      (t (return)))))))

(defun cli-print-item (item)
  (concatenate 'string
	       (getf item :name) ", price: "
	       (write-to-string (getf item :price)) ", weights:"
	       (write-to-string (getf item :weights))))

(defun modify-bill-name (bill new-name)
  (if (bill-name-exists new-name)
      (format t "~&No duplicate bill names allowed!")
      (setf (getf bill :name) new-name)))

(defun modify-item-name (item new-name)
  (setf (getf item :name) new-name))

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

(defun modify-item-price (item new-price)
  (if (numberp new-price)
      (setf (getf item :price) new-price)))

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

(defun modify-bill-payer (bill payer)
  (setf (getf bill :payer) payer))

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

(defun bill-index (bill &optional (bill-list *bills*) (index 0))
  (cond ((null bill) nil)
	((eq bill (car bill-list)) index)
	(t (bill-index (cdr bill-list) (1+ index)))))

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
  (and (no-nilsp *payers*) (no-nilsp *bills*) *payers* *bills*))

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
		 :bills bill-list)))))
	
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
  (format *query-io* "~&~A" message)
  (force-output *query-io*)
  (read-line *query-io*))
