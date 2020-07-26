(defun buffer-eof-p ()
  "Return t if (point) points to EOF.
nil otherwise."
  (> (point) (buffer-size)))

(defun peek ()
  "Return the single-byte string on (point)."
  (buffer-substring (point) (1+ (point))))

(defun peek-ahead ()
  "Return the single-byte string ahead of (point)."
  (let ((point (1+ (point))))
	(buffer-substring point (1+ point))))

(defun skip-whitespaces ()
  "Ignore whitespaces, jumping to next valid character"
  (skip-chars-forward "[:space:]"))

(defun parse-string ()
  "Parse string with (point) pointing to the opening quote"
  (let ((quote-char (peek))
		(parsed-string ""))
	(forward-char 1)
	(while (and (not (string= (peek) quote-char)))
	  ;; check if there is a backslashed quote
	  ;; and ignore it to not parse the end of string
	  (if (and (string= (peek) "\\")
			   (string= (peek-ahead) quote-char))
		  (progn
			(setq parsed-string (concat parsed-string (peek)))
			(forward-char 1)))
	  ;; concatenate the character on the result string
	  (setq parsed-string (concat parsed-string (peek)))
	  (forward-char 1))
	(skip-chars-forward quote-char)
	parsed-string))

(defun parse-number ()
  "Parse number on JSON"
  (let ((valid-nums "1234567890.")
		(num-str ""))
	;; check if character is number or dot
	(while (string-match-p (regexp-quote (peek)) valid-nums)
	  (setq num-str (concat num-str (peek)))
	  (forward-char 1))
	(string-to-number num-str)))

(defun parse-array ()
  "Parse array with (point) pointing to '['"
  (let ((array-begin "[")
		(array-end "]")
		(parsed-array '()))
	(skip-chars-forward array-begin)
	(skip-whitespaces)
	(while (not (string= (peek) array-end))
	  (if (string= (peek) ",")
		  (progn
			(skip-chars-forward ",") ;; jump past ','
			(skip-whitespaces)))
	  (setq parsed-array
			(append parsed-array (list (parse-token))))
	  (skip-whitespaces))
	(skip-chars-forward array-end)
	parsed-array))

(defun parse-boolean ()
  "Parse boolean value ('true' or 'false')"
  (let ((return-value '()))
  (if (string= (buffer-substring (point) (+ (point) 5)) "false")
		(setq return-value nil))
  (if (string= (buffer-substring (point) (+ (point) 4)) "true")
		(setq return-value t))
  (if (eq return-value nil)
	  (forward-char 5)
	(forward-char 4))
  return-value))


(defun parse-null ()
  (if (string= (buffer-substring (point) (+ (point) 4)) "null")
	  (progn
		(forward-char 4)
		'())))


(defun parse-token ()
  "Parse value from JSON. Values can be either strings,
array, jsons, numbers or booleans"
  ;; decides which path to take based on first character
  (cond ((or (string= (peek) "\"")
			 (string= (peek) "'"))
		 (parse-string))
		((string= (peek) "{") (parse-json))
		((string= (peek) "[") (parse-array))
		((string-match-p (regexp-quote (peek)) "1234567890.") (parse-number))
		((or (string= (peek) "f") (string= (peek) "t")) (parse-boolean))
		((string= (peek) "n") (parse-null))))


(defun parse-json ()
  "Parse JSON from given string."
  (let ((json-begin "{")
		(json-end "}")
		(parsed-json '()))
	(if (string= (peek) json-begin)
		(progn
		  (forward-char 1)
		  (while (and (not (buffer-eof-p))
					  (not (string= (peek) json-end)))
			(skip-whitespaces)
			(let ((parsed-key (parse-token)))
			  (skip-whitespaces)
			  (skip-chars-forward ":") ;; jumping past ':'
			  (skip-whitespaces)
			  (let ((parsed-value (parse-token)))
				;; create dotted pair and append to json
				(setq parsed-json
					  (append parsed-json
							  (list (cons parsed-key parsed-value))))

				;; go past ',' if there is one
				(if (string= (peek) ",")
					(forward-char 1))
				(skip-whitespaces))))))
	(forward-char 1)
	parsed-json))

(defun parse-json-from-file (file-path)
  "Parse JSON that is inside a file."
  (with-temp-buffer
	(insert-file-contents file-path)
	(goto-char 1)
	(parse-json)))

(defun parse-json-from-string (string)
  "Parse JSON from given string."
  (with-temp-buffer
	(insert string)
	(goto-char 1)
	(parse-json)))
