(defvar uninteresting-commands nil)

;; "borrowed" from doom emacs
(defmacro appendq! (sym &rest lists)
  "Append LISTS to SYM in place."
  `(setq ,sym (append ,sym ,@lists)))

(defun concat-list (L)
  (let ((result '()))
    (dolist (e L)
      (appendq! result e))
    result))

(defun process (keymap)
  (cond
   ((eq 'keymap
        (car keymap))
    (process
     (cdr keymap)))
   ((stringp
     (car keymap))
    (process
     (cdr keymap)))
   ((eq 'remap
        (car keymap))
    nil)
   (t keymap)))

(defun top-level-keybinds ()
  (let ((all (seq-filter (lambda (x) (and (listp x)
                                          (commandp (cdr x))
                                          (not (commandp (car x)))))
                         (concat-list (mapcar #'process (current-active-maps)))))
        (seen (make-hash-table))
        (result '()))
    (dolist (pair all)
      (unless (gethash (car pair) seen)
        (puthash (car pair) t seen)
        (push pair result)))
    (mapcar (lambda (pair) (cons (key-description (vector (car pair)))
                                 (cdr pair)))
            result)))

(top-level-keybinds)

(defun is-interesting-p (pair)
  (let ((key (car pair))
        (command (cdr pair)))
    (not
     (or (eq 'digit-argument command)
         (eq 'negative-argument command)
         (cl-search "mouse" key)
         (and (symbolp command)
              (or (cl-search "mouse" (symbol-name command))
                  (cl-search "mwheel" (symbol-name command))
                  (member (symbol-name command) uninteresting-commands)))))))
(defun lex< (list1 list2)
  "Lexicographically compare two shallow lists of strings and numbers"
  (cond ((and (equal list1 nil) 
              (equal list2 nil))
         nil)
        ((eq list1 nil)
         t)
        ((eq list2 nil)
         nil)
        ((and (numberp list1)
              (numberp list2))
         (cond
          ((< list1 list2)
           t)
          (t nil)))
        ((and (stringp list1)
              (stringp list2))
         (cond
          ((string< list1 list2)
           t)
          (t nil)))
        ((and (numberp (car list1))
              (numberp (car list2)))
         (cond
          ((< (car list1) (car list2))
           t)
          ((> (car list1) (car list2))
           nil)
          (t (lex< (cdr list1) (cdr list2)))))
        ((and (stringp (car list1))
              (stringp (car list2)))
         (cond
          ((string< (car list1) (car list2))
           t)
          ((string> (car list1) (car list2))
           nil)
          (t (lex< (cdr list1) (cdr list2)))))
        (t (lex< (cdr list1) (cdr list2)))))

(defun interesting-top-level ()
  (sort (seq-filter #'is-interesting-p (top-level-keybinds))
        (lambda (x y) (lex< (list (length (split-string (car x) "-")) (length (car x)) (downcase (car x)))
                            (list (length (split-string (car y) "-")) (length (car y)) (downcase (car y)))))))

;;;###autoload
(defun which-key-show-simple ()
  (interactive)
  (let ((keylist (mapcar (lambda (x)
                           (list
                            (propertize (car x) 'face 'which-key-key-face)
                            (propertize " → " 'face 'which-key-separator-face)
                            (propertize (if (symbolp (cdr x)) (symbol-name (cdr x)) "lambda") 'face 'which-key-command-description-face)))
                         (interesting-top-level))))
    (setq which-key--pages-obj
          (which-key--create-pages keylist))
    (which-key--show-page)))

(provide 'which-key-show-simple)