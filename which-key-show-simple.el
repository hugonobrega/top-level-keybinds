(require 'which-key)

(defvar uninteresting-commands nil)

(defun concat-list (L)
  (let ((result '()))
    (dolist (e L)
      (setq result (append result e)))
    result))

(defun process (keymap)
  (cond
   ((eq 'keymap (car keymap))
    (process (cdr keymap)))
   ((stringp (car keymap))
    (process (cdr keymap)))
   ((eq 'remap (car keymap))
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

(defun is-interesting-p (pair)
  (let ((key (car pair))
        (command (cdr pair)))
    (not
     (or (eq 'digit-argument command)
         (eq 'negative-argument command)
         (cl-search "mouse" key)
         (and (cl-search "<" key)
              (cl-search ">" key))
         (and (symbolp command)
              (or (cl-search "evil-" (symbol-name command))
                  (cl-search "mouse" (symbol-name command))
                  (cl-search "mwheel" (symbol-name command))
                  (cl-search "menu-bar" (symbol-name command))
                  (member (symbol-name command) uninteresting-commands)))))))

(defun lex< (list1 list2)
  "Lexicographically compare two shallow lists of strings and numbers"
  (cond ((and (null list1) 
              (null list2))
         nil)
        ((null list1)
         t)
        ((null list2)
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
(defface which-key-hide-face
  nil
  ""
  :group 'which-key-faces)

(defface which-key-highlight-key-face
  nil
  ""
  :group 'which-key-faces)

(defun which-key-setup-invisible (&optional x)
  (set-face-foreground 'which-key-hide-face
                      (face-background 'default))
  (set-face-background 'which-key-highlight-key-face
                       (face-foreground 'which-key-command-description-face))
  (set-face-foreground 'which-key-highlight-key-face
                      (face-background 'default)))

(advice-add 'which-key--show-page :after #'which-key-setup-invisible)

(defun propertize-for-which-key (pair)
  (let ((key (car pair))
        (command (cdr pair)))
    (cond ((symbolp command)
           (let* ((command-name (symbol-name command))
                  (goodmatch (cl-search (concat "-" (downcase key)) (downcase command-name)))
                  (match (if goodmatch
                             (+ 1 goodmatch)
                           (cl-search (downcase key) (downcase command-name)))))
             (if match
                 (list
                  (propertize key 'face 'which-key-hide-face)
                  (propertize " " 'face 'which-key-hide-face)
                  (concat
                   (propertize (substring command-name 0 match) 'face 'which-key-command-description-face)
                   (propertize key 'face 'which-key-highlight-key-face)
                   (propertize (substring command-name (+ 1 match)) 'face 'which-key-command-description-face)
                   (propertize " " 'face 'which-key-command-description-face)))
               (list
                (propertize key 'face 'which-key-highlight-key-face)
                (propertize ":" 'face 'which-key-key-face)
                (propertize command-name 'face 'which-key-command-description-face)))))
          (t (list
              (propertize key 'face 'which-key-highlight-key-face)
              (propertize ":" 'face 'which-key-key-face)
              (propertize "(lambda)" 'face 'which-key-command-description-face))))))

;;;###autoload
(defun which-key-show-simple ()
  (interactive)
  (let ((keylist (mapcar #'propertize-for-which-key (interesting-top-level))))
    (setq which-key--pages-obj
          (which-key--create-pages keylist))
    (which-key--show-page)))

(provide 'which-key-show-simple)
