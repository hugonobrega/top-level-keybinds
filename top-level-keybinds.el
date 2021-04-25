(require 'which-key)

(defvar tlk/uninteresting-commands '("digit-argument"
                                     "negative-argument"
                                     "execute-extended-command")
  "A list of command names that top-level-keybinds will ignore")

(defvar tlk/change-look? t
  "A boolean controling whether to change the which-key buffer
look or not. The change includes trying to embed the key in
the command name when possible.")

(defvar tlk/show-prefix? "+prefix"
  "Controls how top-level-keybinds should show prefix keys:
  - nil, to hide prefix keys from the which-key buffer
  - a string")

(defvar tlk/comparison-predicate #'tlk/compare-keys
  "The predicate used for comparing pairs (key . command-name) when sorting
the results. Define your own, or use one of the two provided:

 - #'tlk/compare-keys: sorts by key length first, then alphabetically by the
key itself (not case sensitive)
 - #'tlk/compare-command-names: sorts alphabetically by command name")

(defun tlk/pair-key-command-p (candidate)
  (and (listp candidate)
       (or (commandp (cdr candidate))
           (autoloadp (cdr candidate)))
       (not (or (commandp (car candidate))
                (autoloadp (car candidate))))))

(defun tlk/add-M- (key)
  (cond
   ((cl-search "M-" key)
    key)
   ((memq 0 (list (cl-search "C-" key)
                  (cl-search "H-" key)))
    (concat (substring key 0 2) "M-" (substring key 2)))
   ((eq 0 (cl-search "A-" key))
    (concat "M-" (substring key 2)))
   (t (concat "M-" key))))

(defun tlk/collect? (pair &optional add-M-?)
  (cond ((tlk/pair-key-command-p pair)
         (let* ((key (key-description (vector (car pair))))
                (key (if add-M-?
                         (tlk/add-M- key)
                       key))
                (key (replace-regexp-in-string "A-" "M-" key))
                (command-name
                 (if (symbolp (cdr pair))
                     (symbol-name (cdr pair))
                   "(lambda)")))
           (cons key command-name)))
        ((and (listp pair)
              (or (symbolp (car pair))
                  (integerp (car pair)))
              (listp (cdr pair))
              (not (eq 27 (car pair)))
              (eq 'keymap (cadr pair)))
         (cons (key-description (vector (car pair))) tlk/show-prefix?))))

(defun tlk/compare-keys (x y)
  (tlk/lex< (list (length (split-string (car x) "-"))
                  (length (car x))
                  (downcase (car x)))
            (list (length (split-string (car y) "-"))
                  (length (car y))
                  (downcase (car y)))))

(defun tlk/compare-command-names (x y)
  (string< (cdr x) (cdr y)))

(defun tlk/interesting? (pair)
  (let ((key (car pair))
        (command-name (cdr pair)))    
    (not (or
          (null command-name)
          (cl-search "mouse" key)
          (and (not (equal "<menu>" key))
               (cl-search "<" key)
               (cl-search ">" key))
          (cl-search "evil-" command-name)
          (cl-search "mouse" command-name)
          (cl-search "mwheel" command-name)
          (cl-search "menu-bar" command-name)
          (member command-name tlk/uninteresting-commands)))))

(defun tlk/collect (&optional thing comparison-predicate)
  (let ((thing (or thing
                   (seq-reduce #'append (current-active-maps) nil)))
        (comparison-predicate (or comparison-predicate
                                  tlk/comparison-predicate))
        (queue nil))
    (dolist (item thing)
      (cond ((tlk/collect? item)
             (push (tlk/collect? item) queue))
            ((listp item)
             (cond ((and (eq 27 (car item))
                         (eq 'keymap (cadr item)))
                    (dolist (subitem (cddr item))
                      (let ((processed (tlk/collect? subitem t)))
                        (when processed
                          (push processed queue)))))))))
    (let
        ((all (nreverse queue))
         (seen (make-hash-table :test #'equal))
         (result '()))
      (dolist (pair all)
        (unless (gethash (car pair) seen)
          (puthash (car pair) t seen)
          (push pair result)))
      (sort (seq-filter #'tlk/interesting? result) comparison-predicate))))

(defun tlk/lex< (list1 list2)
  "Lexicographically (strictly) compare two shallow lists of strings and numbers.
Assumes both lists have the same types of elements at positions in which they
are both defined."
  (cond
   ((null list2)
    nil)
   ((null list1)
    t)
   ((and (numberp list1)
         (numberp list2))
    (when (< list1 list2)
      t))
   ((and (stringp list1)
         (stringp list2))
    (when (string< list1 list2)
      t))
   ((and (numberp (car list1))
         (numberp (car list2)))
    (cond
     ((< (car list1) (car list2))
      t)
     ((> (car list1) (car list2))
      nil)
     (t (tlk/lex< (cdr list1) (cdr list2)))))
   ((and (stringp (car list1))
         (stringp (car list2)))
    (cond
     ((string< (car list1) (car list2))
      t)
     ((string> (car list1) (car list2))
      nil)
     (t (tlk/lex< (cdr list1) (cdr list2)))))
   (t (tlk/lex< (cdr list1) (cdr list2)))))

(defface tlk/which-key-hide-face
  nil
  ""
  :group 'tlk/which-key-faces)

(defface tlk/which-key-highlight-key-face
  nil
  ""
  :group 'tlk/which-key-faces)

(defun tlk/which-key-setup-invisible (&optional x)
  (when tlk/change-look?
    (set-face-foreground 'tlk/which-key-hide-face
                         (face-background 'default))
    (set-face-background 'tlk/which-key-highlight-key-face
                         (face-foreground 'which-key-command-description-face))
    (set-face-foreground 'tlk/which-key-highlight-key-face
                         (face-background 'default))))

(advice-add 'which-key--show-page :after #'tlk/which-key-setup-invisible)

(defun tlk/propertize-for-which-key (pair)
  (let*
      ((key (car pair))
       (command (cdr pair))
       (goodmatch (cl-search (concat "-" (downcase key)) (downcase command)))
       (match (if goodmatch
                  (+ 1 goodmatch)
                (cl-search (downcase key) (downcase command))))
       (command-face (if (equal tlk/show-prefix? command)
                         'which-key-group-description-face
                       'which-key-command-description-face)))
    (cond
     ((not tlk/change-look?)
      (list
       (propertize key
                   'face 'which-key-key-face)
       (propertize which-key-separator
                   'face 'which-key-separator-face)
       (propertize command
                   'face command-face)))
     (match
      (list
       (propertize key
                   'face 'tlk/which-key-hide-face)
       (propertize " "
                   'face 'tlk/which-key-hide-face)
       (concat
        (propertize (substring command 0 match)
                    'face command-face)
        (propertize key
                    'face 'tlk/which-key-highlight-key-face)
        (propertize (substring command (+ 1 match))
                    'face command-face)
        (propertize " "
                    'face command-face))))
     (t
      (list
       (propertize key
                   'face 'tlk/which-key-highlight-key-face)
       ":"
       (propertize command
                   'face command-face))))))

;;;###autoload
(defun tlk/show ()
  (interactive)
  (let ((keylist (mapcar #'tlk/propertize-for-which-key (tlk/collect))))
    (setq which-key--pages-obj
          (which-key--create-pages keylist))
    (which-key--show-page)))

(provide 'top-level-keybinds)
