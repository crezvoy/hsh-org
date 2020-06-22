(defun my/previous-workdate (d)
  (time-subtract d 
		 (if (= (nth 6 (decode-time)) 1)
		     (days-to-time 3)
		   (days-to-time 1))))

(defun my/next-workdate (d)
  (time-add d 
	    (if (= (nth 6 (decode-time)) 5)
		(days-to-time 3)
	      (days-to-time 1))))
(defun my/previous-workday-raw ()
  (format-time-string "%Y-%m-%d"
		      (time-subtract (current-time)
				     (if (= (nth 6 (decode-time)) 1)
					 (days-to-time 3)
				       (days-to-time 1)))))
(defun my/next-workday-raw ()
  (format-time-string "%Y-%m-%d"
		      (time-subtract (current-time)
				     (if (= (nth 6 (decode-time)) 1)
					 (days-to-time 3)
				       (days-to-time 1)))))

(defun my/previous-workday () (format-time-string "[%s]" (my/previous-workday-raw)))

(defun my/previous-week ()
  (format-time-string "[%Y-%m-%d]" (time-subtract (current-time) (days-to-time 7))))

(defun my/previous-week-raw ()
  (format-time-string "%Y-%m-%d" (time-subtract (current-time) (days-to-time 7))))

(defun my/today-raw ()
  (format-time-string "%Y-%m-%d" (current-time)))

(defun my/active-timestamp (d)
  (format-time-string "<%Y-%m-%d>" d))

(defun my/active-repeat-timestamp (d r)
  (format "<%s %s>" (format-time-string "%Y-%m-%d" d) r))

(defun my/active-repeat-timestamp-with-time (d r)
  (format "<%s %s>" (format-time-string "%Y-%m-%d %a %H:%M" d) r))



(defun my/inactive-timestamp (d)
  (format-time-string "[%Y-%m-%d]" d))

(defun my/one-week-from-now()
  (format-time-string "%Y-%m-%d"
		      (time-add (current-time)
				(days-to-time 7))))

(defun my/previous-week-innactive ()
  (format-time-string "[%s]" (my/previous-week-raw)))

(defun my/set-category (value)
  (unless (equal (org-entry-get nil "CATEGORY") value)
    (org-entry-put nil "CATEGORY" value)))

(defun my/reset-category ()
  (interactive)
  (org-delete-property "CATEGORY"))

(use-package org
  :ensure org-plus-contrib
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   :keymaps 'override
   "x"   '((lambda () (interactive) (org-capture nil "e")) :which-key "Org capture")
   ;; Applications
   "an"  '((lambda () (interactive) (find-file "~/.local/share/notes/inbox.org")) :which-key "Notes")
   )
  (general-define-key
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
   :keymaps 'org-mode-map
   :states '(normal visual insert emacs)
   ;; Dates
   "m<" '(org-deadline :which-key "insert deadline")
   "m." '(org-schedule :which-key "insert date")
   "m/" '((lambda () (interactive) (org-time-stamp nil)) :which-key "insert date")
   "m," '((lambda () (interactive) (org-time-stamp-inactive nil)) :which-key "insert inactive date")
   ;; Attachements
   "ma" '(org-attach-open :which-key "Open attachment")
   "mA" '(org-attach-attach-cp :which-key "Add attachment")
   "mS" '(org-attach-delete-one :which-key "Delete attachment")
   ;; Subtrees
   "mn" '(org-narrow-to-subtree :which-key "narrow to subtree")
   "mN" '(widen :which-key "widen")
   "m\\" '(org-reveal :which-key "reveal subtree")
   "mm" '(org-refile :which-key "refile subtree")
   "mi" '(org-insert-heading-respect-content :which-key "insert subtree")
   "mt" '(org-insert-todo-heading-respect-content :which-key "insert todo")
   "m@" '(org-archive-subtree-default-with-confirmation :which-key "archive subtree")
   ;; Clocks
   "mI" '(org-clock-in :which-key "Clock in")
   "mO" '(org-clock-out :which-key "Clock out")
   "mU" '(org-clock-cancel :which-key "Cancel current clock")
   "mG" '(org-clock-goto :which-key "Go to current clock")
   "mR" '(org-clock-update-time-maybe :which-key "recompute current clock")
   ;; Priorities
   "mP" '((lambda () (interactive) (org-priority-up)) :which-key "Priority up")
   "mp" '((lambda () (interactive) (org-priority-down)) :which-key "Priority down")
   ;; Links
   "ml" '((lambda () (interactive) (org-open-at-point)) :which-key "Open link")
   "mL" '(org-insert-link :which-key "insert link")
   ;; Tags
   "m:"  '(counsel-org-tag :which-key "Add tags")
   ;; Blocks
   "mb" '((lambda () (interactive) (org-dblock-update)) :which-key "refresh block")
   "mB" '((lambda () (interactive) (org-update-all-dblocks)) :which-key "refresh all blocks")
   "mX" '((lambda () (interactive) (org-babel-execute-maybe)) :which-key "Execute block")
   "mE" '((lambda () (interactive) (org-edit-special)) :which-key "Edit block")
   "mT" '(org-babel-tangle :which-key "Tangle file")
   ;; footnotes
   "mf" '(org-footnote-new :which-key "new footnote")
   ;; Text
   "tb" '((lambda () (interactive) (org-emphasize ?*)) :which-key "bold")
   "ti" '((lambda () (interactive) (org-emphasize ?/)) :which-key "italics")
   "ts" '((lambda () (interactive) (org-emphasize ?+)) :which-key "strikethrough")
   "tu" '((lambda () (interactive) (org-emphasize ?_)) :which-key "underlined")
   "tv" '((lambda () (interactive) (org-emphasize ?=)) :which-key "verbatim")
   "tc" '((lambda () (interactive) (org-emphasize ?~)) :which-key "code"))
  (org-babel-do-load-languages 
   'org-babel-load-languages '((emacs-lisp .t)
			       (shell . t)
			       (python . t)))
  (custom-set-faces
   '(org-hide    ((t (:foreground "black"))))
   '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (setq org-capture-templates '(("e" "Entry" entry (file "~/.local/share/notes/notes.org")
				 "* %?\n%a\n"
				 :clock-in t
				 :clock-resume t)
				("." "Popup template" entry (file ,(concat org-directory "inbox.org"))
				 "* %?\n%x\n"
				 :clock-in t
				 :clock-resume t)
				)
	org-catch-invisible-edits t
	org-clock-out-remove-zero-time-clocks t
	org-cycle-hide-drawers t
	org-default-notes-file "~/.local/share/notes/inbox.org"
	org-directory "~/.local/share/notes"
	org-enforce-todo-checkbox-dependencies t
	org-enforce-todo-dependencies t
	org-file-apps '((system . "xdg-open \"%s\"") (auto-mode . emacs))
	org-hide-block-startup t
	org-hide-leading-stars t
	org-log-done t
	org-log-into-drawer "LOGBOOK"
	org-log-refile t
	org-log-repeat t
	org-odd-levels-only nil
	org-outline-path-complete-in-steps nil         ; Refile in a single go
	org-refile-targets '((("~/org/notes.org") . (:maxlevel . 1)))
	org-refile-use-outline-path nil 
	org-src-tab-acts-natively t
	org-startup-folded t
	org-startup-indented t
	org-startup-with-inline-images nil
	org-tags-column 50
	org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "ABRT(c)" "DONE(d)")
	  (sequence "[ ](T)" "[-](P)" "[?](M)" "|" "[X](D)")
	  (sequence "GOAL(g)" "AREA(a)" "|" "FINI(f)"))
	org-todo-keyword-faces '(("TODO" . (:foreground "cyan" :weight bold))
				 ("NEXT" . (:foreground "red" :weight bold))
				 ("GOAL" . (:foreground "blue" :weight bold))
				 ("AREA" . (:foreground "black" :weight bold))
				 ("WAIT" . (:foreground "yellow" :weight bold))
				 ("DONE" . (:foreground "green" :weight bold))
				 ("ABRT" . (:foreground "green" :strike-through t)))
	org-use-tag-inheritance t)
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))

(use-package org-agenda
  :config
  (setq org-agenda-confirm-kill nil
	org-agenda-clockreport-parameter-plist '(:link t
						       :maxlevel 5
						       :fileskip0 t
						       :compact t
						       :narrow 80)))

(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map nil))

(use-package evil-org
  :ensure t
  :after org
  :hook 
  ((org-mode . evil-org-mode)
   (evil-org-mode . (lambda ()
		      (evil-org-set-key-theme))))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(org-reload)
