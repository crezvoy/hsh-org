(setq my/org-directory "~/Documents/Notes/")
(setq my/workspace-root "~/Documents/Workspaces/")

(defun my/list-org-files ()
  (directory-files-recursively my/org-directory "\.org$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Lists of common tags ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my/contexts '())

(setq my/people '())

(setq my/kinds '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org helper functions ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cal-iso)
(defun my/cur-iso-week-to-time ()
  (pcase-let* ((`(,in-week ,in-day ,in-year)
		(calendar-iso-from-absolute
		 (calendar-absolute-from-gregorian (calendar-current-date))))
	       (`(,out-month ,out-day ,out-year)
		(calendar-gregorian-from-absolute
		 (calendar-iso-to-absolute `(,in-week 1 ,in-year)))))
    (encode-time 0 0 0 out-day out-month out-year)))

(defun  my/capture-location ()
  (let* ((week-start (my/cur-iso-week-to-time))
	 (dir (concat my/org-directory (format-time-string "/%G/")))
	 (file (format-time-string "W-%V_%m-%d.org" week-start))
	 (path (concat dir file))
	 (title (format-time-string "* Week %V, %d/%m/%Y" week-start))
	 (heading (format-time-string "** %d/%m/%Y")))
    (progn
      (when (not (file-directory-p dir))
	(mkdir dir t))
      (when (not (file-exists-p path))
	(write-region title nil path)
	(add-to-list 'org-agenda-files path))
      (set-buffer (org-capture-target-buffer path))
      (goto-char (point-min))
      (if (not (re-search-forward (regexp-quote heading) nil t))
	  (progn
	    (goto-char (point-max)) 
	    (insert (concat "\n" heading))))
      (goto-char (point-max)))))

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


(defun my/is-refile-target-p ()
  (let ((cat (org-entry-get (point) "CATEGORY" nil))
	(has-todo (equal (org-get-todo-state) "TODO")))
    (or (string-match-p "PERSPECTIVE" cat)
	(string-match-p "IDEA" cat)
	has-todo)))

(defun my/set-category-or-context ()
  (interactive)
  (let* ((completion-ignore-case t)
	 (allowed-category my/categories)
	 (allowed-delegate '("@mine" "@wait"))
	 (allowed-context my/contexts)
	 (prop "CATEGORY")
	 (pom (point-at-bol))
	 (has-context (s-matches? "^@.*" (org-get-category pom)))
	 (has-todo (member (org-get-todo-state) org-todo-keywords-1))
	 (val (if has-todo (if has-context
			       (completing-read "Context:" allowed-context)
			     (if (y-or-n-p "Delegate?") "@wait" "@mine"))
		(completing-read "Category: " allowed-category))))
    (org-entry-put pom prop val)))

(defadvice org-capture-finalize 
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "Quick note" (frame-parameter nil 'name))
      (delete-frame)))

(defun my/setup-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (let* ((f-width 480)
	 (f-height 240)
	 (f-top (truncate (/ (- (display-pixel-height) f-height) 2)))
	 (f-left (truncate (/ (- (display-pixel-width) f-width) 2)))
	 (frame (make-frame `((name . "Quick note")
			      (transient . t)))))
    (set-frame-size frame f-width f-height t)
    (set-frame-position frame f-left f-top)
    (select-frame-set-input-focus frame)
    (with-selected-frame frame
      (setq mode-line-format nil)
      (switch-to-buffer "*scratch*")
      (require 'org-capture)
      (condition-case ex
	  (cl-letf (((symbol-function 'pop-to-buffer) #'switch-to-buffer))
	    (setq org-capture-entry (org-capture-select-template "."))
	    (org-capture))
	('error
	 (message "org capture: %s" (error-message-string ex))
	 (delete-frame frame))))))

(defun my/open-or-create-workspace (action)
  (interactive)
  (let* ((property-name "WORKSPACE")
	 (heading-comps (org-heading-components))
	 (heading (nth 4 heading-comps))
	 (workspace-subdir (org-entry-get (point) property-name t))
	 (defdir (concat (file-name-as-directory my/workspace-root)
			 (if (null workspace-subdir)
			     (progn
			       (org-set-property property-name
						 (replace-regexp-in-string "[ ,:/|?]" "-" heading))
			       (org-entry-get (point) property-name nil))
			   workspace-subdir))))
    (progn
      (if (not(file-exists-p defdir))
	  (make-directory defdir t))
      (split-window-sensibly)
      (let ((default-directory defdir))
	(funcall action)))))

(defun my/term-in-workspace ()
  (open-or-create-workspace #'(lambda () (term "/bin/bash"))))
(defun my/dired-in-workspace ()
  (open-or-create-workspace #'(lambda () (dired default-directory))))

(defun my/unpin-actions ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-todo "NEXT")
     "/PIN"
     agenda)))

(defun my/display-agenda (cc)
  (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
    (get-buffer-create org-agenda-buffer-name))
  (with-current-buffer (get-buffer org-agenda-buffer-name)
    (org-agenda-mode)
    ;; (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
    (get-buffer-create org-agenda-buffer-name))
  (display-buffer-in-side-window (get-buffer org-agenda-buffer-name)
				 '((side . left)
				   (window-width . 40)))
  (org-agenda nil cc)
  (setq buffer-read-only nil))

;;;;;;;;;;;
;;; Org ;;;
;;;;;;;;;;;
(use-package org
  :ensure org-plus-contrib
  :config
  (require 'org-attach)
  (require 'org-tempo)
  (my/leader
    "x"   '((lambda () (interactive) (org-capture nil "e")) :which-key "Org capture")
    ;; Applications
    "an"  '((lambda () (interactive) (find-file (concat my/org-directory "inbox.org"))) :which-key "Notes"))
  (my/local-leader
    :keymaps 'org-mode-map
    ;; Todos
    "0" '((lambda () (interactive) (org-todo "")) :which-key "Remove todo")
    "1" '((lambda () (interactive) (org-todo "TODO")) :which-key "-> TODO")
    "2" '((lambda () (interactive) (org-todo "NEXT")) :which-key "-> NEXT")
    "3" '((lambda () (interactive) (org-todo "PIN")) :which-key "-> PIN")
    "4" '((lambda () (interactive) (org-todo "DONE")) :which-key "-> DONE")
    "5" '((lambda () (interactive) (org-todo "ABORT")) :which-key "-> ABORT")
    ;; Dates
    "<" '(org-deadline :which-key "Insert deadline")
    "." '(org-schedule :which-key "Schedule entry")
    "/" '((lambda () (interactive) (org-time-stamp nil)) :which-key "insert date")
    "," '((lambda () (interactive) (org-time-stamp-inactive nil)) :which-key "insert inactive date")
    ; Attachements
    "a" '(org-attach-open :which-key "Open attachment")
    "A" '(org-attach-attach-mv :which-key "Add attachment")
    "S" '(org-attach-delete-one :which-key "Delete attachment")
    ; Subtrees
    "n" '(org-narrow-to-subtree :which-key "narrow to subtree")
    "N" '(widen :which-key "widen")
    "\\" '(org-reveal :which-key "reveal subtree")
    "m" '(org-refile :which-key "refile subtree")
    "i" '(org-insert-heading-respect-content :which-key "insert subtree")
    "t" '(org-insert-todo-heading-respect-content :which-key "insert todo")
    "@" '(org-archive-subtree-default-with-confirmation :which-key "archive subtree")
    ; Clocks
    "c" '(:ignore t :which-key "Clocks")
    "ci" '(org-clock-in :which-key "Clock in")
    "co" '(org-clock-out :which-key "Clock out")
    "cu" '(org-clock-cancel :which-key "Cancel current clock")
    "cg" '(org-clock-goto :which-key "Go to current clock")
    "cr" '(org-clock-update-time-maybe :which-key "recompute current clock")
    ; Categories
    "C" '(my/set-category-or-context :which-key "set category or context")
    ; Priorities
    "P" '((lambda () (interactive) (org-priority-up)) :which-key "Priority up")
    "p" '((lambda () (interactive) (org-priority-down)) :which-key "Priority down")
    ; Links
    "l" '((lambda () (interactive) (org-open-at-point)) :which-key "Open link")
    "L" '(org-insert-link :which-key "insert link")
    ; Tags
    ":"  '(counsel-org-tag :which-key "Add tags")
    ; Blocks
    "b" '((lambda () (interactive) (org-dblock-update)) :which-key "refresh block")
    "B" '((lambda () (interactive) (org-update-all-dblocks)) :which-key "refresh all blocks")
    "X" '((lambda () (interactive) (org-babel-execute-maybe)) :which-key "Execute block")
    "E" '((lambda () (interactive) (org-edit-special)) :which-key "Edit block")
    "T" '(org-babel-tangle :which-key "Tangle file")
    ; Workspace
    "w" '((lambda ()
	    (interactive)
	    (my/open-or-create-workspace #'(lambda () (term "/bin/bash"))))
	   :which-key "Create Workspace")
    "W" '((lambda ()
	    (interactive)
	    (my/open-or-create-workspace #'(lambda () (dired default-directory))))
	  :which-key "Create Workspace")
    ; footnotes
    "f" '(org-footnote-new :which-key "new footnote")
    "S" '(my/org-add-subtask :which-key "Insert todo subheading")
    ;; Text
    "eb" '((lambda () (interactive) (org-emphasize ?*)) :which-key "bold")
    "ei" '((lambda () (interactive) (org-emphasize ?/)) :which-key "italics")
    "es" '((lambda () (interactive) (org-emphasize ?+)) :which-key "strikethrough")
    "eu" '((lambda () (interactive) (org-emphasize ?_)) :which-key "underlined")
    "ev" '((lambda () (interactive) (org-emphasize ?=)) :which-key "verbatim")
    "ec" '((lambda () (interactive) (org-emphasize ?~)) :which-key "code"))
  (general-define-key
   :prefix "C-c"
   :non-normal-prefix "C-c"
   :keymaps 'org-mode-map
   :states '(normal visual insert emacs)
   "[" '((lambda () (interactive) (org-time-stamp nil t)) :which-key "Insert inactive
timestamp")
   "<" '((lambda () (interactive) (org-time-stamp nil nil)) :which-key "Insert timestamp"))
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
  (setq org-attach-auto-tag nil
	org-attach-id-dir (concat my/org-directory ".attach")
	org-attach-use-inheritance nil
	org-capture-templates `(("e" "Entry" entry (file ,(concat my/org-directory "inbox.org"))
				 "* %?\n%a\n"
				 :clock-in t
				 :clock-resume t)
				("j" "Journal Entry" plain (function my/capture-location)
				 "*%<%H:%M> %?\n%a\n"
				 :clock-in t
				 :clock-resume t)
				("." "Popup template" entry (file ,(concat my/org-directory "inbox.org")) 
				 "* %?\n%x\n"
				 :clock-in t
				 :clock-resume t))
	org-catch-invisible-edits t
	org-clock-out-remove-zero-time-clocks t
	org-clock-persist 'history
	org-cycle-hide-drawers t
	org-default-notes-file (concat my/org-directory "inbox.org")
	org-directory my/org-directory 
	org-duration-format 'h:mm
	org-enforce-todo-checkbox-dependencies t
	org-enforce-todo-dependencies t
	org-use-fast-todo-selection t
	org-file-apps '((system . "xdg-open \"%s\"") (auto-mode . emacs))
	org-hide-block-startup t
	org-hide-leading-stars t
	org-log-done t
	org-log-into-drawer "LOGBOOK"
	org-log-refile nil
	org-log-repeat t
	org-odd-levels-only nil
	org-outline-path-complete-in-steps nil         ; Refile in a single go
	org-refile-targets '((nil :maxlevel . 9)
			     (org-agenda-files :maxlevel . 9))
	org-refile-use-outline-path nil 
	org-refile-target-verify-function 'my/is-refile-target-p
	org-src-tab-acts-natively t
	org-startup-folded t
	org-startup-indented t
	org-startup-with-inline-images nil
	org-tags-column 50
	org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "PIN(p)" "|" "DONE(d)" "ABRT(a)"))
	org-todo-keyword-faces '(("TODO" :foreground "royal blue" :weight bold)
				 ("NEXT" :foreground "dodger blue" :weight bold)
				 ("PIN" :foreground "DarkOrange1" :weight bold)
				 ("DONE" :foreground "olive drab" :weight bold)
				 ("ABRT" :foreground "dark olive green" :strike-through t))
	org-use-tag-inheritance t)
  (general-def [remap org-capture] 'ignore)
  (org-clock-persistence-insinuate)
  (add-to-list 'org-link-abbrev-alist '("att" . org-attach-expand-link))
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))

;;;;;;;;;;;;;;
;;; Agenda ;;;
;;;;;;;;;;;;;;
(setq my/inbox-fillers '())
(defun my/fill-inbox ()
  (interactive)
  (mapc 'funcall my/inbox-fillers))

(setq my/categories '("DISCUSS"
		      "DOC"
		      "IDEA"
		      "LINKS"
		      "MAIL"
		      "MEETING_NOTES"
		      "PERSPECTIVE"
		      "PICTURE"
		      "SHEETMUSIC"
		      "TIPSNTRICKS"))

(defun my/skip-categorized ()
  "Skip entries that already has actions attached to it"
  (save-restriction
    (widen)
    (let ((todo (org-get-todo-state))
	  (category (org-entry-get (point) "CATEGORY" nil)))
      (if (or todo category)
	  nil
	(save-excursion (or (outline-next-heading) (point-max)))))))

(defun my/skip-actionned ()
  "Skip entries that already has actions attached to it"
  (save-restriction
    (widen)
    (let ((has-subtask nil)
	  (subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (progn
	    (when (member (org-get-todo-state) org-todo-keywords-1)
	      (setq has-subtask t)))))
      (if (not has-subtask)
	  nil
	(save-excursion (or (outline-next-heading) (point-max)))))))

(defun my/skip-has-next ()
  "Skip entries that already has actions attached to it"
  (save-restriction
    (widen)
    (let ((has-subtask nil)
	  (subtree-end (save-excursion (org-end-of-subtree t))))
      (save-excursion
	(forward-line 1)
	(while (and (not has-subtask)
		    (< (point) subtree-end)
		    (re-search-forward "^\*+ " subtree-end t))
	  (progn
	    (when (equal (org-get-todo-state) "NEXT")
	      (setq has-subtask t)))))
      (if (not has-subtask)
	  nil
	(save-excursion (or (outline-next-heading) (point-max)))))))

(defun my/org-add-subtask ()
  (interactive)
  (org-insert-todo-heading-respect-content)
  (org-demote))

(defun my/org-agenda-add-subtask ()
  "Add a time-stamped note to the entry at point."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((completion-ignore-case t)
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker))
	 (task (read-string "task: "))
	 (inhibit-read-only t))
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(my/org-add-subtask)
	(insert task)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead marker))))

(setq my/contexts '("@mine"
		    "@wait"
		    "@morning"
		    "@daily"
		    "@obeya"
		    "@home"
		    "@work"
		    "@mail"
		    "@jira"
		    "@confluence"
		    "@noon"
		    "@night"
		    "@phone"
		    "@shop"
		    "@tools")
      my/current-context (car my/contexts))

(defun my/cycle-context ()
  (interactive)
  (setq my/current-context (my/next-in-list my/current-context my/contexts)))

(defun my/agenda-set-category-or-context ()
  "Set the category for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((completion-ignore-case t)
	 (hdmarker (or (org-get-at-bol 'org-hd-marker)
		       (org-agenda-error)))
	 (buffer (marker-buffer hdmarker))
	 (pos (marker-position hdmarker))
	 (inhibit-read-only t)
	 newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
	(widen)
	(goto-char pos)
	(org-show-context 'agenda)
	(call-interactively 'my/set-category-or-context)
	(end-of-line 1)
	(setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun my/skip-not-current-context ()
  "Skip entries that already has actions attached to it"
  (if (equal (org-get-category) my/current-context)
      nil
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun my/display-agenda (cc)
  (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
    (get-buffer-create org-agenda-buffer-name))
  (with-current-buffer (get-buffer org-agenda-buffer-name)
    (org-agenda-mode)
    (setq buffer-read-only t)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  ;; (mapcar
  ;;  (lambda (x)
  ;;    (let ((inhibit-read-only t))
  ;; 	 (insert (format "%s" x) "\n")))
  ;;  result))
  (unless (buffer-live-p (get-buffer org-agenda-buffer-name))
    (get-buffer-create org-agenda-buffer-name))
  (display-buffer-in-side-window (get-buffer org-agenda-buffer-name)
				 '((side . left)
				   (window-width . 40)))
  (org-agenda nil cc))

(use-package org-agenda
  :init
  (require 'org)
  :config
  (require 'org-super-agenda)
  (org-super-agenda-mode)
  (setq org-agenda-confirm-kill nil
	org-agenda-clockreport-parameter-plist '(:link t
						       :maxlevel 5
						       :fileskip0 t
						       :compact t
						       :narrow 80)
	my/emoji-dir (concat emojify-emojis-dir "/" emojify-emoji-set "/")
	org-agenda-category-icon-alist
	`(
	  ("^inbox" ,(concat my/emoji-dir "1f4e5.png") nil nil :ascent center)
	  ("^RSS$" ,(concat  my/emoji-dir "1f4f0.png") nil nil :ascent center)
	  ("^IDEA$" ,(concat my/emoji-dir "1f4a1.png") nil nil :ascent center)
	  ("^PERSPECTIVE$" ,(concat my/emoji-dir "1f441.png") nil nil :ascent center)
	  ("^MEETING_NOTES$" ,(concat my/emoji-dir "1f91d.png") nil nil :ascent center)
	  ("^DISCUSS$" ,(concat my/emoji-dir "1f5e8.png") nil nil :ascent center)
	  ("^LINKS$" ,(concat my/emoji-dir "1f517.png") nil nil :ascent center)
	  ("^DOC$" ,(concat my/emoji-dir "1f4ce.png") nil nil :ascent center)
	  ("^PICTURE$" ,(concat my/emoji-dir "1f4f7.png") nil nil :ascent center)
	  ("^MAIL$" ,(concat my/emoji-dir "1f4e7.png") nil nil :ascent center)
	  ("^SHEETMUSIC$" ,(concat my/emoji-dir "1f3b5.png" ) nil nil :ascent center)
	  ("^TIPSNTRICKS$" ,(concat my/emoji-dir "2728.png" ) nil nil :ascent center)
	  ("^@mine" ,(concat my/emoji-dir "2611.png") nil nil :ascent center)
	  ("^@wait" ,(concat my/emoji-dir "231b.png") nil nil :ascent center)
	  ("^@home$" ,(concat my/emoji-dir "1f3e0.png") nil nil :ascent center)
	  ("^@work$" ,(concat my/emoji-dir "1f4bc.png") nil nil :ascent center)
	  ("^@mail" ,(concat my/emoji-dir "1f4e7.png") nil nil :ascent center)
	  ("^@issue" ,(concat my/emoji-dir "26a0.png") nil nil :ascent center)
	  ("^@confluence" ,(concat my/emoji-dir "1f1ff.png") nil nil :ascent center)
	  ("^@night$" ,(concat my/emoji-dir "1f319.png") nil nil :ascent center)
	  ("^@noon$" ,(concat my/emoji-dir "1f374.png") nil nil :ascent center)
	  ("^@morning$" ,(concat my/emoji-dir "2615.png") nil nil :ascent center)
	  ("^@phone$" ,(concat my/emoji-dir "1f4de.png") nil nil :ascent center)
	  ("^@shop$" ,(concat my/emoji-dir "1f6d2.png") nil nil :ascent center)
	  ("^@tools" ,(concat my/emoji-dir "1f528.png") nil nil :ascent center))
	org-agenda-custom-commands
	`(("g2" "GTD: clarify"
	   ((tags "+CATEGORY={^inbox$}+TODO={^$}"
		  ((org-agenda-overriding-header "What is it: TODO (SPC m t)? or entry to be categorized (SPC m C)")
		   (org-agenda-prefix-format "%s")
		   (org-agenda-skip-function 'my/skip-categorized)
		   (org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags "-CATEGORY={^inbox$}-CATEGORY={PERSPECTIVE}-CATEGORY={IDEA}-CATEGORY={RSS}+TODO={^$}+LEVEL=1|CATEGORY={RSS}+TODO={^$}+LEVEL=2"
		  ((org-agenda-overriding-header "Actionable (SPC m S)?")
		   (org-agenda-prefix-format "%i %s")
		   (org-agenda-skip-function 'my/skip-actionned)
		   (org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags-todo "+TODO={^TODO$}-CATEGORY={@wait}"
		       ((org-agenda-overriding-header "What is the next step? had action sub-headline (SPC m t n)")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-skip-function 'my/skip-has-next)
			(org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags-todo "+TODO={^NEXT$}-CATEGORY={^[@].*}"
		       ((org-agenda-overriding-header "Delegate (SPC m C + wait), do myself later (SPC m C mine) ? ")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags-todo "+LEVEL=1+TODO={^$}+CATEGORY={[A-Z_]+}-CATEGORY=PERSPECTIVE|LEVEL=2+TODO={^$}+CATEGORY={[A-Z_]+}-CATEGORY=PERSPECTIVE|CATEGORY={RSS}+TODO={^$}+LEVEL=2"
		       ((org-agenda-overriding-header "Worth keeping (C-k to delete) ?")
			(org-agenda-skip-function 'my/skip-has-next)
			(org-agenda-files '(,(concat my/org-directory "inbox.org")))))))
	  ("g3" "GTD: Organize"
	   ((tags-todo "+CATEGORY={^@mine$}+TODO={NEXT}"
		       ((org-agenda-overriding-header "Add a context: SPC m C")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags "CATEGORY={[@].*}+TODO={.+}+LEVEL=1|CATEGORY={^[A-Z_]\+$}+LEVEL=1-CATEGORY={RSS}|CATEGORY={^RSS$}+LEVEL=2"
		  ((org-agenda-overriding-header "Move to an existing project or a perpective: SPC m r")
		   (org-agenda-prefix-format "%i %s")
		   (org-agenda-files '(,(concat my/org-directory "inbox.org")))))))
	  ("g4" "GTD: Plan the day"
	   ((agenda ""
		    ((org-agenda-overriding-header "Plan the day")
		     (org-agenda-start-day "0d")
		     (org-agenda-prefix-format "%i %s")
		     (org-agenda-span 3)
		     (org-deadline-warning-days 0)
		     (org-agenda-repeating-timestamp-show-all t)
		     (org-agenda-include-deadlines t)))
	    (tags-todo "TODO={^NEXT$}"
		       ((org-agenda-overriding-header "Today candidates")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-skip-function 'my/skip-not-current-context)
			(org-super-agenda-groups
			 '((:auto-category t)))))))
	  ("g5" "GTD: Today"
	   ((agenda ""
		    ((org-agenda-overriding-header "Today")
		     (org-agenda-start-day "0d")
		     (org-agenda-span 1)
		     (org-agenda-time-grid '((daily today)
					     (800 1000 1200 1400 1600 1800 2000)
					     "················"
					     "----------------"))
		     (org-agenda-prefix-format "%i %s")
		     (org-agenda-include-deadlines nil)))
	    (tags-todo "+TODO={^PIN$}-CATEGORY={^@wait$}"
		       ((org-agenda-overriding-header "Picked for today")
			(org-agenda-prefix-format "%i %s")))
	    (tags-todo "TODO={^.*$}"
		       ((org-agenda-overriding-header "Fast track")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-files '(,(concat my/org-directory "inbox.org")))))
	    (tags-todo "TODO={^NEXT$}"
		       ((org-agenda-overriding-header "Current Context")
			(org-agenda-prefix-format "%i %s")
			(org-agenda-skip-function 'my/skip-not-current-context)))))
	  ("g6" "GTD: Project Grooming"
	   ;; Goals
	   ((tags "+CATEGORY={.*}"
		  ((org-agenda-overriding-header "Project Grooming")
		   (org-super-agenda-groups
		    '((:name "Projects that require attention"
			     :and (:todo "TODO"
					 :not (:category ("IDEA" "@wait") :tag ("@maybe"))
					 :not (:children ("NEXT" "PIN"))))
		      (:name "uncategorized entries"
			     :category ""
			     :category nil)
		      (:name "tasks without context"
			     :and (:todo "NEXT"
					 :category "@mine")
			     :and (:todo "NEXT"
					 :not (:regexp ":CATEGORY:")))
		      (:name "Waiting task without deadline"
			     :and (:category "@wait"
					     :todo "NEXT"
					     :deadline nil))
		      (:name "Waiting task without assignee"
			     :and (:category "@wait"
					     :tag nil)
			     :discard (:anything t))))
		   (org-agenda-prefix-format "%l %i %s")))))
	  ("g7" "Perspective Review"
	   ((tags "+CATEGORY={.*}"
		  ((org-agenda-overriding-header "Perspective Review")
		   (org-super-agenda-groups
		    '((:name "Perspectives"
			     :and (:category "PERSPECTIVE"
				   :regexp ":CATEGORY:"))
		      (:name "Done"
			     :todo "DONE")
		      (:name "Next"
			     :and (:todo ("NEXT" "PIN")
				   :not (:tag "@maybe")))
		      (:name "Entries that require attention"
			     :and (:todo "TODO"
					 :not (:category ("IDEA" "@wait") :tag ("@maybe"))
					 :not (:children ("NEXT" "PIN"))))
		      (:name "Todo"
			     :and (:todo "TODO"
			           :not (:tag "@maybe")))
		      (:name "References & links"
			     :and (:category ("DOC" "LINKS" "PICTURE" "MAIL" "TIPSNTRICKS")
				   :regexp ":CATEGORY:"))
		      (:name "Ideas"
			     :tag "@maybe"
			     :category "IDEA")))
		   (org-agenda-prefix-format "%l %i %s")))))
	  ("g9" "Daily review"
	   ((agenda ""
		    ((org-agenda-overriding-header "Today")
		     (org-agenda-span 1)
		     (org-agenda-start-day "0d")
		     (org-agenda-compact-blocks t)
		     (org-agenda-show-log t)
		     (org-agenda-repeating-timestamp-show-all t)
		     (org-agenda-clockreport-mode t)
		     (org-agenda-show-clocking-issues 1)
		     (org-agenda-log-mode-items '(clock closed))
		     (org-agenda-prefix-format "%t %s")
		     (org-agenda-show-log 'clockcheck)
		     (org-agenda-clockreport-parameter-plist
		      '(:link t
			      :hidefiles t
			      :scope agenda-with-archives
			      :maxlevel 20
			      :fileskip0 t
			      :compact t
			      :narrow 35))
		     (org-agenda-clock-consistency-checks
		      '(:max-duration "4:00"
				      :min-duration "0:01"
				      :max-gap "0:15"
				      :gap-ok-around ("4:00" "13:00" "14:00" "19:00")))))))
	  ("g0" "Weekly review"
	   ((agenda ""
		    ((org-agenda-overriding-header "Previous Week")
		     (org-agenda-span 7)
		     (org-agenda-start-on-weekday 1)
		     (org-agenda-start-day "-4d")
		     (org-agenda-compact-blocks t)
		     (org-agenda-show-log t)
		     (org-agenda-repeating-timestamp-show-all t)
		     (org-agenda-clockreport-mode t)
		     (org-agenda-show-clocking-issues 1)
		     (org-agenda-log-mode-items '(clock closed))
		     (org-agenda-prefix-format "%t %s")
		     (org-agenda-show-log 'clockcheck)
		     (org-agenda-clockreport-parameter-plist
		      '(:link t
			      :hidefiles t
			      :scope agenda-with-archives
			      :maxlevel 20
			      :fileskip0 t
			      :compact t
			      :narrow 35))
		     (org-agenda-clock-consistency-checks
		      '(:max-duration "4:00"
				      :min-duration "0:01"
				      :max-gap "0:15"
				      :gap-ok-around ("4:00" "13:00" "14:00" "19:00"))))))))
	org-agenda-files (my/list-org-files) 
	org-agenda-window-setup 'only-window)
  (my/leader
    "n"  '(:ignore t :which-key "Notes")
    "n1" '(my/fill-inbox :which-key "GTD: Fill inbox")
    "n2" '((lambda () (interactive) (my/display-agenda "g2")) :which-key "GTD: Clarify")
    "n3" '((lambda () (interactive) (my/display-agenda "g3")) :which-key "GTD: Organize")
    "n4" '((lambda () (interactive) (my/display-agenda "g4")) :which-key "GTD: Plan the day")
    "n5" '((lambda () (interactive) (my/display-agenda "g5")) :which-key "GTD: Today")
    "n6" '((lambda () (interactive) (my/display-agenda "g6")) :which-key "GTD: Project Grooming")
    "n7" '((lambda () (interactive) (my/display-agenda "g7")) :which-key "GTD: Perspective Review")
    "n9" '((lambda () (interactive) (my/display-agenda "g9")) :which-key "GTD: Review the day")
    "n0" '((lambda () (interactive) (my/display-agenda "g0")) :which-key "GTD: Review the past week")
    "n@" '((lambda ()
	     (interactive)
	     (setq my/current-context (ivy-completing-read "Context: " my/contexts))
	     (org-agenda-redo t)
	     (message (concat "Current context set to " my/current-context)))
	   :which-key "GTD: Review the past week")
    "nu" '(my/unpin-actions :which-key "GTD: Unpick actions"))
  (evil-define-key* '(motion visual insert emacs) org-agenda-mode-map
		    (kbd "SPC") nil)
  (my/local-leader
    :states '(motion normal visual insert emacs)
    :keymaps 'org-agenda-mode-map
    "0" '((lambda () (interactive) (org-agenda-todo "")) :which-key "Remove todo")
    "1" '((lambda () (interactive) (org-agenda-todo "TODO")) :which-key "-> TODO")
    "2" '((lambda () (interactive) (org-agenda-todo "NEXT")) :which-key "-> NEXT")
    "3" '((lambda () (interactive) (org-agenda-todo "PIN")) :which-key "-> PIN")
    "4" '((lambda () (interactive) (org-agenda-todo "DONE")) :which-key "-> DONE")
    "5" '((lambda () (interactive) (org-agenda-todo "ABRT")) :which-key "-> ABRT")
    "m" '(org-agenda-refile :which-key "Refile ")
    "<" '(org-agenda-deadline :which-key "Insert deadline")
    "." '(org-agenda-schedule :which-key "Schedule entry")
    ; Clocks
    "ci" '(org-agenda-clock-in :which-key "Clock in")
    "co" '(org-agenda-clock-out :which-key "Clock out")
    "cu" '(org-agenda-clock-cancel :which-key "Cancel current clock")
    "cg" '(org-agenda-clock-goto :which-key "Go to current clock")
    "s" '(org-agenda-show-and-scroll-up :which-key "Reveal entry")
    "S" '(my/org-agenda-add-subtask :which-key "Add subtask")
    "C" '(my/agenda-set-category-or-context :which-key "Set category or context")
    "@" '(org-agenda-archive-default-with-confirmation :which-key "archive subtree")
    "]" '(org-agenda-set-restriction-lock-from-agenda :which-key "Restrict")
    "[" '(org-agenda-remove-restriction-lock :which-key "Unrestrict")))

(use-package org-super-agenda
  :ensure t
  :config
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
  (evil-org-agenda-set-keys)
  (evil-set-initial-state 'org-agenda-mode 'normal))

(org-reload)
