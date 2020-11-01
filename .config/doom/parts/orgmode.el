(setq org-caldav-calendars (list)
      plstore-cache-passphrase-for-symmetric-encryption t)

(defun my/agenda-do (x)
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
        (call-interactively x)
        (org-save-all-org-buffers)))
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo-all)))))

(defun my/is-refile-target-p ()
  (let ((cat (org-entry-get (point) "CATEGORY" nil))
        (has-todo (equal (org-get-todo-state) "TODO")))
    (or (string-match-p "perspective" cat)
        has-todo)))

(defun my/skip-non-top-project ()
  "Skip entries that are sub-todos"
  (save-restriction
    (widen)
    (let ((todo (org-get-todo-state))
          (parent-todo nil)
          (lvl (org-current-level)))
      (if todo
          (save-restriction
            (widen)
            (save-excursion
              (while (and lvl
                          (> lvl 1)
                          (not parent-todo))
                (progn
                  (outline-up-heading 1)
                  (setq parent-todo (org-get-todo-state)
                        lvl (org-current-level))))))
        (setq parent-todo 1))
      (if parent-todo
          (save-excursion (or (outline-next-heading) (point-max)))
        nil))))

(defun my/is-top-project-p ()
  (let ((cat (org-entry-get (point) "CATEGORY" nil))
        (has-todo (equal (org-get-todo-state) "TODO")))
    (or (string-match-p "PERSPECTIVE" cat
         has-todo))))

(setq org-directory "~/Documents/Notes/"
      my/workspace-root "~/Documents/Workspaces/"
      org-src-tab-acts-natively t
      org-use-tag-inheritance t
      org-clock-out-remove-zero-time-clocks t
      org-agenda-files `(,org-directory)
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-refile-use-outline-path nil
      org-refile-target-verify-function 'my/is-refile-target-p
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-enforce-todo-checkbox-dependencies t
      org-enforce-todo-dependencies t
      org-habit-show-habits t
      org-todo-repeat-to-state "NEXT")

(after! org
  (setq org-attach-method 'mv
        org-attach-preferred-new-method 'id
        org-attach-use-inheritance nil
        org-attach-id-dir (concat org-directory ".attach")))

;; Capture
(after! org
  (setq org-capture-templates `(("e" "Entry" entry (file ,(concat org-directory "inbox.org"))
                                 "* %?\n%a\n"
                                 :clock-in t
                                 :clock-resume t)
                                ("j" "Journal Entry" plain (function my/capture-location)
                                 "*%<%H:%M> %?\n%a\n"
                                 :clock-in t
                                 :clock-resume t)
                                ("." "Popup template" entry (file ,(concat org-directory "inbox.org"))
                                 "* %?\n%x\n"
                                 :clock-in t
                                 :clock-resume t))))

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
                              (transient . t)
                              (minibuffer . nil)
                              (window-system . x)))))
    (set-frame-size frame f-width f-height t)
    (set-frame-position frame f-left f-top)
    (select-frame-set-input-focus frame)
    (with-selected-frame frame
      (switch-to-buffer "*scratch*")
      (require 'org-capture)
      (condition-case ex
          (cl-letf (((symbol-function 'pop-to-buffer) #'switch-to-buffer))
            (setq org-capture-entry (org-capture-select-template "."))
            (org-capture))
        ('error
         (message "org capture: %s" (error-message-string ex))
         (delete-frame frame))))))

(map!
 :leader
 :desc "Org capture"
 "X" #'(lambda () (interactive)
         (org-capture nil "e")))

;; Todos

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PIN(p)" "|" "DONE(d)" "ABRT(a)"))
        org-todo-keyword-faces '(("TODO" :foreground "royal blue" :weight bold)
                                 ("NEXT" :foreground "dodger blue" :weight bold)
                                 ("PIN" :foreground "DarkOrange1" :weight bold)
                                 ("DONE" :foreground "olive drab" :weight bold)
                                 ("ABRT" :foreground "dark olive green" :strike-through t))))
(map!
 :localleader
 :map org-mode-map
 :desc "Remove todo"
 "0" '(lambda () (interactive) (org-todo "")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Remove todo"
 "0" '(lambda () (interactive) (org-agenda-todo "")))

(map!
 :localleader
 :map org-mode-map
 :desc "-> TODO"
 "1" '(lambda () (interactive) (org-todo "TODO")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "-> TODO"
 "1" '(lambda () (interactive) (org-agenda-todo "TODO")))

(map!
 :localleader
 :map org-mode-map
 :desc "-> NEXT"
 "2" '(lambda () (interactive) (org-todo "NEXT")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "-> NEXT"
 "2" '(lambda () (interactive) (org-agenda-todo "NEXT")))

(map!
 :localleader
 :map org-mode-map
 :desc "-> PIN"
 "3" '(lambda () (interactive) (org-todo "PIN")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "-> PIN"
 "3" '(lambda () (interactive) (org-agenda-todo "PIN")))

(map!
 :localleader
 :map org-mode-map
 :desc "-> DONE"
 "4" '(lambda () (interactive) (org-todo "DONE")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "-> DONE"
 "4" '(lambda () (interactive) (org-agenda-todo "DONE")))

(map!
 :localleader
 :map org-mode-map
 :desc "-> ABORT"
 "5" '(lambda () (interactive) (org-todo "ABRT")))
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "-> ABORT"
 "5" '(lambda () (interactive) (org-agenda-todo "ABRT")))

(defun my/unpin-actions ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-todo "NEXT")
     "/PIN"
     agenda)))

(map!
 :leader
 :desc "Unpin actions"
 "nu" #'my/unpin-action)

;; Subtrees
(map!
 :localleader
 :map org-mode-map
 "\\" 'org-reveal)

;; Clocks
(map!
 :localleader
 :map org-mode-map
 "cr" 'org-clock-update-time-maybe)

(defun my/material-icon (name)
  "Returns an all-the-icons-material icon"
  (list (all-the-icons-material name)))

(setq org-agenda-category-icon-alist (list))

;;;;;;;;;;;;;;;;;;;;
;; Kinds of tasks ;;
;;;;;;;;;;;;;;;;;;;;
(setq my/kinds (list))
(defun my/def-kind (cat icon)
  (add-to-list 'my/kinds cat)
  (add-to-list 'org-agenda-category-icon-alist `(,cat ,(my/material-icon icon))))

(my/def-kind "DIY" "build")
(my/def-kind "buy" "shopping_cart")
(my/def-kind "code" "code")
(my/def-kind "read" "library_books")
(my/def-kind "write" "edit")
(my/def-kind "mail" "email")
(my/def-kind "search" "search")
(my/def-kind "phonecall" "call")
(my/def-kind "admin" "account_balance")
(my/def-kind "org" "swap_horiz")
(my/def-kind "discuss" "message")

;;;;;;;;;;;;;;;;;;;;;;;;;
;; category of entries ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(setq my/categories (list))
(defun my/def-cat (cat icon)
  (add-to-list 'my/categories cat)
  (add-to-list 'org-agenda-category-icon-alist `(,cat ,(my/material-icon icon))))

(my/def-cat "link" "link")
(my/def-cat "perspective" "visibility")
(my/def-cat "mail" "email")
(my/def-cat "meeting" "group")
(my/def-cat "notes" "event_note")
(my/def-cat "appointment" "date_range")
(my/def-cat "birthday" "cake")
(my/def-cat "doc" "attachment")
(my/def-cat "book" "book")

;;;;;;;;;;;;
;; people ;;
;;;;;;;;;;;;
(setq my/people (list))
(defun my/def-people (p)
  (add-to-list 'my/people (concat "@" p)))

(setq my/current-people "@Nobody")

(defun my/select-people ()
  (interactive)
  (setq my/current-people (ivy-completing-read "person: " my/people))
  (message (concat "Curent person set to " my/current-people))
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo-all))))

(defun my/skip-not-current-people ()
  "Skip entries that already has actions attached to it"
  (if (member my/current-people (org-get-tags nil))
      nil
    (save-excursion (or (outline-next-heading) (point-max)))))

(defun my/pretty-people (x)
  (let ((case-fold-search nil))
    (replace-regexp-in-string "@ " ""
                              (replace-regexp-in-string "[A-Z]" " \\&" x))))

(map!
 :leader
 :desc "Set filter"
 "n@" #'my/select-people)

(defun my/toggle-delegate ()
  (interactive)
  (let* ((completion-ignore-case)
         (is-delegated (member "WAIT" (org-get-tags nil t)))
         (delegated-to (seq-filter (lambda (x) x) (org-get-tags nil t))))
    (if is-delegated
        (progn
          (org-toggle-tag "WAIT" 'off)
          (map nil (lambda (x) (org-toggle-tag x 'off)) delegated-to))
      (let* ((delegate-to (ivy-completing-read "delegate to: " my/people)))
        (progn
          (org-toggle-tag "WAIT" 'on)
          (org-toggle-tag delegate-to 'on))))))

(map!
 :localleader
 :map org-mode-map
 :desc "Delegate"
 "D" 'my/toggle-delegate)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Delegate"
 "D" '(lambda ()
        (interactive)
        (my/agenda-do 'my/toggle-delegate)))

(defun my/toggle-people ()
  (interactive)
  (let* ((people (ivy-completing-read "person: " my/people)))
    (org-toggle-tag people)))

(map!
 :localleader
 :map org-mode-map
 :desc "People"
 "@" 'my/toggle-people)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "People"
 "@" '(lambda ()
        (interactive)
        (my/agenda-do 'my/toggle-people)))


;;;;;;;;;;;;;;;;;;
;; special tags ;;
;;;;;;;;;;;;;;;;;;
(setq my/at-tag (list))
(defun my/def-at-tag (tag)
  (add-to-list 'my/at-tag tag))

(my/def-at-tag "WAIT")
(my/def-at-tag "MAYBE")

;;;;;;;;;;;;;
;; filters ;;
;;;;;;;;;;;;;
(setq my/filters (list))
(defun my/def-filter (f)
  (add-to-list 'my/filters (concat "f_" f)))

(defun my/toggle-filter()
  (interactive)
  (let* ((filter (ivy-completing-read "filter: " my/filters)))
    (org-toggle-tag filter)))

(map!
 :localleader
 :map org-mode-map
 :desc "Filter"
 "!" 'my/toggle-filter)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Filter"
 "!" '(lambda ()
        (interactive)
        (my/agenda-do 'my/toggle-filter)))


(setq my/current-filter nil)

(defun my/cycle-filters ()
  (interactive)
  (setq my/current-filter (if my/current-filter (my/next-in-list my/current-filter my/filters) (car my/filters)))
  (message (concat "Curent filter set to " my/current-filter))
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo-all))))

(defun my/skip-not-current-filter ()
  "Skip entries that already has actions attached to it"
  (if (member my/current-filter (org-get-tags nil))
      nil
    (save-excursion (or (outline-next-heading) (point-max)))))

(map!
 :leader
 :desc "Set filter"
 "n!" #'my/cycle-filters)


;; Categories
(after! emojify
  (add-hook 'after-init-hook #'global-emojify-mode))


(defun my/set-kind-or-category ()
  (interactive)
  (let* ((completion-ignore-case t)
         (prop "CATEGORY")
         (pom (point-at-bol))
         (has-context (s-matches? "^@.*" (org-get-category pom)))
         (has-todo (member (org-get-todo-state) org-todo-keywords-1))
         (val (if has-todo
                  (completing-read "Kind: " my/kinds)
                (completing-read "Category: " my/categories))))
    (org-entry-put pom prop val)))

(map!
 :localleader
 :map org-mode-map
 :desc "Set kind or category"
 "C" 'my/set-kind-or-category)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Set kind or category"
 "C" '(lambda ()
        (interactive)
        (my/agenda-do 'my/set-kind-or-category)))

;; Workspaces
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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun my/term-in-workspace ()
  (interactive)
  (my/open-or-create-workspace #'(lambda () (eshell-here))))

(defun my/dired-in-workspace ()
  (interactive)
  (my/open-or-create-workspace #'(lambda () (dired default-directory))))

(map!
 :localleader
 :map org-mode-map
 :desc "Term in workspace"
 "W" #'my/term-in-workspace)

(map!
 :localleader
 :map org-mode-map
 :desc "Open workspace"
 "w" #'my/dired-in-workspace)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Term in workspace"
 "W" '(lambda ()
        (interactive)
        (my/agenda-do 'my/term-in-workspace)))

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Open workspace"
 "w" '(lambda ()
        (interactive)
        (my/agenda-do 'my/dired-in-workspace)))

;; Agendas

(use-package! org-super-agenda
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-header-map nil))

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


(setq org-agenda-custom-commands '())

(add-to-list 'org-agenda-custom-commands
             `("g2" "GTD: clarify"
               ((tags "+TAGS={^.*$}"
                      ((org-agenda-overriding-header "Clarify/Organize")
                       (org-super-agenda-groups
                        '((:name "todo (SPC m 1) or Categorize (SPC m C)"
                           :and (:todo nil
                                 :not (:regexp "CATEGORY:")))
                          (:name "Add next step (SPC m S) or MAYBE (SPC m q)"
                           :and (:todo "TODO"
                                 :not (:children "NEXT")
                                 :not (:tag "MAYBE")))
                          (:name "Add a kind (SPC m C)"
                           :and (:not (:regexp "CATEGORY:")
                                 :todo "NEXT"
                                 :not (:tag "MAYBE")))
                          (:name "Add a filter (SPC m !)"
                           :and (:todo "NEXT"
                                 :not (:regexp ":f_.*")
                                 :not (:tag "MAYBE")))
                          (:name "Delegate (SPC m @)"
                           :and (:todo "NEXT"
                                 :not (:regexp ":@.")
                                 :not (:tag "MAYBE")))
                          (:name "Move to an existing project (SPC m r), delete (dd)"
                           :regexp "^\* ")))
                       (org-agenda-prefix-format "%i %s")
                       (org-agenda-files '(,(concat org-directory "inbox.org")
                                           ,(concat org-directory "inbox-phone.org")
                                           ,(concat org-directory "inbox-calendar-perso.org"))))))))

(map!
 :leader
 :desc "GTD: Clarify"
 "n1" #'(lambda () (interactive) (org-agenda nil "g2")))

(add-to-list 'org-agenda-custom-commands
             `("g4" "GTD: Plan the day"
               ((agenda ""
                        ((org-agenda-overriding-header "Plan the day")
                         (org-agenda-start-day "0d")
                         (org-agenda-prefix-format "%i %s")
                         (org-agenda-span 3)
                         (org-deadline-warning-days 0)
                         (org-agenda-repeating-timestamp-show-all t)
                         (org-agenda-include-deadlines t)))
                (tags-todo "-MAYBE-WAIT/PIN"
                           ((org-agenda-overriding-header "Current picks")
                            (org-agenda-prefix-format "%i %s")
                            (org-agenda-skip-function 'my/skip-not-current-filter)))
                (tags-todo "-MAYBE-WAIT+SCHEDULED={^$}/NEXT"
                           ((org-agenda-overriding-header (concat "Today candidates (" (string-remove-prefix "f_"  my/current-filter) ")"))
                            (org-agenda-prefix-format "%i %s")
                            (org-agenda-skip-function 'my/skip-not-current-filter)
                            (org-super-agenda-groups
                              '((:auto-category t))))))))
                           

(map!
 :leader
 :desc "GTD: Plan the day"
 "n3" #'(lambda () (interactive) (org-agenda nil "g4")))

(add-to-list 'org-agenda-custom-commands
             `("g5" "GTD: Today"
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
                (tags-todo "-MAYBE-WAIT/PIN"
                           ((org-agenda-overriding-header "Picked for today")
                            (org-agenda-prefix-format "%i %s")))
                (tags-todo "TODO={^.*$}"
                           ((org-agenda-overriding-header "Fast track")
                            (org-agenda-prefix-format "%i %s")
                            (org-agenda-files '(,(concat org-directory "inbox.org")
                                                ,(concat org-directory "inbox-phone.org")))))
                (tags-todo "-MAYBE-WAIT+SCHEDULED={^$}/NEXT"
                           ((org-agenda-overriding-header (concat "Current Context (" (string-remove-prefix "f_" my/current-filter) ")"))
                            (org-agenda-prefix-format "%i %s")
                            (org-agenda-skip-function 'my/skip-not-current-filter)
                            (org-super-agenda-groups
                             '((:auto-category t))))))))

(map!
 :leader
 :desc "GTD: Today"
 "n4" #'(lambda () (interactive) (org-agenda nil "g5")))

(add-to-list 'org-agenda-custom-commands
             `("g55" "People view"
               ((tags my/current-people
                      ((org-agenda-overriding-header (my/pretty-people my/current-people))
                       (org-super-agenda-groups
                        '((:name "Delegated"
                           :and (:tag "WAIT"
                                 :not (:todo ("DONE" "ABRT"))))
                          (:name "Discuss"
                           :and (:category ("discuss" "phonecall" "mail")
                                 :not (:todo ("DONE" "ABRT"))))
                          (:name "Todo"
                           :todo ("TODO" "NEXT" "PIN"))
                          (:name "Finished tasks"
                           :todo ("DONE" "ABRT"))))
                       (org-agenda-prefix-format "%i %s"))))))

(map!
 :leader
 :desc "GTD: People"
 "n5" #'(lambda () (interactive) (org-agenda nil "g55")))

(add-to-list 'org-agenda-custom-commands
             `("g6" "GTD: Project Grooming"
               ;; Goals
               ((tags-todo "+TODO={.*}"
                 ((org-agenda-overriding-header "task policing")
                  (org-super-agenda-groups
                   '((:name "Tasks outside of filters"
                      :and (:todo "NEXT"
                            :not (:regexp ":f_.*")))
                     (:name "Tasks without context"
                        :and (:todo "NEXT"
                              :not (:regexp "CATEGORY:")))
                     (:name "Waiting tasks without assignee"
                        :and (:tag "WAIT"
                              :not (:regexp ":@.*")))
                     (:name "Waiting tasks without deadline"
                        :and (:tag "WAIT"
                              :todo "NEXT"
                              :deadline nil)
                        :discard (:anything t))))
                  (org-agenda-prefix-format "%l %i %s")))
                (tags-todo "+TODO={.*}"
                 ((org-agenda-overriding-header "Projects")
                  (org-super-agenda-groups
                   '((:name "Projects requiring attention"
                      :and (:todo "TODO"
                            :not (:tag ("MAYBE"))
                            :not (:children ("NEXT" "PIN"))))
                     (:name "Active Projects"
                      :and (:todo ("TODO" "NEXT" "PIN")
                            :not (:tag ("MAYBE"))))
                     (:name "Finished Projects"
                      :todo ("DONE" "ABRT"))
                     (:name "Potential Projects"
                      :tag "MAYBE")))
                  (org-agenda-prefix-format "%l %i %s")
                  (org-agenda-skip-function 'my/skip-non-top-project))))))

(map!
 :leader
 :desc "GTD: Project grooming"
 "n7" #'(lambda () (interactive) (org-agenda nil "g6")))



(add-to-list 'org-agenda-custom-commands
             `("g7" "Perspective Review"
               ((tags "+CATEGORY={.*}"
                      ((org-agenda-overriding-header "Perspective Review")
                       (org-super-agenda-groups
                        '((:name "Perspectives"
                           :and (:category "perspective"
                                 :regexp ":CATEGORY:"))
                          (:name "Done"
                           :todo "DONE")
                          (:name "Next"
                           :and (:todo ("NEXT" "PIN")
                                 :not (:tag "MAYBE")))
                          (:name "Todo without next actions"
                           :and (:todo "TODO"
                                 :not (:tag ("MAYBE"))
                                 :not (:children ("NEXT" "PIN"))))
                          (:name "Todo"
                           :and (:todo "TODO"
                                 :not (:tag "MAYBE")))
                          (:name "Ideas"
                           :tag "MAYBE")
                          (:name "References & links"
                           :regexp ":CATEGORY:")))
                       (org-agenda-prefix-format "%l %i %s"))))))
(map!
 :leader
 :desc "GTD: Perspective review"
 "n8" #'(lambda () (interactive) (org-agenda nil "g7")))

(add-to-list 'org-agenda-custom-commands
             `("g9" "Daily review"
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
                            :gap-ok-around ("4:00" "13:00" "14:00" "19:00"))))))))

(map!
 :leader
 :desc "GTD: Review the day"
 "n9" #'(lambda () (interactive) (org-agenda nil "g9")))

(add-to-list 'org-agenda-custom-commands
             `("g0" "Weekly review"
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

(map!
 :leader
 :desc "GTD: Review the past week"
 "n0" #'(lambda () (interactive) (org-agenda nil "g0")))
(after! org
  (add-hook 'auto-save-hook 'org-save-all-org-buffers))
(after! org-agenda
  (set-popup-rules!
    '(("^\\*Org Agenda" :slot 1 :side right :width 40 :select t)))
  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode))))

;; Subtask
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

(map!
 :localleader
 :map org-mode-map
 :desc "Add subtask"
 "S" #'my/org-add-subtask)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Add subtask"
 "S" #'my/org-agenda-add-subtask)

;; Restrictions
(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Restrict"
 "]" #'org-agenda-set-restriction-lock-from-agenda)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Unrestrict"
 "[" #'org-agenda-remove-restriction-lock)

(map!
 :localleader
 :map dired-mode-map
 :desc "Attach"
 "a" #'org-attach-dired-to-subtree)
