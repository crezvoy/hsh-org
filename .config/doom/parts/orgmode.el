(defun my/is-refile-target-p ()
  (let ((cat (org-entry-get (point) "CATEGORY" nil))
        (has-todo (equal (org-get-todo-state) "TODO")))
    (or (string-match-p "PERSPECTIVE" cat)
        (string-match-p "IDEA" cat)
        has-todo)))

(setq org-directory "~/Documents/Notes"
      org-attach-id-dir (concat org-directory ".attach")
      org-attach-use-inheritance nil
      my/workspace-root "~/Documents/Workspaces/"
      org-src-tab-acts-natively t
      org-use-tag-inheritance t
      org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9))
      org-refile-use-outline-path nil
      org-refile-target-verify-function 'my/is-refile-target-p)

;; Capture
(setq org-capture-templates `(("e" "Entry" entry (file ,(concat org-directory "inbox.org"))
                               "* %?\n%a\n")
                              :clock-in t
                              :clock-resume t
                              ("j" "Journal Entry" plain (function my/capture-location))
                              "*%<%H:%M> %?\n%a\n"
                              :clock-in t
                              :clock-resume t
                              ("." "Popup template" entry (file ,(concat org-directory "inbox.org")))
                              "* %?\n%x\n"
                              :clock-in t
                              :clock-resume t))

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

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PIN(p)" "|" "DONE(d)" "ABRT(a)"))
      org-todo-keyword-faces '(("TODO" :foreground "royal blue" :weight bold)
                               ("NEXT" :foreground "dodger blue" :weight bold)
                               ("PIN" :foreground "DarkOrange1" :weight bold)
                               ("DONE" :foreground "olive drab" :weight bold)
                               ("ABRT" :foreground "dark olive green" :strike-through t)))
(map!
 :localleader
 :map org-mode-map
 :desc "Remove todo"
 "0" '(lambda () (interactive) (org-todo "")))
(map!
 :localleader
 :map org-mode-map
 :desc "-> TODO"
 "1" '(lambda () (interactive) (org-todo "TODO")))
(map!
 :localleader
 :map org-mode-map
 :desc "-> NEXT"
 "2" '(lambda () (interactive) (org-todo "NEXT")))
(map!
 :localleader
 :map org-mode-map
 :desc "-> PIN"
 "3" '(lambda () (interactive) (org-todo "PIN")))
(map!
 :localleader
 :map org-mode-map
 :desc "-> DONE"
 "4" '(lambda () (interactive) (org-todo "DONE")))
(map!
 :localleader
 :map org-mode-map
 :desc "-> ABORT"
 "5" '(lambda () (interactive) (org-todo "ABORT")))

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

;; Contexts
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

(map!
 :leader
 :desc "Set context"
 "n@" #'my/cycle-context)


;; Categories
(use-package! emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(setq
 my/emoji-dir (concat emojify-emojis-dir "/" emojify-emoji-set "/")
 my/categories '("DISCUSS"
                 "DOC"
                 "IDEA"
                 "LINKS"
                 "MAIL"
                 "MEETING_NOTES"
                 "PERSPECTIVE"
                 "PICTURE"
                 "SHEETMUSIC"
                 "TIPSNTRICKS")
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
   ("^@tools" ,(concat my/emoji-dir "1f528.png") nil nil :ascent center)))

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

(map!
 :localleader
 :map org-mode-map
 :desc "Set category or context"
 "C" 'my/set-category-or-context)

(defun my/agenda-set-category-or-context ()
  "Set the category for the current headline."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((completion-ignore-case t))
   (hdmarker (or (org-get-at-bol 'org-hd-marker))
           (org-agenda-error))
   (buffer (marker-buffer hdmarker))
   (pos (marker-position hdmarker))
   (inhibit-read-only t)
   newhead
    (org-with-remote-undo buffer
      (with-current-buffer buffer)))
  (widen)
  (goto-char pos)
  (org-show-context 'agenda)
  (call-interactively 'my/set-category-or-context)
  (end-of-line 1)
  (setq newhead (org-get-heading)
      (org-agenda-change-all-lines newhead hdmarker)))

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Set category or context"
 "C" 'my/agenda-set-category-or-context)

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

(defun my/term-in-workspace ()
  (interactive)
  (open-or-create-workspace #'(lambda () (term "/bin/bash"))))

(defun my/dired-in-workspace ()
  (interactive)
  (open-or-create-workspace #'(lambda () (dired default-directory))))

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
 "W" #'my/term-in-workspace)

(map!
 :localleader
 :map org-agenda-mode-map
 :desc "Open workspace"
 "w" #'my/dired-in-workspace)

;; Agendas

(use-package! org-super-agenda
  :ensure t
  :config
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
    (let ((has-subtask nil)))
    (subtree-end (save-excursion (org-end-of-subtree t))
      (save-excursion)))
  (forward-line 1)
  (while (and (not has-subtask))
        (< (point) subtree-end)
        (re-search-forward "^\*+ " subtree-end t)
    (progn
      (when (equal (org-get-todo-state) "NEXT")
        (setq has-subtask t))
      (if (not has-subtask)
          nil
        (save-excursion (or (outline-next-heading) (point-max)))))))

(defun my/skip-not-current-context ()
  "Skip entries that already has actions attached to it"
  (if (equal (org-get-category) my/current-context)
      nil
    (save-excursion (or (outline-next-heading) (point-max)))))

(setq org-agenda-custom-commands '())

(add-to-list 'org-agenda-custom-commands
             `("g2" "GTD: clarify"
               ((tags "+CATEGORY={^inbox$}+TODO={^$}")
                ((org-agenda-overriding-header "What is it: TODO (SPC m t)? or entry to be categorized (SPC m C)")
                 (org-agenda-prefix-format "%s")
                 (org-agenda-skip-function 'my/skip-categorized)
                 (org-agenda-files '(,(concat org-directory "inbox.org"))))
                (tags "-CATEGORY={^inbox$}-CATEGORY={PERSPECTIVE}-CATEGORY={IDEA}-CATEGORY={RSS}+TODO={^$}+LEVEL=1|CATEGORY={RSS}+TODO={^$}+LEVEL=2")
                ((org-agenda-overriding-header "Actionable (SPC m S)?")
                 (org-agenda-prefix-format "%i %s")
                 (org-agenda-skip-function 'my/skip-actionned)
                 (org-agenda-files '(,(concat org-directory "inbox.org"))))
                (tags-todo "+TODO={^TODO$}-CATEGORY={@wait}"
                           ((org-agenda-overriding-header "What is the next step? had action sub-headline (SPC m t n)")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-skip-function 'my/skip-has-next)
                (org-agenda-files '(,(concat org-directory "inbox.org")))
                (tags-todo "+TODO={^NEXT$}-CATEGORY={^[@].*}"
                           ((org-agenda-overriding-header "Delegate (SPC m C + wait), do myself later (SPC m C mine) ? ")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-files '(,(concat org-directory "inbox.org")))
                (tags-todo "+LEVEL=1+TODO={^$}+CATEGORY={[A-Z_]+}-CATEGORY=PERSPECTIVE|LEVEL=2+TODO={^$}+CATEGORY={[A-Z_]+}-CATEGORY=PERSPECTIVE|CATEGORY={RSS}+TODO={^$}+LEVEL=2"
                           ((org-agenda-overriding-header "Worth keeping (C-k to delete) ?")))
                (org-agenda-skip-function 'my/skip-has-next)
                (org-agenda-files '(,(concat org-directory "inbox.org"))))))

(map!
 :leader
 :desc "GTD: Clarify"
 "n1" #'(lambda () (interactive) (org-agenda nil "g2")))

(add-to-list 'org-agenda-custom-commands
             `("g3" "GTD: Organize"
               ((tags-todo "+CATEGORY={^@mine$}+TODO={NEXT}"
                         ((org-agenda-overriding-header "Add a context: SPC m C")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-files '(,(concat org-directory "inbox.org")))
                (tags "CATEGORY={[@].*}+TODO={.+}+LEVEL=1|CATEGORY={^[A-Z_]\+$}+LEVEL=1-CATEGORY={RSS}|CATEGORY={^RSS$}+LEVEL=2")
                ((org-agenda-overriding-header "Move to an existing project or a perpective: SPC m r")
                 (org-agenda-prefix-format "%i %s")
                 (org-agenda-files '(,(concat org-directory "inbox.org")))))))

(map!
 :leader
 :desc "GTD: Organize"
 "n2" #'(lambda () (interactive) (org-agenda nil "g3")))

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
                (tags-todo "TODO={^NEXT$}-CATEGORY={^@wait$}"
                           ((org-agenda-overriding-header "Today candidates")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-skip-function 'my/skip-not-current-context)
                (org-super-agenda-groups
                 '((:auto-category t))))))

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
                         (org-agenda-time-grid '((daily today))
                                               (800 1000 1200 1400 1600 1800 2000)
                                               "················"
                                               "----------------")
                         (org-agenda-prefix-format "%i %s")
                         (org-agenda-include-deadlines nil)))
                (tags-todo "+TODO={^PIN$}-CATEGORY={^@wait$}"
                           ((org-agenda-overriding-header "Picked for today")))
                (org-agenda-prefix-format "%i %s")
                (tags-todo "TODO={^.*$}"
                           ((org-agenda-overriding-header "Fast track")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-files '(,(concat org-directory "inbox.org")))
                (tags-todo "TODO={^NEXT$}"
                           ((org-agenda-overriding-header "Current Context")))
                (org-agenda-prefix-format "%i %s")
                (org-agenda-skip-function 'my/skip-not-current-context))))
(map!
 :leader
 :desc "GTD: Today"
 "n4" #'(lambda () (interactive) (org-agenda nil "g5")))

(add-to-list 'org-agenda-custom-commands
             `("g6" "GTD: Project Grooming"
               ;; Goals
               ((tags "+CATEGORY={.*}")
                ((org-agenda-overriding-header "Project Grooming")
                 (org-super-agenda-groups
                  '((:name "Projects that require attention"
                     :and (:todo "TODO")
                     :not (:category ("IDEA" "@wait") :tag ("@maybe"))
                     :not (:children ("NEXT" "PIN")))
                    (:name "uncategorized entries"
                     :category ""
                     :category nil)
                    (:name "tasks without context"
                     :and (:todo "NEXT")
                     :category "@mine"
                     :and (:todo "NEXT")
                     :not (:regexp ":CATEGORY:"))
                    (:name "Waiting task without deadline"
                     :and (:category "@wait")
                     :todo "NEXT"
                     :deadline nil)
                    (:name "Waiting task without assignee"
                     :and (:category "@wait")
                     :tag nil
                     :discard (:anything t))))
                 (org-agenda-prefix-format "%l %i %s")))))

(map!
 :leader
 :desc "GTD: Project grooming"
 "n7" #'(lambda () (interactive) (org-agenda nil "g6")))

(add-to-list 'org-agenda-custom-commands
             `("g7" "Perspective Review"
               ((tags "+CATEGORY={.*}")
                ((org-agenda-overriding-header "Perspective Review")
                 (org-super-agenda-groups
                  '((:name "Perspectives"
                     :and (:category "PERSPECTIVE")
                     :regexp ":CATEGORY:")
                    (:name "Done"
                     :todo "DONE")
                    (:name "Next"
                     :and (:todo ("NEXT" "PIN"))
                     :not (:tag "@maybe"))
                    (:name "Entries that require attention"
                     :and (:todo "TODO")
                     :not (:category ("IDEA" "@wait") :tag ("@maybe"))
                     :not (:children ("NEXT" "PIN")))
                    (:name "Todo"
                     :and (:todo "TODO"
                           :not (:tag "@maybe")))
                    (:name "References & links"
                     :and (:category ("DOC" "LINKS" "PICTURE" "MAIL" "TIPSNTRICKS"))
                     :regexp ":CATEGORY:")
                    (:name "Ideas"
                     :tag "@maybe"
                     :category "IDEA")))
                 (org-agenda-prefix-format "%l %i %s")))))
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

(after! org-agenda
  (set-popup-rules!
    '(("^\\*Org Agenda" :slot 1 :side right :width 40 :select t))))

;; Subtask
(defun my/org-add-subtask ()
  (interactive)
  (org-insert-todo-heading-respect-content)
  (org-demote))

(defun my/org-agenda-add-subtask ()
  "Add a time-stamped note to the entry at point."
  (interactive)
  (org-agenda-check-no-diary)
  (let* ((completion-ignore-case t))
    (marker (or (org-get-at-bol 'org-marker))
            (org-agenda-error))
    (buffer (marker-buffer marker))
    (pos (marker-position marker))
    (task (read-string "task: "))
    (inhibit-read-only t
                       (org-with-remote-undo buffer
                         (with-current-buffer buffer))))
  (widen)
  (goto-char pos)
  (org-show-context 'agenda)
  (my/org-add-subtask)
  (insert task)
  (end-of-line 1)
  (setq newhead (org-get-heading)
        (org-agenda-change-all-lines newhead marker)))

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
