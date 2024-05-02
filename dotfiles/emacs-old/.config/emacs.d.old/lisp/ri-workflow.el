(setq org-directory "~/Notes")



;; --- org-agenda ------
(setq org-deadline-warning-days 14
      org-agenda-start-with-log-mode t ; enable log-mode by def
      org-log-done 'time
      org-log-into-drawer t) ; ?

;; --- agenda files ------ (turn this into a flet func?)
(let ((list '("~/Notes/org/"
              "~/Notes/School/"
              "~/uuouhtonu/"))) ; to return, blank list
  (setq org-agenda-files
        (delete nil
                (mapcar (lambda (file)
                          (if (f-dir-p file)
                              file))
                        list))))

(defvar ri/agenda.org "~/Notes/org/agenda.org") ; just extra

;; --- todo keywords ------
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
;;         (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

;; --- commonly known tasks to appear when counsel-org-tag ------
;; org-set-tags-command ?
(setq org-tag-alist
      '((:startgroup)
        ;; Put mutually exclusive tags here
        (:endgroup)
        ("habit" . ?h)
        ("daily" . ?d)
        ("event" . ?e)
        ("assignment" . ?a)
        ("personal" . ?p)))

;; ("@errand" . ?E)
;; ("@home" . ?H)
;; ("@work" . ?W)
;; ("agenda" . ?a)
;; ("planning" . ?p)
;; ("daily" . ?d)
;; ("publish" . ?P)
;; ("batch" . ?b)
;; ("note" . ?n)
;; ("idea" . ?i)

;; --- Custom Agenda Views! ------
;; (easier with org-ql)
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ;; ("W" "Work Tasks" tags-todo "+work-email")

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ;; ("w" "Workflow Status"
        ;;  ((todo "WAIT"
        ;;         ((org-agenda-overriding-header "Waiting on External")
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "REVIEW"
        ;;         ((org-agenda-overriding-header "In Review")
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "PLAN"
        ;;         ((org-agenda-overriding-header "In Planning")
        ;;          (org-agenda-todo-list-sublevels nil)
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "BACKLOG"
        ;;         ((org-agenda-overriding-header "Project Backlog")
        ;;          (org-agenda-todo-list-sublevels nil)
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "READY"
        ;;         ((org-agenda-overriding-header "Ready for Work")
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "ACTIVE"
        ;;         ((org-agenda-overriding-header "Active Projects")
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "COMPLETED"
        ;;         ((org-agenda-overriding-header "Completed Projects")
        ;;          (org-agenda-files org-agenda-files)))
        ;;   (todo "CANC"
        ;;         ((org-agenda-overriding-header "Cancelled Projects")
        ;;          (org-agenda-files org-agenda-files)))))
        ))

;; --- Org Capture Templates! ------
(setq org-default-notes-file "org/misc.org") ; if no path specified below
;; (basically quickly add new entries mindlessly)
(setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "org/agenda.org" "Main")
         "* TODO %?\n  %T\n  %i" :empty-lines 1)
        ("tc" "At-point" entry (file+olp "org/agenda.org" "Main")
         "* TODO %?\n  %T\n  %a\n  %i" :empty-lines 1)
        ("tf" "File-task" entry (file+olp ri/agenda.org "Main")
         "* TODO %?\n  %T\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
         (file+olp+datetree "org/journal.org")
         "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
         :clock-in :clock-resume
         :empty-lines 1)

        ("jm" "Meeting" entry
         (file+olp+datetree "org/journal.org")
         "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
         :clock-in :clock-resume
         :empty-lines 1)

        ("w" "Workflows")
        ("we" "Checking Email" entry (file+olp+datetree "~/Notes/org/journal.org")
         "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "org/metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

;; --- org-refile ------
;; (add target locations for org-refile)
(setq org-refile-targets
      '(("Archive.org" :maxlevel . 1)
        ("work.org" :maxlevel . 1)))
;; save org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; --- org-habit ------
;; (require 'org-habit)
;; (add-to-list 'org-modules 'org-habit)
;; (setq org-habit-graph-column 60)

;; --- org-journal ------
(use-package org-journal
  :config
  (setq org-journal-dir "~/Notes/org/journal/"
        ;; org-journal-date-format "%B %d, %Y (%A) "
        ;; org-journal-file-format "%Y-%m-%d.org"
        ))

(provide 'ri-workflow)
