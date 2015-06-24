;;; org-report.el --- Generate status reports from Org mode files

;; Copyright (C) 2015  Michael Strey

;; Author: Michael Strey <mstrey@strey.biz>
;; Keywords: Org, mode, report, email, period, status

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The code exports reports of sales activities from one or more
;; Org mode source files.  It can generate as well complete reports
;; from a designated subtree as differential reports made over a
;; period of time of the same subtree.

;; Caveats:

;; Files in SOURCES_TARGETS must be Org mode files containing sections
;; named according to HEADING in CLIENTS.  The program expects a
;; property field :EXPORT_DATE: in the respective subtree.

;;; Code:

(require 'org)

(defgroup org-report nil
  "Options about automatic report generation."
  :group 'org)

(defcustom *clients*
  '((Glovane "* Glovane" "yeounggyu.kim@glovane.com" "sales_@glovane.com")
    (Actia "* ACTIA" "f.desclaux@actiatelecom.fr" nil))
  "alist of clients consisting of KEY, regular expression to find a HEADING
for the subtree to export, TO: field for email message, CC: field of email message"
  :type '(editable-list)
  :group 'org-report)
  )

(defcustom *sources_targets*
  '((leads "/home/strey/GTD/customers.org" "/tmp/" "leads")
    (sales "/home/strey/GTD/salesprojects.org" "/tmp/" "sales_projects"))
  "alist defining sources and targets for the various reports
KEY, Orgmode SOURCE, target PATH, target FILE_NAME"
  :type '(editable-list)
  :group 'org-report)
  )

(defcustom *periods*
  '(default nil ("2015-06-11" "2015-06-21"))
  "Periods of time to create reports for.  NIL stands for a complete report."
  :type '(editable-list)
  :group 'org-report)
  )

(defun ms-iso-date-to-string (iso-date)
  "Convert a string with ISO-DATE of the form \"2015-06-10\" into a string
of the form \"150610\" for use in file names"
  (format-time-string "%y%m%d" (org-time-string-to-time iso-date)))

(defun ms-create-target-file-name (report period)
  "Create a file name from a list of report parameters"
  (let ((path (cadddr report))
        (radical (nth 4 report))
        (heading (downcase (cadr (split-string (cadr report))))))
    (concat path "rep-"  radical "_" heading
            (when period
              (concat "-"
                      (ms-iso-date-to-string (car period)) "_"
                      (ms-iso-date-to-string (cadr period)))))))

(defun ms-day-before (iso-date)
  "Return an orgmode time stamp of the day before the given ISO-DATE."
  (format-time-string "%Y-%m-%d %a"
                      (time-subtract (org-time-string-to-time iso-date)
                                     (seconds-to-time 86400))))

(defun ms-day-after (iso-date)
  "Return an orgmode time stamp of the day before the given ISO-DATE."
  (format-time-string "%Y-%m-%d %a"
                      (time-add (org-time-string-to-time iso-date)
                                     (seconds-to-time 86400))))

(defun ms-export-report (report)
  "Export one report.

REPORT is a list of parameters describing a report consisting of:
- PERIOD is a period of time defined by a list of start-date end-date or nil resp..
- SOURCE is the name of the orgmode source file.
- PATH is the output path for the target file.
- FILE_NAME is a string to construct the target file name with.
- SUBTREE is a string containing a regular expression to find the heading of
the subtree to export."
  (let* ((period (car report))
         (subtree (cadr report))
         (source-file-name (caddr report)))
  (save-excursion
    (find-file source-file-name)
    (setq org-ts-type 'all)
    (goto-char (point-min))
    (re-search-forward subtree)
    (setq last_date (org-entry-get (point) "EXPORT_DATE"))
    (setq today (format-time-string "[%Y-%m-%d %a]" (current-time)))
    (org-entry-put (point) "EXPORT_DATE" today)
    (when period
      (when (eq period 'default)
        (setq period (cons (ms-day-before last_date) (cons (ms-day-after today) nil))))
      (org-check-dates-range (car period) (cadr period)))
    (goto-char (point-min))
    (re-search-forward subtree)
    (setq target-file-name (ms-create-target-file-name report period))
    (setq outfile (concat target-file-name ".tex"))
    (org-export-to-file 'latex outfile
      nil t period nil nil
      (lambda (file) (org-latex-compile file)))
    )
  (concat target-file-name ".pdf")))
         
(defun ms-make-reports-list (keylist sources_targets subtree periods)
  "Create a list of lists, where each list is describing one report (PDF file)
with the following keys:

- PERIOD is a period of time defined by a list of start-date end-date or nil resp..
- SUBTREE is a string containing a regular expression to find the heading of
- SOURCE is the name of the orgmode source file.
the subtree to export.
- PATH is the output path for the target file.
- FILE_NAME is a string to construct the target file name with."
  (let ((sources (mapcar (lambda (key)
                           (assoc key sources_targets))
                         keylist)))  
    (-flatten-n 1
                (mapcar
                 (lambda (source_target)
                   (mapcar (lambda (period)
                             (cons period (cons subtree (cdr source_target))))
                           periods))
                 sources))))

(defun ms-create-report-message (client clients keylist sources_targets periods body)
  "Create a message with an attached list of PDF reports exported from files in sources.

CLIENT is a symbol to chose a set of client specific parameters from the alist CLIENTS.
KEYLIST is a list of keys to chose the reports to be exported from the alist
SOURCES_TARGETS.
PERIODS is a list of time periods to restrict the outputs to given periods of time.
NIL in this list causes a complete report without time constraints, 
DEFAULT creates a report starting one day before the last EXPORT_DATE and ending today. 
Take care that DEFAULT is the first element of the list, since EXPORT_DATE will be set to 
today's date as side effect of function MS-EXPORT-REPORT."
  (let* ((today (current-time-string))
         (clientlist (assoc client clients))
         (subtree (cadr clientlist))
         (to (caddr clientlist))
         (cc (cadddr clientlist))
         (reports (ms-make-reports-list keylist sources_targets subtree periods)))
    (compose-mail
     to
     (format "Weekly report %s" today)
     (when cc
       (cons (cons "Cc" cc) nil)))
    (message-goto-body)
    (insert body)
    (dolist (report reports) (mml-attach-file (ms-export-report report)))
    ))

(ms-create-report-message 'Glovane *clients* '(leads sales) *sources_targets* '(default nil) 
                          "Dear Mr. Kim,\n\nPlease find attached my weekly reports.\n\nBest regards\nMichael Strey\n\n")
 #+end_src

(provide 'org-report)

;;; org-report.el ends here
