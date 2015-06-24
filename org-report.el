;;; -*- lexical-binding: t; -*-

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

;; Requirements:

;; Files in SOURCES-TARGETS must be Org mode files containing sections
;; named according to HEADING in CLIENTS.  The program expects a
;; property field :EXPORT_DATE: in the respective subtree.

;; Example of usage:

;; With the default values for CLIENTS and SOURCES-TARGETS, the
;; following function call will generate six reports exporting the
;; subtrees for client A from files customers.org and
;; sales_projects.org for three periods: all changes since the last
;; report (DEFAULT), complete subtree export, all changes made to the
;; subtree in June 2015.  Finally it will create an email message
;; buffer with an email addressed to the persons to report to.

;; (org-report-create-report-message
;;  'clientA default-clients '(leads sales) default-sources-targets
;;  '(default nil '("2015-06-01" "2015-06-31")) 
;;  "Dear Mr. Kim,\n\nPlease find attached my weekly reports.\n\n
;; Best regards\nMichael Strey\n\n")
;;

(require 'org)

;;; Code:

(defgroup org-report nil
  "Options about automatic report generation."
  :group 'org)

(defcustom default-clients
  '((clientA "* Client A" "kim@clienta.com" "sales_@clienta.com")
    (clientB "* Client B" "franz@clientb.fr" nil))
  "Alist of clients consisting of KEY, regular expression to find a HEADING
for the subtree to export, TO: field for email message, CC: field of email message."
  :type '(editable-list)
  :group 'org-report)

(defcustom default-sources-targets
  '((leads "/home/strey/GTD/customers.org" "/tmp/" "leads")
    (sales "/home/strey/GTD/salesprojects.org" "/tmp/" "sales_projects"))
  "Alist defining sources and targets for the various reports
KEY, Orgmode SOURCE, target PATH, target FILE_NAME."
  :type '(editable-list)
  :group 'org-report)

(defcustom default-periods
  '(default nil)
  "Periods of time to create reports for.  NIL stands for a complete report.
DEFAULT takes the start date from property :EXPORT_DATE: and uses today's date
as end date.  If DEFAULT is used, it must be the first entry in the list."
  :type '(editable-list)
  :group 'org-report)

(defcustom default-subject
  "Report from %s"
  "Subject that shall be used in the email message header.
%s will be replaced by the date of today."
  :type '(string)
  :group 'org-report))

(defun org-report-iso-date-to-string (iso-date)
  "Convert a string with ISO-DATE of the form \"2015-06-10\" into a string
of the form \"150610\" for use in file names."
  (format-time-string "%y%m%d" (org-time-string-to-time iso-date)))

(defun org-report-create-target-file-name (report period)
  "Create a file name from the list REPORT of report parameters.
PERIOD is a list consisting of a pair of start date and end date
overriding the period entry in the REPORT list."
  (let ((path (cadddr report))
        (radical (nth 4 report))
        (heading (downcase (cadr (split-string (cadr report))))))
    (concat path "rep-"  radical "_" heading
            (when period
              (concat "-"
                      (org-report-iso-date-to-string (car period)) "_"
                      (org-report-iso-date-to-string (cadr period)))))))

(defun org-report-day-before (iso-date)
  "Return an orgmode time stamp of the day before the given ISO-DATE."
  (format-time-string "%Y-%m-%d %a"
                      (time-subtract (org-time-string-to-time iso-date)
                                     (seconds-to-time 86400))))

(defun org-report-day-after (iso-date)
  "Return an orgmode time stamp of the day before the given ISO-DATE."
  (format-time-string "%Y-%m-%d %a"
                      (time-add (org-time-string-to-time iso-date)
                                     (seconds-to-time 86400))))

(defun org-report-export-report (report)
  "Export one report.

REPORT is a list of parameters describing a report consisting of:
- PERIOD is a period of time defined by a list of start-date end-date
  or nil resp..
- SOURCE is the name of the orgmode source file.
- PATH is the output path for the target file.
- FILE_NAME is a string to construct the target file name with.
- SUBTREE is a string containing a regular expression to find
  the heading of the subtree to export."
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
        (setq period
              (cons (org-report-day-before last_date)
                    (cons (org-report-day-after today) nil))))
      (org-check-dates-range (car period) (cadr period)))
    (goto-char (point-min))
    (re-search-forward subtree)
    (setq target-file-name (org-report-create-target-file-name report period))
    (setq outfile (concat target-file-name ".tex"))
    (org-export-to-file 'latex outfile
      nil t period nil nil
      (lambda (file) (org-latex-compile file))))
  (concat target-file-name ".pdf")))
         
(defun org-report-make-reports-list (keylist sources-targets subtree periods)
  "Create a list of lists, where each list is describing one report (PDF file)
with the following keys:

- KEYLIST is a list of keys to chose entries of the alist SOURCES-TARGETS.
- SUBTREE is a string containing a regular expression to find
  the heading of the subtree to export.
- PERIODS is a list of periods of time defined by a list of start-date end-date
  or NIL respectively."
  (let ((sources (mapcar (lambda (key)
                           (assoc key sources-targets))
                         keylist)))  
    (-flatten-n 1
                (mapcar
                 (lambda (source-target)
                   (mapcar (lambda (period)
                             (cons period (cons subtree (cdr source-target))))
                           periods))
                 sources))))

(defun org-report-create-reports
    (client keylist &optional clients sources-targets periods)
  "Create a set of PDF reports exported according to the rules defined by the
function parameters.

CLIENT is a symbol to chose a set of client specific parameters from the alist CLIENTS.
KEYLIST is a list of keys to chose the reports to be exported from the alist
SOURCES-TARGETS.
PERIODS is a list of time periods to restrict the outputs to given periods of time.
NIL in this list causes a complete report without time constraints, 
DEFAULT creates a report starting one day before the last EXPORT_DATE and ending today. 
Take care that DEFAULT is the first element of the list, since EXPORT_DATE will
be set to today's date as side effect of function ORG-REPORT-EXPORT-REPORT."
  (let* ((clients (or clients default-clients))
         (sources-targets (or sources-targets default-sources-targets))
         (periods (or periods default-periods))
         (subject (or subject default-subject))
         (clientlist (assoc client clients))
         (subtree (cadr clientlist))
         (reports
          (org-report-make-reports-list keylist sources-targets subtree periods)))
    (dolist (report reports) (org-report-export-report report))))

(defun org-report-create-report-message
    (client keylist &optional clients sources-targets periods body subject)
  "Create an email message with an attached list of PDF reports exported according
to the rules defined by the function parameters.

CLIENT is a symbol to chose a set of client specific parameters from the alist CLIENTS.
KEYLIST is a list of keys to chose the reports to be exported from the alist
SOURCES-TARGETS.
PERIODS is a list of time periods to restrict the outputs to given periods of time.
NIL in this list causes a complete report without time constraints, 
DEFAULT creates a report starting one day before the last EXPORT_DATE and ending today. 
Take care that DEFAULT is the first element of the list, since EXPORT_DATE will
be set to today's date as side effect of function ORG-REPORT-EXPORT-REPORT.
BODY is a string containing the email body.
SUBJECT is a string that shall be used in the email message header.
%s used in this string will be replaced by the date of today."
  (let* ((today (current-time-string))
         (clients (or clients default-clients))
         (sources-targets (or sources-targets default-sources-targets))
         (periods (or periods default-periods))
         (subject (or subject default-subject))
         (clientlist (assoc client clients))
         (subtree (cadr clientlist))
         (to (caddr clientlist))
         (cc (cadddr clientlist))
         (reports
          (org-report-make-reports-list keylist sources-targets subtree periods)))
    (compose-mail
     to
     (format subject today)
     (when cc
       (cons (cons "Cc" cc) nil)))
    (message-goto-body)
    (when body
      (insert body))
    (dolist (report reports) (mml-attach-file (org-report-export-report report)))))

(provide 'org-report)

;;; org-report.el ends here
