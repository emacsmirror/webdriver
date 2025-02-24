;;; webdriver-chrome.el --- Implement webdriver-service for chrome  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Mauro Aranda

;; Author: Mauro Aranda <tbb@tbb-desktop>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This files implements a `webdriver-service-chrome' class, derived from
;; `webdriver-service'.  This allows users to manage a browser with the
;; chromedriver driver.

;;; Code:
(require 'webdriver)

;; Chrome Capabilities.
(defclass webdriver-capabilities-chrome (webdriver-capabilities)
  ((capabilities
    :initform (list :alwaysMatch (list :browserName "chrome"))))
  "Represent a Webdriver Capabilities specification for Chrome.")

;; Chrome Service.
(defclass webdriver-service-chrome (webdriver-service)
  ((executable
    :initform "chromedriver"
    :documentation "Executable when running a Chrome Service.
This is usually \"chromedriver\", and it should be in `exec-path'.")
   (port
    :initform 9515
    :initarg :port
    :documentation "Port to pass as an option to chromedriver.
By default, it is 9515, which is the default for chromedriver.")
   (buffer
    :initform " *webdriver-chromedriver*"
    :initarg :buffer
    :type (or null string buffer)
    :documentation "Buffer to use for I/O with the process."))
  "The Chrome Service that runs the chromedriver.")

(cl-defmethod webdriver-service-start :before
  ((self webdriver-service-chrome) &optional _retries &rest _process-args)
  "Add the port argument to the command line."
  (unless (seq-some (lambda (arg)
                      (string-match-p "--port=[0-9]*" arg))
                    (oref self args))
    (oset self args (append (oref self args)
                            (list (concat "--port="
                                          (number-to-string (oref self port))))))))

(cl-defmethod webdriver-service-get-port ((self webdriver-service-chrome))
  "Get port where SELF is listening on.

Looks up the port from the associated buffer to SELF.  It is an error to call
this function if SELF doesn't have an associated buffer."
  (with-current-buffer (oref self buffer)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "on port ")
      (string-to-number (buffer-substring
                         (point)
                         (progn (end-of-line) (point)))))))


(provide 'webdriver-chrome)
;;; webdriver-chrome.el ends here
