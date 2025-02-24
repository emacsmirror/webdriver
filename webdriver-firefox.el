;;; webdriver-firefox.el --- Implement webdriver-service for firefox -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Mauro Aranda

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

;; This file implements a `webdriver-service-firefox' class, derived from
;; `webdriver-service'.  This allows users to manage a brower with the
;; firefox driver.

;;; Code:
(require 'webdriver)

(defclass webdriver-capabilities-firefox (webdriver-capabilities)
  ((capabilities
    :initform (list :alwaysMatch (list :browserName "firefox"))))
  "Represent a Webdriver Capabilities specification for Firefox.")

(defclass webdriver-firefox-log nil
  ((level :initform "info"
          :initarg :level
          :type string
          :documentation "Level of verbosity for logging information."))
  "Represent a log object for firefox capabilities.")

(cl-defmethod webdriver-object-to-plist ((self webdriver-firefox-log))
  "Return SELF as a property list."
  (list :level (oref self level)))

(defclass webdriver-firefox-preferences nil
  ((preferences :initform nil
                :initarg :preferences
                :type list
                :documentation "Property list with preferences and values."))
  "Represent a Firefox Preferences object.")

(cl-defmethod webdriver-firefox-preference-add
  ((self webdriver-firefox-preferences) preference value)
  "Add to SELF the preference PREFERENCE with value VALUE."
  (oset self preferences
        (plist-put (oref self preferences) preference value)))

(defclass webdriver-firefox-env nil
  ((env-vars :initform nil
             :initarg :env-vars
             :type list
             :documentation "Property list with environment variables."))
  "Represent a Firefox Env object.")

(cl-defmethod webdriver-firefox-env-add
  ((self webdriver-firefox-env) env-var value)
  "Add to SELF the environment variable ENV-VAR with value VALUE."
  (oset self env-vars
        (plist-put (oref self env-vars) env-var value)))

(cl-defmethod webdriver-capabilities-add :around
  ((self webdriver-capabilities-firefox)
   cap val &optional required)
  "Add capability CAP with value VAL to the capabilities in SELF.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability.

If CAP is not a moz:firefoxOptions capability, it falls back to the super's
method."
  (if (member cap '(:binary :args :profile :log :prefs :env))
      (let* ((caps (plist-get (oref self capabilities)
                              (if required :alwaysMatch :firstMatch)))
             (moz-opts (plist-get caps :moz:firefoxOptions)))
        (oset self capabilities
              (plist-put
               (oref self capabilities)
               (if required :alwaysMatch :firstMatch)
               (plist-put caps :moz:firefoxOptions
                          (plist-put moz-opts cap val)))))
    (cl-call-next-method self cap val required)))

(cl-defmethod webdriver-capabilities-add :around
  ((self webdriver-capabilities-firefox)
   cap (val webdriver-firefox-log) &optional required)
  "Add capability CAP with value VAL to the capabilities in SELF.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability.

If CAP is not a moz:firefoxOptions capability, it falls back to the super's
method."
  (unless (eq cap :log)
    (signal 'webdriver-error (list (format "Wrong capability %s" cap))))
  (webdriver-capabilities-add self cap (webdriver-object-to-plist val)
                              required))

(cl-defmethod webdriver-capabilities-add :around
  ((self webdriver-capabilities-firefox)
   cap (val webdriver-firefox-preferences) &optional required)
  "Add to SELF the capability CAP with value VAL.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability.

VAL should be a firefox preference, and CAP should be :prefs."
  (unless (eq cap :prefs)
    (signal 'webdriver-error (list (format "Wrong capability %s" cap))))
  (webdriver-capabilities-add self cap (oref val preferences) required))

(cl-defmethod webdriver-capabilities-add :around
  ((self webdriver-capabilities-firefox)
   cap (val webdriver-firefox-env) &optional required)
  "Add to SELF the capability CAP with value VAL.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability.

VAL should be a firefox env object, and CAP should be :env."
  (unless (eq cap :env)
    (signal 'webdriver-error (list (format "Wrong capability %s" cap))))
  (webdriver-capabilities-add self cap (oref val env-vars) required))

(cl-defmethod webdriver-capabilities-firefox-add-arg
  ((self webdriver-capabilities-firefox) arg &optional required)
  "Add the firefox argument ARG to the capabilities in SELF.

If REQUIRED is non-nil, adds it as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability."
  (let* ((caps (plist-get (oref self capabilities)
                          (if required :alwaysMatch :firstMatch)))
         (moz-opts (plist-get caps :moz:firefoxOptions)))
    (oset self capabilities
          (plist-put (oref self capabilities)
                     (if required :alwaysMatch :firstMatch)
                     (plist-put caps :moz:firefoxOptions
                                (plist-put moz-opts :args
                                           (vconcat
                                            (vector arg)
                                            (plist-get moz-opts :args))))))))

(cl-defmethod webdriver-capabilities-firefox-add-args
  ((self webdriver-capabilities-firefox) args &optional required)
  "Add firefox arguments in list ARGS to the capabilities in SELF.

If REQUIRED is non-nil, adds it as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability.

Unlike `webdriver-capabilities-firefox-add-arg', this function overwrites
all arguments already present in :args."
  (let* ((caps (plist-get (oref self capabilities)
                          (if required :alwaysMatch :firstMatch)))
         (moz-opts (plist-get caps :moz:firefoxOptions)))
    (oset self capabilities
          (plist-put (oref self capabilities)
                     (if required :alwaysMatch :firstMatch)
                     (plist-put caps :moz:firefoxOptions
                                (plist-put moz-opts :args
                                           (vconcat args)))))))

;; Firefox Service.
(defclass webdriver-service-firefox (webdriver-service)
  ((executable
    :initform "geckodriver"
    :documentation "Executable when running a Firefox Service.
This is usually \"geckodriver\", and it should be in `exec-path'.")
   (port
    :initform 4444
    :documentation "Port to pass as an option to geckodriver.
By default, it is 4444, which is the default for geckodriver.")
   (buffer
    :initform " *webdriver-geckodriver*"
    :initarg :buffer
    :type (or null string buffer)
    :documentation "Buffer to use for I/O with the process."))
  "The Firefox Service, that runs the geckodriver.")

(cl-defmethod webdriver-service-start :before
  ((self webdriver-service-firefox) &optional _retries &rest _process-args)
  "Add the port argument to the command line."
  (unless (member "--port" (oref self args))
    (oset self args (append (oref self args)
                            (list "--port" (number-to-string (oref self port)))))))

(cl-defmethod webdriver-service-get-port ((self webdriver-service-firefox))
  "Get port where SELF is listening on.

Looks up the port from the associated buffer to SELF.  It is an error to call
this function if SELF doesn't have an associated buffer."
  (with-current-buffer (oref self buffer)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "Listening on [^:]*:")
      (string-to-number (buffer-substring
                         (point)
                         (progn (end-of-line) (point)))))))

(provide 'webdriver-firefox)
;;; webdriver-firefox.el ends here
