;;; webdriver.el --- WebDriver local end implementation -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Mauro Aranda

;; Author: Mauro Aranda <maurooaranda@gmail.com>
;; Maintainer: Mauro Aranda <maurooaranda@gmail.com>
;; Created: Wed Nov 23 10:24:00 2022
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools
;; URL: https://gitlab.com/mauroaranda/emacs-webdriver

;; This file is NOT part of GNU Emacs.

;; webdriver is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; webdriver is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with webdriver.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements a W3C WebDriver compatible client,
;; known as a "local end", in ELisp.

;; See the specification in https://www.w3.org/TR/webdriver/

;; This means you can remotely control any web browser that implements the
;; server side of the protocol linked above.

;; To start controlling a remote browser, you can:
;; - Create an instance of `webdriver-session' and run
;; `webdriver-session-start'.  This will use the value stored in
;; `webdriver-default-service' to start running a compatible service.
;; - Alternatively, you can create a custom instance of `webdriver-service'
;; and pass it as a :service argument when creating an instance of
;; `webdriver-session'.

;; See the list of endpoints supported with the corresponding ELisp function
;; to see the things you can do.

;; List of Endpoints supported:
;; Command Name -> ELisp function
;; New Session -> `webdriver-session-start'
;; Delete Session -> `webdriver-session-stop'
;; Status -> `webdriver-service-status'
;; Get Timeouts -> `webdriver-get-timeouts'
;; Set Timeouts -> `webdriver-set-timeouts'
;; Navigate To -> `webdriver-goto-url'
;; Get Current URL -> `webdriver-get-current-url'
;; Back -> `webdriver-go-back'
;; Forward -> `webdriver-go-forward'
;; Refresh -> `webdriver-refresh'
;; Get Title -> `webdriver-get-title'
;; Get Window Handle -> `webdriver-get-window-handle'
;; Close Window -> `webdriver-close-window'
;; Switch to Window -> `webdriver-switch-to-window'
;; Get Window Handles -> `webdriver-get-window-handles'
;; New Window -> `webdriver-create-new-window'
;; Switch To Frame -> `webdriver-switch-to-frame'
;; Switch To Parent Frame -> `webdriver-switch-to-parent-frame'
;; Get Window Rect -> `webdriver-get-window-rect'
;; Set Window Rect -> `webdriver-set-window-rect'
;; Maximize Window -> `webdriver-maximize-window'
;; Minimize Window -> `webdriver-minimize-window'
;; Fullscreen Window -> `webdriver-fullscreen-window'
;; Get Active Element -> `webdriver-get-active-element'
;; Get Element Shadow Root -> `webdriver-get-element-shadow-root'
;; Find Element -> `webdriver-find-element'
;; Find Elements -> `webdriver-find-elements'
;; Find Element From Element -> `webdriver-find-element-from-element'
;; Find Elements From Element -> `webdriver-find-elements-from-element'
;; Find Element From Shadow Root -> `webdriver-find-element-from-shadow-root'
;; Find Elements From Shadow Root -> `webdriver-find-elements-from-shadow-root'
;; Is Element Selected -> `webdriver-element-selected-p'
;; Get Element Attribute -> `webdriver-get-element-attribute'
;; Get Element Property -> `webdriver-get-element-property'
;; Get Element CSS Value -> `webdriver-get-element-css-value'
;; Get Element Text -> `webdriver-get-element-text'
;; Get Element Tag Name -> `webdriver-get-element-tag-name'
;; Get Element Rect -> `webdriver-get-element-rect'
;; Is Element Enabled -> `webdriver-element-enabled-p'
;; Get Computed Role -> `webdriver-element-get-computed-role'
;; Get Computed Label -> `webdriver-element-get-computed-label'
;; Element Click -> `webdriver-element-click'
;; Element Clear -> `webdriver-element-clear'
;; Element Send Keys -> `webdriver-element-send-keys'
;; Get Page Source -> `webdriver-get-page-source'
;; Execute Script -> `webdriver-execute-synchronous-script'
;; Execute Async Script -> `webdriver-execute-asynchronous-script'
;; Get All Cookies -> `webdriver-get-all-cookies'
;; Get Named Cookie -> `webdriver-get-cookie'
;; Add Cookie -> `webdriver-add-cookie'
;; Delete Cookie -> `webdriver-delete-cookie'
;; Delete All Cookies -> `webdriver-delete-all-cookies'
;; Perform Actions -> `webdriver-perform-actions'
;; Release Actions -> `webdriver-release-actions'
;; Dismiss Alert -> `webdriver-dismiss-alert'
;; Accept Alert -> `webdriver-accept-alert'
;; Get Alert Text -> `webdriver-get-alert-text'
;; Send Alert Text -> `webdriver-send-alert-text'
;; Take Screenshot -> `webdriver-take-screenshot'
;; Take Element Screenshot -> `webdriver-take-element-screenshot'
;; Print Page -> `webdriver-print-page'

;; Here is a usage example:
;; (let ((session (make-instance 'webdriver-session)))
;;   (webdriver-session-start session)
;;   (webdriver-goto-url session "https://www.example.org")
;;   (let ((element
;;          (webdriver-find-element session (make-instance 'webdriver-by
;;                                                         :strategy "tag name"
;;                                                         :selector "h1"))))
;;     (message (webdriver-get-element-text session element)))
;;   (webdriver-session-stop session))

;; If you want to request the remote end for certain capabilities, you can do
;; that using a `webdriver-capabilities' object, and adding options to it.
;; For example, for a headless Firefox session, you can use:
;; (let ((caps (make-instance 'webdriver-capabilities-firefox)))
;;   (webdriver-capabilities-firefox-add-arg caps "-headless" t)
;;   (make-instance 'webdriver-session :requested-capabilities caps))
;; This adds the "-headless" argument as part of :args in the
;; :moz:firefoxOptions.  The third argument t says to add this requested
;; capability as an alwaysMatch capability.

;;; Code:
(require 'eieio)
(require 'eieio-base)
(require 'cl-lib)
(require 'json)
(defvar url-request-method)
(defvar url-request-data)

;; Variables and Options.
(defgroup webdriver nil "WebDriver options."
  :group 'tools
  :version "0.1")

(defcustom webdriver-default-service 'webdriver-service-firefox
  "The default service to use when creating a session.

When creating a `webdriver-session' object without a specified service,
a service of this class will be instantiated when executing
`webdriver-session-start'.

Its value should be a symbol, a class name for a `webdriver-service'."
  :type `(symbol :match ,(lambda (_wid val)
                           (child-of-class-p val 'webdriver-service))
                 :validate ,(lambda (w)
                              (unless (widget-apply w :match (widget-value w))
                                (widget-put w :error
                                            (format
                                             "%S is not a webdriver-service"
                                             (widget-value w)))
                                w)))
  :package-version "0.1")

(defvar webdriver-errors-hash-table
  (make-hash-table :test #'equal :size 30)
  "Hash table to map a Webdriver \"error code\" to an ELisp error.
Error codes are strings, defined in the specification.")

;; Utils.
(defun webdriver--get-free-port ()
  "Return a free port on localhost, as an integer."
  (let (server ret)
    (unwind-protect
        (progn
          (setq server (make-network-process :name "dummy"
                                             :server t
                                             :host 'local
                                             ;; Get a random port available.
                                             :service t))
          (setq ret (cadr (process-contact server))))
      (delete-process server))
    ret))

;; Errors.
(define-error 'webdriver-error "Webdriver error")

(defun webdriver-define-error (error-code)
  "Define an ELisp error with its name taken from the ERROR-CODE string.

Adds it to `webdriver-errors-hash-table'."
  ;; webdriver-name is just to keep package-lint happy.
  ;; It's really imenu's fault.
  (let ((webdriver-name
         (intern (concat "webdriver-"
                         (mapconcat #'identity (split-string error-code " ")
                                    "-")))))
    (define-error webdriver-name (format "Webdriver error: %s" error-code)
                  'webdriver-error)
    (puthash error-code webdriver-name webdriver-errors-hash-table)))

(dolist (err
         '("element click intercepted"
           "element not interactable"
           "insecure certificate"
           "invalid argument"
           "invalid cookie domain"
           "invalid element state"
           "invalid selector"
           "invalid session id"
           "javascript error"
           "move target out of bounds"
           "no such alert"
           "no such cookie"
           "no such element"
           "no such frame"
           "no such window"
           "no such shadow root"
           "script timeout error"
           "session not created"
           "stale element reference"
           "detached shadow root"
           "timeout"
           "unable to set cookie"
           "unable to capture screen"
           "unexpected alert open"
           "unknown command"
           "unknown error"
           "unknown method"
           "unsupported operation"))
  (webdriver-define-error err))

(defun webdriver-check-for-error (value)
  "Check if VALUE, the JSON response from the webdriver, is an error value.

If it is, then signal an error.  If it is not, return VALUE."
  (let ((val (alist-get 'value value)))
    (when (and (listp val) (alist-get 'error val))
      (signal (gethash (alist-get 'error val) webdriver-errors-hash-table
                       'webdriver-error)
              (list (alist-get 'message val)
                    (alist-get 'stacktrace val)
                    (when (alist-get 'data val)
                      (alist-get 'text (alist-get 'data val))))))
    value))

;; WebDriver Capabilities.
(defclass webdriver-capabilities nil
  ((capabilities
    :initarg :capabilities
    :initform nil
    :type list
    :documentation "Hold a plist with capabilities."))
  "Represent a Webdriver Capabilities specification.")

(cl-defmethod webdriver-capabilities-add ((self webdriver-capabilities)
                                          cap val &optional required)
  "Add capability CAP with value VAL to the capabilities in SELF.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability."
  (let ((caps (plist-get (oref self capabilities)
                         (if required :alwaysMatch :firstMatch))))
    (oset self capabilities
          (plist-put (oref self capabilities) (if required
                                                  :alwaysMatch
                                                :firstMatch)
                     (plist-put caps cap val)))))

(cl-defmethod webdriver-capabilities-get ((self webdriver-capabilities))
  "Get the capabilities in SELF, as a plist."
  (oref self capabilities))

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

;; WebDriver proxy configuration.
(defclass webdriver-proxy-configuration nil
  ((proxyType :initform ""
              :initarg :proxy-type
              :type string
              :documentation "Type of proxy configuration.")
   (proxyAutoconfigUrl :initform ""
                       :initarg :proxy-autoconfig-url
                       :type string
                       :documentation "URL for a proxy auto-config file.
It's meaningful when proxyType is \"pac\".")
   (ftpProxy :initform ""
             :initarg :ftp-proxy
             :type string
             :documentation "Proxy host for FTP traffic,
when proxyType is \"manual\".")
   (httpProxy :initform ""
              :initarg :http-proxy
              :type string
              :documentation "Proxy host for HTTP traffic,
when proxyType is \"manual\".")
   (noProxy :initform []
            :initarg :no-proxy
            :type vector
            :documentation "Vector with address to bypass,
when proxyType \"manual\".")
   (sslProxy :initform ""
             :initarg :ssl-proxy
             :type string
             :documentation "Proxy host for encrypted TLS traffic,
when proxyType is \"manual\".")
   (socksProxy :initform ""
               :initarg :socks-proxy
               :type string
               :documentation "Proxy host for a SOCKS proxy,
when proxyType is \"manual\".")
   (socksVersion :initform 0
                 :initarg :socks-version
                 :type number
                 :documentation "The SOCKS proxy version,
when proxyType is \"manual\"."))
  "Representation of a WebDriver proxy configuration.")

(cl-defmethod webdriver-object-to-plist ((self webdriver-proxy-configuration))
  "Represent SELF as a property list.

The property list is as: (PROP VAL), where PROP is each property of SELF
and VAL is the value of that property."
  ;; FIXME: Starting to see a pattern here.
  (let ((keys '(:proxyType :proxyAutoconfigUrl :ftpProxy :httpProxy
                           :noProxy :sslProxy :socksProxy :socksVersion))
        (props '(proxyType proxyAutoconfigUrl ftpProxy httpProxy noProxy
                           sslProxy socksProxy socksVersion)))
    (cl-mapcan (lambda (key prop)
                 (let ((val (slot-value self prop)))
                   (cond ((and (stringp val) (not (string= val "")))
                          (list key val))
                         ((and (vectorp val) (/= (length val) 0))
                          (list key val))
                         (t nil))))
               keys props)))

(cl-defmethod webdriver-json-serialize ((self webdriver-proxy-configuration))
  "JSON-serialize SELF.

Calls `json-serialize' with SELF represented as a property list."
  (json-serialize (webdriver-object-to-plist self)))

(cl-defmethod webdriver-capabilities-add ((self webdriver-capabilities) cap
                                          (val webdriver-proxy-configuration)
                                          &optional required)
  "Add capability CAP with value VAL to the capabilities in SELF.

VAL should be a proxy configuration object, and CAP should be :proxy.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability."
  (unless (eq cap :proxy)
    (signal 'webdriver-error
            (list (format "Bad CAP %s for webdriver-proxy-configuration"
                          cap))))
  (let ((caps (plist-get (oref self capabilities)
                         (if required :alwaysMatch :firstMatch))))
    (oset self capabilities
          (plist-put (oref self capabilities) (if required
                                                  :alwaysMatch
                                                :firstMatch)
                     (plist-put caps cap
                                (webdriver-object-to-plist val))))))

;; WebDriver Service.
(defclass webdriver-service nil
  ((executable
    :initarg :executable
    :type string
    :documentation "The executable to run for this service.")
   (port
    :initarg :port
    :initform nil
    :type (or null integer)
    :documentation
    "Port to connect to, usually passed as a command-line argument.")
   (log
    :initarg :log
    :initform nil
    :type (or null string buffer)
    :documentation "Buffer to use when logging information.")
   (buffer
    :initarg :buffer
    :initform nil
    :type (or null string buffer)
    :documentation "Buffer to use for I/O with the process.")
   (process
    :initform nil
    :type (or null process)
    :documentation "Process for this service.")
   (args
    :initarg :args
    :initform nil
    :type list
    :documentation "Arguments to pass to executable."))
  "A WebDriver service abstraction, for running the remote ends.")

(cl-defmethod webdriver-service-start ((self webdriver-service)
                                       &optional retries
                                       &rest process-args)
  "Start a new service described by SELF.

This starts a new process which runs `executable' and connects it via a pipe.

To start the new process, it calls `make-process', with `executable' as the
program to run and `args' as the arguments to pass.  It stores the new process
in `process'.

If `log' is non-nil, use that buffer to log information.

If `buffer' is non-nil, associate the process with that buffer.

Optional argument RETRIES may be a number to specify the number of attempts
to connect to the server before giving up.  It may also be a list of the
form (RETRIES WAIT-TIME), where RETRIES specifies the number of attempts to
connect to the server before giving up, and WAIT-TIME the time to wait before
retrying.  By default, WAIT-TIME is 0.1 and RETRIES is 10.

If given, PROCESS-ARGS are keyword-value pairs to use when starting the
process.  See `make-process'."
  (let ((exec (oref self executable))
        (wait (if (and retries (listp retries))
                  (cadr retries)
                0.1))
        (retries (if retries
                     (if (listp retries)
                         (car retries)
                       retries)
                   10))
        (i 0))
    (unless (executable-find exec)
      (signal 'webdriver-error (list (format "%s cannot be found" exec))))
    (oset self process (apply #'make-process
                               :name exec
                               :command (append (list exec)
                                                (oref self args))
                               :connection-type 'pipe
                               :buffer (oref self buffer)
                               process-args))
    (accept-process-output (oref self process) 1.0 nil t)
    (when (oref self buffer)
      ;; Get the port from the response.
      (oset self port (webdriver-service-get-port self)))
    (while (and (process-live-p (oref self process))
                (< i retries)
                (not (webdriver-service-connectable-p self)))
      (sleep-for wait)
      (setq i (1+ i)))
    (cond
     ((not (process-live-p (oref self process)))
      (signal 'webdriver-error (list "Service unexpectedly closed")))
     ((= i retries)
      (signal 'webdriver-error (list "Couldn't connect to the service")))
     (t
      (when (oref self log)
        (with-current-buffer (get-buffer-create (oref self log))
          (insert "Webdriver Info: Started executable correctly\n\n")))))))

(cl-defmethod webdriver-service-stop ((self webdriver-service))
  "Stop the service described by SELF.

Stops the process stored in `process', and sets it to nil."
  (when (and (oref self log) (buffer-live-p (oref self log)))
    (kill-buffer (oref self log)))
  (when (and (processp (oref self process)))
    (delete-process (oref self process)))
  (oset self process nil)
  (when (and (oref self buffer) (buffer-live-p (oref self buffer)))
    (kill-buffer (oref self buffer))))

(cl-defmethod webdriver-service-connectable-p ((self webdriver-service))
  "Non-nil if connection can be established to the server associated to SELF."
  (and (process-live-p (oref self process))
       (condition-case _
           (let (conn)
             (unwind-protect
                 (setq conn (make-network-process :name "test-connection"
                                                  :host 'local
                                                  :service (oref self port)))
               (delete-process conn)))
         (error nil))))

(cl-defmethod webdriver-service-url ((self webdriver-service))
  "Return the URL where the process associated to SELF is listening."
  (let ((port (or (oref self port)
                  (oset self port (webdriver--get-free-port)))))
    (format "http://localhost:%d" port)))

(cl-defgeneric webdriver-service-get-port (service)
  "Get port where SERVICE is listening on.")

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

;; WebDriver Session.
(defclass webdriver-session nil
  ((service
    :initform nil
    :initarg :service
    :type (or boolean webdriver-service)
    :documentation
    "An instance of `webdriver-service' that the session should connect to.")
   (cleanup-service
    :initform nil
    :type boolean
    :documentation "Whether the session is responsible for cleaning up the service.
This is non-nil when :service wasn't specified, so creating a `webdriver-session'
object started a default service.")
   (id :initform nil
       :type (or boolean string)
       :documentation "String ID for this session.")
   (requested-capabilities
    :initform nil
    :initarg :requested-capabilities
    :type (or list webdriver-capabilities)
    :documentation
    "The requested capabilities passed when creating the session.")
   (capabilities
    :initform nil
    :type (or list webdriver-capabilities)
    :documentation "The actual capabilities that the session supports."))
  "Represent a WebDriver session to control a browser.")

(cl-defmethod initialize-instance :after ((self webdriver-session) &rest _args)
  "If there is no service associated with SELF, create a default one."
  (unless (oref self service)
    (let ((service (make-instance webdriver-default-service)))
      (webdriver-service-start service)
      (oset self service service)
      (oset self cleanup-service t))))

(cl-defmethod webdriver-service-status ((self webdriver-session))
  "Return information about whether the service can create a new session SELF."
  (webdriver-execute-command self "status" "GET"))

(cl-defmethod webdriver-session-start ((self webdriver-session))
  "Start a new session associated to SELF."
  (when (oref self id)
    (signal 'webdriver-error (list "Session already started")))
  (let ((value (webdriver-execute-command
                self "session" "POST"
                ;; Don't use `plistp', to support Emacs < 29.
                `(:capabilities ,(if (listp (oref self requested-capabilities))
                                     (oref self requested-capabilities)
                                   (webdriver-capabilities-get
                                    (oref self requested-capabilities)))))))
    (setq value (alist-get 'value value))
    (oset self id (alist-get 'sessionId value))
    (oset self capabilities (alist-get 'capabilities value))))

(cl-defmethod webdriver-session-stop ((self webdriver-session))
  "Stop the session associated to SELF."
  (unless (oref self id)
    (signal 'webdriver-error (list "Session doesn't have a session ID")))
  (webdriver-execute-command self (format "session/%s" (oref self id)) "DELETE")
  (when (oref self cleanup-service)
    (webdriver-service-stop (oref self service)))
  (oset self id nil))

(cl-defmethod webdriver-service-ready-p ((self webdriver-session))
  "Non-nil if the service connected to SELF is ready."
  (alist-get 'ready (alist-get 'value (webdriver-service-status self))))

;; Webdriver Commands.
(defclass webdriver-command nil
  ((name :initform ""
         :initarg :name
         :type string
         :documentation "Name of the command, an URL.")
   (method :initform "GET"
           :initarg :method
           :type string
           :documentation "Method to use when requesting name.")
   (body :initform nil
         :initarg :body
         :type (or list string)
         :documentation "Data to pass to the command, JSON-serialized."))
  "Abstraction of a WebDriver command.")

(defun webdriver-execute-command (session name method &optional body)
  "Execute command NAME with body BODY in session SESSION.

SESSION should be an instance of `webdriver-session'.
NAME should be the name of the command.  METHOD should be the method to use.

BODY should be in a form acceptable for the body slot of a command.

Returns the value returned by the driver, unless there are errors."
  (let* ((cmd (make-instance 'webdriver-command
                             :name name
                             :method method
                             :body body))
         (value (webdriver-send-command session cmd)))
    (condition-case err
        (webdriver-check-for-error value)
      (webdriver-error
       (when-let ((buf (oref (oref session service) log))
                  (data (cdr err)))
         (with-current-buffer buf
           (insert (format "%S:\nStacktrace" (car err) )
                   (nth 1 data) ":\n"
                   (if (nth 2 data)
                       (concat "Data: " (nth 2 data))
                     ""))))
       (signal (car err) (list (cadr err)))))))

(cl-defmethod webdriver-send-command ((self webdriver-session)
                                      (command webdriver-command))
  "Send the command COMMAND in session SELF.

Serializes the body of COMMAND only if it is not already a string."
  (let* ((url-request-method (oref command method))
         (url-request-data (encode-coding-string
			    (if (stringp (oref command body))
				(oref command body)
                              (json-serialize (oref command body)))
			    'utf-8))
         (buffer (url-retrieve-synchronously
                  (concat (webdriver-service-url (oref self service))
                          "/" (oref command name))))
         (text (with-current-buffer buffer
                 (goto-char (point-min))
                 (re-search-forward "\n\n")
                 (decode-coding-string (buffer-substring (point) (point-max))
                                       'utf-8)))
         (value (with-temp-buffer
                  (insert text)
                  (goto-char (point-min))
                  (json-read))))
    (prog1 value
      (kill-buffer buffer))))

;; Navigation.
(cl-defmethod webdriver-goto-url ((self webdriver-session) url)
  "Navigate to the url URL in session SELF."
  (webdriver-execute-command self (format "session/%s/url" (oref self id))
                             "POST" `(:url ,url)))

(cl-defmethod webdriver-get-current-url ((self webdriver-session))
  "Return the current url of the session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/url" (oref self id)) "GET")))

(cl-defmethod webdriver-go-back ((self webdriver-session))
  "Go back to the previous url of the session SELF."
  (webdriver-execute-command self (format "session/%s/back" (oref self id))
                             "POST"))

(cl-defmethod webdriver-go-forward ((self webdriver-session))
  "Go forward to the next url of the session SELF."
  (webdriver-execute-command self (format "session/%s/forward" (oref self id))
                             "POST"))

(cl-defmethod webdriver-refresh ((self webdriver-session))
  "Refresh the current url of the session SELF."
  (webdriver-execute-command self (format "session/%s/refresh" (oref self id))
                             "POST"))

(cl-defmethod webdriver-get-title ((self webdriver-session))
  "Get the title of the current page visited by SELF."
  (alist-get 'value (webdriver-execute-command self (format "session/%s/title"
                                                            (oref self id))
                                               "GET")))

;; WebDriver Timeouts.
(defclass webdriver-timeouts nil
  ((script :initform 30 :initarg :script :type (or integer null)
           :writer webdriver-timeout-set-script
           :reader webdriver-timeout-get-script
           :documentation "Timeout limit for script evaluation.")
   (pageLoad :initform 300 :initarg :page-load :type (or integer null)
             :writer webdriver-timeout-set-pageload
             :reader webdriver-timeout-get-pageload
             :documentation "Timeout limit for an explicit navigation attempt.")
   (implicit :initform 0 :initarg :implicit :type (or integer null)
             :writer webdriver-timeout-set-implicit
             :reader webdriver-timeout-get-implicit
             :documentation "Timeout limit for locating an element."))
  "Representation of a WebDriver timeouts configuration.

Each property is stored as a number of seconds.")

(cl-defmethod webdriver-object-to-plist ((self webdriver-timeouts))
  "Represent SELF as a property list.

The property list is as: (PROP VAL), where PROP is each property of SELF
and VAL is the value of that property."
  (let ((timeouts '(:script :pageLoad :implicit))
        (props '(script pageLoad implicit)))
    (cl-mapcan (lambda (timeout prop)
                 (when (slot-value self prop)
                   (list timeout (* (slot-value self prop) 1000))))
               timeouts props)))

(cl-defmethod webdriver-json-serialize ((self webdriver-timeouts))
  "JSON-serialize SELF.

Calls `json-serialize' with SELF represented as a property list."
  (json-serialize (webdriver-object-to-plist self)))

(cl-defmethod webdriver-capabilities-add ((self webdriver-capabilities) cap
                                          (val webdriver-timeouts)
                                          &optional required)
  "Add capability CAP with value VAL to the capabilities in SELF.

VAL should be a timeouts object, and CAP should be :timeouts.

If REQUIRED is non-nil, adds CAP as an \"alwaysMatch\" capability.  Else, adds
it as a \"firstMatch\" capability."
  (unless (eq cap :timeouts)
    (signal 'webdriver-error (list (format "Bad CAP %s for webdriver-timeouts"
                                           cap))))
  (let ((caps (plist-get (oref self capabilities)
                         (if required :alwaysMatch :firstMatch))))
    (oset self capabilities
          (plist-put (oref self capabilities) (if required
                                                  :alwaysMatch
                                                :firstMatch)
                     (plist-put caps cap
                                (webdriver-object-to-plist val))))))

(cl-defmethod webdriver-get-timeouts ((self webdriver-session))
  "Get the timeout specification for the session SELF.

Returns a `webdriver-timeouts' object with the current timeout specification."
  (let ((value (alist-get 'value (webdriver-execute-command
                                  self (format "session/%s/timeouts"
                                               (oref self id))
                                  "GET"))))
    (make-instance 'webdriver-timeouts
                   :script (/ (alist-get 'script value) 1000)
                   :page-load (/ (alist-get 'pageLoad value) 1000)
                   :implicit (/ (alist-get 'implicit value) 1000))))

(cl-defmethod webdriver-get-timeout ((self webdriver-session) timeout)
  "Get the timeout in seconds for timeout TIMEOUT in session SELF.

TIMEOUT should be a symbol, one of script, pageLoad or implicit."
  (let ((timeouts (webdriver-get-timeouts self)))
    (if timeout
        (if (member timeout '(script pageLoad implicit))
            (slot-value timeouts timeout)
          (signal 'webdriver-error (list
                                    (format "invalid timeout, %S" timeout))))
      timeouts)))

(cl-defmethod webdriver-set-timeouts ((self webdriver-session)
                                      (timeouts webdriver-timeouts))
  "Set the timeouts specification in TIMEOUTS for the session SELF."
  (webdriver-execute-command self (format "session/%s/timeouts" (oref self id))
                             "POST" (webdriver-json-serialize timeouts)))

(cl-defmethod webdriver-set-timeout ((self webdriver-session) timeout secs)
  "Set timeout TIMEOUT to value SECS times 1000 in session SELF.

TIMEOUT should be a symbol, one of script, pageLoad or implicit."
  (if (member timeout '(script pageLoad implicit))
      (webdriver-execute-command self (format "session/%s/timeouts"
                                              (oref self id))
                                 "POST" (list (intern (format ":%s" timeout))
                                              (* secs 1000)))
    (signal 'webdriver-error (list (format "invalid timeout %S" timeout)))))

;; A WebDriver rect.
(defclass webdriver-rect nil
  ((width :initarg :width
          :initform nil
          :type (or null number)
          :documentation "Width for the rect, or null for a default value.")
   (height :initarg :height
           :initform nil
           :type (or null number)
           :documentation "Height for the rect, or null for a default value.")
   (x :initarg :x
      :initform nil
      :type (or null number)
      :documentation "X position for the rect, or null for a default value.")
   (y :initarg :y
      :initform nil
      :type (or null number)
      :documentation "Y position the rect, or null for a default value."))
  "A rect class for use in window and element commands.")

(cl-defmethod webdriver-json-serialize ((self webdriver-rect))
  "JSON-Serialize SELF."
  (json-serialize (list :width (oref self width)
                        :height (oref self height)
                        :x (oref self x)
                        :y (oref self y))))

;; Window handles.
(cl-defmethod webdriver-get-window-handle ((self webdriver-session))
  "Get the current window handle associated to the session SELF."
  (alist-get 'value
             (webdriver-execute-command self (format "session/%s/window"
                                                     (oref self id))
                                        "GET")))

(cl-defmethod webdriver-close-window ((self webdriver-session))
  "Close the current window handle associated to the session SELF.

If there are no more open top-level windows, stop SELF."
  (let ((value
         (alist-get 'value (webdriver-execute-command
                            self
                            (format "session/%s/window" (oref self id))
                            "DELETE"))))
    ;; Close the session when there are no more windows.
    (when (seq-empty-p value)
      (webdriver-session-stop self))
    value))

(cl-defmethod webdriver-switch-to-window ((self webdriver-session) handle)
  "Switch to the window with handle HANDLE in session SELF."
  (webdriver-execute-command self (format "session/%s/window" (oref self id))
                             "POST" (list :handle handle)))

(cl-defmethod webdriver-get-window-handles ((self webdriver-session))
  "Get all window handles associated to the session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/window/handles" (oref self id))
                     "GET")))

(cl-defmethod webdriver-create-new-window ((self webdriver-session) type)
  "Create a new window handle of type TYPE for the session SELF.

TYPE defaults to \"tab\", and can be one of \"tab\" or \"window\"."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/window/new" (oref self id))
                     "POST" (json-serialize `(:type ,(or type "tab"))))))

(cl-defmethod webdriver-get-window-rect ((self webdriver-session))
  "Get the window rectangle for the current window handle of the session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self
                           (format "session/%s/window/rect"
                                   (oref self id))
                           "GET"))))
    (make-instance 'webdriver-rect
                   :width (alist-get 'width value)
                   :height (alist-get 'height value)
                   :x (alist-get 'x value)
                   :y (alist-get 'y value))))

(cl-defmethod webdriver-set-window-rect ((self webdriver-session)
                                         (rect webdriver-rect))
  "Set rectangle to RECT for the current window handle of session SELF."
  (webdriver-execute-command self
                             (format "session/%s/window/rect" (oref self id))
                             "POST" (webdriver-json-serialize rect)))

(cl-defmethod webdriver-maximize-window ((self webdriver-session))
  "Maximize the current window associated to the session SELF."
  (webdriver-execute-command self (format "session/%s/window/maximize"
                                          (oref self id))
                             "POST"))

(cl-defmethod webdriver-minimize-window ((self webdriver-session))
  "Minimize the current window associated to the session SELF."
  (webdriver-execute-command self (format "session/%s/window/minimize"
                                          (oref self id))
                             "POST"))

(cl-defmethod webdriver-fullscreen-window ((self webdriver-session))
  "Fullscreen the current window associated to the session SELF."
  (webdriver-execute-command self (format "session/%s/window/fullscreen"
                                          (oref self id))
                             "POST"))

(cl-defmethod webdriver-switch-to-frame ((self webdriver-session) id)
  "Switch to the frame ID in the session SELF."
  (webdriver-execute-command self (format "session/%s/frame" (oref self id))
                             "POST" (json-serialize (list :id id))))

(cl-defmethod webdriver-switch-to-parent-frame ((self webdriver-session))
  "Switch to the parent frame in the session SELF."
  (webdriver-execute-command self (format "session/%s/frame/parent"
                                          (oref self id))
                             "POST"))

;; WebDriver By strategies.
(defclass webdriver-by nil
  ((strategy :initform ""
             :initarg :strategy
             :type string
             :documentation "Strategy to use for finding elements.")
   (selector :initform ""
             :initarg :selector
             :type string
             :documentation "The value that is going to be looked for.")))

(cl-defmethod webdriver-object-to-plist ((self webdriver-by))
  "Represent SELF, a `webdriver-by' object, as a property list."
  (list :using (oref self strategy)
        :value (oref self selector)))

(cl-defmethod webdriver-json-serialize ((self webdriver-by))
  "JSON-Serialize SELF, a `webdriver-by' object."
  (json-serialize (webdriver-object-to-plist self)))

;; WebDriver Element.
(defclass webdriver-element nil
  ((id :initform ""
       :initarg :reference
       :type string
       :documentation "The ID, a string, that identifies the element."))
  "Abstraction for WebDriver Element.")

(cl-defmethod webdriver-find-element ((self webdriver-session)
                                      (by webdriver-by))
  "Find an element in the current page visited by SELF with strategy BY."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element" (oref self id))
                           "POST" (webdriver-json-serialize by)))))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements ((self webdriver-session)
                                       (by webdriver-by))
  "Find all elements in the current page visited by SELF with strategy BY."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/elements" (oref self id))
                           "POST" (webdriver-json-serialize by)))))
    (mapcar (lambda (el)
              (make-instance 'webdriver-element :reference (cdar el)))
            value)))

(cl-defmethod webdriver-find-element-from-element ((self webdriver-session)
                                                   (element webdriver-element)
                                                   (by webdriver-by))
  "Starting from element ELEMENT, find an element by BY in the session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/element"
                                        (oref self id)
                                        (oref element id))
                           "POST" (webdriver-json-serialize by)))))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements-from-element ((self webdriver-session)
                                                    (element webdriver-element)
                                                    (by webdriver-by))
  "Starting from element ELEMENT, find all elements by BY in the session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/elements"
                                        (oref self id)
                                        (oref element id))
                           "POST" (webdriver-json-serialize by)))))
    (mapcar (lambda (el)
              (make-instance 'webdriver-element :reference (cdar el)))
            value)))

;; WebDriver Shadow Root.
(defclass webdriver-shadow nil
  ((id :initform ""
       :initarg :reference
       :type string
       :documentation "The ID of the shadow root, as a string."))
  "WebDriver Shadow Root.")

(cl-defmethod webdriver-find-element-from-shadow-root ((self webdriver-session)
                                                       (shadow webdriver-shadow)
                                                       (by webdriver-by))
  "Starting from shadow root SHADOW, find an element by BY in session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/shadow/%s/element"
                                        (oref self id)
                                        (oref shadow id))
                           "POST" (webdriver-json-serialize by)))))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements-from-shadow-root
  ((self webdriver-session)
   (shadow webdriver-shadow)
   (by webdriver-by))
  "Start from shadow root SHADOW and find all elements by BY in session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/shadow/%s/elements"
                                        (oref self id)
                                        (oref shadow id))
                           "POST" (webdriver-json-serialize by)))))
    (mapcar (lambda (el)
              (make-instance 'webdriver-element :reference (cdar el)))
            value)))

(cl-defmethod webdriver-get-active-element ((self webdriver-session))
  "Get the active element in session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/active"
                                        (oref self id))
                           "GET"))))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-get-element-shadow-root ((self webdriver-session)
                                                 (element webdriver-element))
  "Get the shadow root for element ELEMENT in session SELF."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/shadow"
                                        (oref self id)
                                        (oref element id))
                           "GET"))))
    (make-instance 'webdriver-shadow :reference (cdar value))))

(cl-defmethod webdriver-element-selected-p ((self webdriver-session)
                                            (element webdriver-element))
  "Non-nil if element ELEMENT in session SELF is selected."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/selected"
                                        (oref self id)
                                        (oref element id))
                           "GET"))))
    (if (eq value json-false) nil t)))

(cl-defmethod webdriver-get-element-attribute ((self webdriver-session)
                                               (element webdriver-element)
                                               attr)
  "For element ELEMENT in session SELF, return the value of attribute ATTR."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/attribute/%s"
                                  (oref self id)
                                  (oref element id)
                                  attr)
                     "GET")))

(cl-defmethod webdriver-get-element-property ((self webdriver-session)
                                              (element webdriver-element)
                                              prop)
  "For element ELEMENT in session SELF, return the value of property PROP."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/property/%s"
                                  (oref self id)
                                  (oref element id)
                                  prop)
                     "GET")))

(cl-defmethod webdriver-get-element-css-value ((self webdriver-session)
                                               (element webdriver-element)
                                               css-value)
  "For element ELEMENT in session SELF, return the value of CSS-VALUE."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/css/%s"
                                  (oref self id)
                                  (oref element id)
                                  css-value)
                     "GET")))

(cl-defmethod webdriver-get-element-text ((self webdriver-session)
                                          (element webdriver-element))
  "For element ELEMENT in session SELF, return its text."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/text"
                                  (oref self id)
                                  (oref element id))
                     "GET")))

(cl-defmethod webdriver-get-element-tag-name ((self webdriver-session)
                                              (element webdriver-element))
  "For element ELEMENT in session SELF, return its tag name."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/name"
                                  (oref self id)
                                  (oref element id))
                     "GET")))

(cl-defmethod webdriver-get-element-rect ((self webdriver-session)
                                          (element webdriver-element))
  "For element ELEMENT in session SELF, return its rectangle."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/rect"
                                        (oref self id)
                                        (oref element id))
                           "GET"))))
    (make-instance 'webdriver-rect
                   :x (alist-get 'x value)
                   :y (alist-get 'y value)
                   :width (alist-get 'width value)
                   :height (alist-get 'height value))))

(cl-defmethod webdriver-element-enabled-p ((self webdriver-session)
                                           (element webdriver-element))
  "Non-nil if element ELEMENT in session SELF is enabled."
  (let ((value (alist-get 'value
                          (webdriver-execute-command
                           self (format "session/%s/element/%s/enabled"
                                        (oref self id)
                                        (oref element id))
                           "GET"))))
    (if (eq value json-false) nil t)))

(cl-defmethod webdriver-element-get-computed-role ((self webdriver-session)
                                                   (element webdriver-element))
  "Get the role of element ELEMENT in session SELF."
  (alist-get 'value
             (webdriver-execute-command
              self (format "session/%s/element/%s/computedrole"
                           (oref self id)
                           (oref element id))
              "GET")))

(cl-defmethod webdriver-element-get-computed-label
  ((self webdriver-session)
   (element webdriver-element))
  "Get the label of element ELEMENT in session SELF."
  (alist-get 'value
             (webdriver-execute-command
              self (format "session/%s/element/%s/computedlabel"
                           (oref self id)
                           (oref element id))
              "GET")))

(cl-defmethod webdriver-element-click ((self webdriver-session)
                                       (element webdriver-element))
  "Click the element ELEMENT in session SELF."
  (webdriver-execute-command self (format "session/%s/element/%s/click"
                                          (oref self id)
                                          (oref element id))
                             "POST"))

(cl-defmethod webdriver-element-clear ((self webdriver-session)
                                       (element webdriver-element))
  "Clear the element ELEMENT in session SELF."
  (webdriver-execute-command self (format "session/%s/element/%s/clear"
                                          (oref self id)
                                          (oref element id))
                             "POST"))

(cl-defmethod webdriver-element-send-keys ((self webdriver-session)
                                           (element webdriver-element)
                                           text)
  "Send TEXT to element ELEMENT in session SELF."
  (webdriver-execute-command self (format "session/%s/element/%s/value"
                                          (oref self id)
                                          (oref element id))
                             "POST" `(:text ,text)))

;; DOM interaction.
(cl-defmethod webdriver-get-page-source ((self webdriver-session))
  "Get the page source for the current page visited by SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/source" (oref self id))
                     "GET")))

(cl-defmethod webdriver-execute-synchronous-script ((self webdriver-session)
                                                    script args)
  "Execute script SCRIPT with arguments ARGS in session SELF, synchronously."
  (webdriver-execute-command self (format "session/%s/execute/sync"
                                          (oref self id))
                             "POST" (list :script script :args args)))

(cl-defmethod webdriver-execute-asynchronous-script ((self webdriver-session)
                                                     script args)
  "Execute script SCRIPT with arguments ARGS in session SELF, asynchronously."
  (webdriver-execute-command self (format "session/%s/execute/async"
                                          (oref self id))
                             "POST" (list :script script :args args)))

;; WebDriver Cookies.
(cl-defmethod webdriver-get-all-cookies ((self webdriver-session))
  "Get all cookies in session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/cookie" (oref self id))
                     "GET")))

(cl-defmethod webdriver-get-cookie ((self webdriver-session)
                                    name)
  "Get a cookie named NAME in session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/cookie/%s" (oref self id) name)
                     "GET")))

(cl-defmethod webdriver-add-cookie ((self webdriver-session)
                                    cookie)
  "Add a cookie COOKIE to the session SELF."
  (webdriver-execute-command self (format "session/%s/cookie" (oref self id))
                             "POST" (list :cookie cookie)))

(cl-defmethod webdriver-delete-cookie ((self webdriver-session)
                                       name)
  "Delete the cookie named NAME in session SELF."
  (webdriver-execute-command self (format "session/%s/cookie/%s" (oref self id)
                                          name)
                             "DELETE"))

(cl-defmethod webdriver-delete-all-cookies ((self webdriver-session))
  "Delete all cookies in session SELF."
  (webdriver-execute-command self (format "session/%s/cookie" (oref self id))
                             "DELETE"))

;; WebDriver Actions.
(defclass webdriver-action-sequence nil
  ((id :initform ""
       :initarg :input-id
       :type string
       :documentation "ID of input source, a string.")
   (type :initform 'none
         :initarg :action-type
         :type symbol
         :documentation
         "A symbol for the source type: one of none, key, pointer or wheel.")
   (actions :initform nil
            :type list
            :documentation "A list that holds all actions to perform.")
   (pointer-params :initform nil
                   :type list
                   :documentation
                   "Extra pointer parameters to pass when type is pointer."))
  "Represent a WebDriver input source.")

(cl-defmethod webdriver-object-to-plist ((self webdriver-action-sequence))
  "Represent SELF, a `webdriver-action-sequence' object, as a property list.

The property list is as: (PROP VAL), where PROP is each property of SELF
and VAL is the value of that property."
  (let ((props '(:type :id :actions :parameters))
        (slots '(id type actions pointer-params)))
    (cl-mapcan (lambda (prop _slot)
                 (list prop (slot-value self prop)))
               props slots)))

(cl-defmethod webdriver-json-serialize ((self webdriver-action-sequence))
  "JSON-serialize SELF, a `webdriver-action-sequence' object.

Calls `json-serialize' with SELF represented as a property list."
  (json-serialize (webdriver-object-to-plist self)))

(cl-defmethod webdriver-perform-actions ((self webdriver-session)
                                         (actions webdriver-action-sequence))
  "Perform the actions stored in slot actions of ACTIONS, in the session SELF."
  (webdriver-perform-actions
   self (make-instance 'webdriver-actions
                       :actions (webdriver-object-to-plist actions))))

(cl-defmethod webdriver-append-action ((self webdriver-action-sequence)
                                       subtype extra)
  "Append an action of subtype SUBTYPE with EXTRA parameters to SELF.

SUBTYPE should be a symbol representing one of the supported action subtypes:
pause, keyDown, keyUp, pointerDown, pointerUp, pointerMove, pointerCancel,
scroll.

EXTRA should be a property list with the parameters that the action to add
needs."
  (oset self actions
        (append (oref self actions) (append (list :type subtype) extra))))

(defclass webdriver-actions nil
  ((actions
    :initform nil
    :initarg :actions
    :type list
    :documentation
    "A property list representation of a `webdriver-action-sequence' object."))
  "Holds a plist that represents a `webdriver-action-sequence' objects.")

(cl-defmethod webdriver-perform-actions ((self webdriver-session)
                                         (actions webdriver-actions))
  "Perform the actions stored in ACTIONS in session SELF."
  (webdriver-execute-command self (format "session/%s/actions" (oref self id))
                             "POST" (json-serialize
                                     (list :actions (oref actions actions)))))

(cl-defmethod webdriver-release-actions ((self webdriver-session))
  "Release all keys and pointer buttons currently depressed in session SELF."
  (webdriver-execute-command self (format "session/%s/actions" (oref self id))
                             "DELETE"))

;; WebDriver Alerts.
(cl-defmethod webdriver-dismiss-alert ((self webdriver-session))
  "Dismiss an alert in session SELF."
  (webdriver-execute-command self (format "session/%s/alert/dismiss"
                                          (oref self id))
                             "POST"))

(cl-defmethod webdriver-accept-alert ((self webdriver-session))
  "Accept an alert in session SELF."
  (webdriver-execute-command self (format "session/%s/alert/accept"
                                          (oref self id))
                             "POST"))

(cl-defmethod webdriver-get-alert-text ((self webdriver-session))
  "Return the alert text in session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/alert/text" (oref self id))
                     "GET")))

(cl-defmethod webdriver-send-alert-text ((self webdriver-session)
                                         text)
  "Send text TEXT to the alert in session SELF."
  (webdriver-execute-command self
                             (format "session/%s/alert/text" (oref self id))
                             "POST" (list :text text)))

;; WebDriver Screenshots.
(cl-defmethod webdriver-take-screenshot ((self webdriver-session))
  "Return an encoded string of a screenshot capture in session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/screenshot" (oref self id))
                     "GET")))

(cl-defmethod webdriver-take-element-screenshot ((self webdriver-session)
                                                 (element webdriver-element))
  "As `webdriver-take-screenshot', but for an element ELEMENT in session SELF."
  (alist-get 'value (webdriver-execute-command
                     self (format "session/%s/element/%s/screenshot"
                                  (oref self id)
                                  (oref element id))
                     "GET")))

(cl-defmethod webdriver-print-page ((self webdriver-session)
                                    printobj)
  "Print the current page visited by SELF as a pdf file, using PRINTOBJ.

PRINTOBJ is a plist with all the parameters to pass to the print command."
  (webdriver-execute-command self (format "session/%s/print" (oref self id))
                             "POST" printobj))

(provide 'webdriver)
;;; webdriver.el ends here
