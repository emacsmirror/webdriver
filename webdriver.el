;;; webdriver.el --- WebDriver local end implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mauro Aranda

;; Author: Mauro Aranda <maurooaranda@gmail.com>
;; Maintainer: Mauro Aranda <maurooaranda@gmail.com>
;; Created: Wed Nov 23 10:24:00 2022
;; Version: 0.1
;; Package-Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools

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
;; a local end, in ELisp.

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
;; Get Timeouts -> `webdriver-get-timeouts'
;; Set Timeouts -> `webdriver-set-timeouts'
;; Navigate To -> `webdriver-goto-url'
;; Get Current Url -> `webdriver-get-current-url'
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
;; Find Elemenets From Element -> `webdriver-find-elements-from-element'
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

;;; Code:
;; Variables and Options.
(defgroup webdriver nil "WebDriver options."
  :group 'tools
  :version "0.1")

(defcustom webdriver-default-service 'webdriver-service-firefox
  "The default service to use when creating a session.

When creating a `webdriver-session' object without a specified service,
a service of this class will be instantiated when executing
`webdriver-sessions-start'.

Its value should be a symbol, a class name for a `webdriver-service'."
  :type 'symbol
  :package-version "0.1")

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
(defun webdriver-check-for-error (value)
  "Check if VALUE, the JSON response from the webdriver, is an error value.

If it is, then signal an error.  If it is not, return VALUE."
  (let ((val (alist-get 'value value)))
    (when (and (listp val) (alist-get 'error val))
      (error (format "Webdriver error (%s): %s"
                     (alist-get 'error val) (alist-get 'message val))))
    value))

;; WebDriver Service.
(defclass webdriver-service nil
  ((executable
    :initarg :executable
    :type string
    :documentation "The executable to run for this service.")
   (port
    :initarg :port
    :initform nil
    :type (or boolean integer)
    :documentation
    "Port to connect to, usually passed as a command-line argument.")
   (log
    :initarg :log
    :initform nil
    :type (or boolean string buffer)
    :documentation "Buffer to use when logging information.")
   (buffer
    :initarg :buffer
    :initform nil
    :type (or boolean string buffer)
    :documentation "Buffer to use for I/O with the process.")
   (process
    :initform nil
    :type (or boolean process)
    :documentation "Process for this service.")
   (args
    :initarg :args
    :initform nil
    :type list
    :documentation "Arguments to pass to executable."))
  "A WebDriver service abstraction, for running the remote ends.")

(cl-defmethod webdriver-service-start ((self webdriver-service)
                                       &optional retries)
    "Start a new service described by SELF.

This starts a new process which runs `executable' and connects it via a pipe.

To start the new process, it calls `make-process', with `executable' as the
program to run and `args' as the arguments to pass.  It stores the new process
in `process'.

If `log' is non-nil, use that buffer to log information.

If `buffer' is non-nil, associate the process with that buffer.

Optional argument RETRIES may be a number to specify the number of attempts
to connect to the server before giving up."
  (let ((exec (oref self executable))
        (retries (or retries 10))
        (i 0)
        done)
    (unless (executable-find exec)
      (error "Can't find the driver to run"))
    (oset self process (make-process :name exec
                                     :command (append (list exec)
                                                      (oref self args))
                                     :connection-type 'pipe
                                     :buffer (oref self buffer)))
    (accept-process-output (oref self process) 1.0 nil t)
    (while (and (process-live-p (oref self process))
                (< i retries)
                (not (setq done (webdriver-service-connectable-p self))))
      (setq i (1+ i)))
    (cond
     ((not (process-live-p (oref self process)))
      (error "Service unexpectedly closed"))
     ((= i retries)
      (error "Couldn't connect to the service"))
     (t
      (when (oref self log)
        (with-current-buffer (get-buffer-create (oref self log))
          (insert "Started executable correctly")))))))

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
  ;; FIXME: Obviously 0 won't work.
  (format "http://localhost:%d" (or (oref self port) 0)))

;; Firefox Service.
(defclass webdriver-service-firefox (webdriver-service)
  ((executable
    :initform "geckodriver"
    :documentation "Executable when running a Firefox Service.
This is usually \"geckodriver\", and it should be in `executable-path'.")
   (port
    :initform 4444
    :documentation "Port to pass as an option to geckodriver.
By default, it is 4444, which is the default for geckodriver."))
  "The Firefox Service, that runs the geckodriver.")

(cl-defmethod webdriver-service-start :before
  ((self webdriver-service-firefox) &optional retries)
  "Add the port argument to the command line."
  (oset self args (list "--port" (number-to-string (oref self port)))))

;; WebDriver Session.
(defclass webdriver-session nil
  ((service
    :initform nil
    :initarg :service
    :type (or boolean webdriver-service)
    :documentation
    "An instance of `webdriver-service' that the session should connect to.")
   (id :initform nil
       :type (or boolean string)
       :documentation "String ID for this session.")
   (requested-capabilities
    :initform nil
    :initarg :requested-capabilities
    :type list
    :documentation
    "The requested capabilities passed when creating the session.")
   (capabilities
    :initform nil
    :type list
    :documentation "The actual capabilities that the session supports."))
  "Represent a WebDriver session to control a browser.")

(cl-defmethod initialize-instance :after ((self webdriver-session) &rest _args)
  "If there is no service associated with SELF, create a default one."
  (unless (oref self service)
    (let ((service (make-instance webdriver-default-service)))
      (webdriver-service-start service)
      (oset self service service))))

(cl-defmethod webdriver-session-start ((self webdriver-session))
  "Start a new session associated to SELF."
  (when (oref self id)
    (error "Session already started"))
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name "session"
                                 :body `(:capabilities
                                         ,(oref self requested-capabilities))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (oset self id (alist-get 'sessionId value))
    (oset self capabilities (alist-get 'capabilities value))))

(cl-defmethod webdriver-session-stop ((self webdriver-session))
  "Stop the session associated to SELF."
  (unless (oref self id)
    (error "Session doesn't have a session ID"))
  (let* ((command (make-instance 'webdriver-command
                                 :method "DELETE"
                                 :name (format "session/%s" (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (oset self id nil)))

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

(cl-defmethod webdriver-send-command ((self webdriver-session)
                                      (command webdriver-command))
  "Send the command COMMAND in session SELF.

Serializes the body of COMMAND only if it is not a string."
  (let* ((url-request-method (oref command method))
         (url-request-data (if (stringp (oref command body))
                               (oref command body)
                             (json-serialize (oref command body))))
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
  "With the session SELF, navigate to the url URL."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/url" (oref self id))
                                 :body `(:url ,url)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))
  
(cl-defmethod webdriver-get-current-url ((self webdriver-session))
  "Return the current url of the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/url"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-go-back ((self webdriver-session))
  "Go back to the previous url of the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/back"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-go-forward ((self webdriver-session))
  "Go forward to the next url of the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/forward"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-refresh ((self webdriver-session))
  "Refresh the current url of the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/refresh"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-get-title ((self webdriver-session))
  "Get the title of the current page visited by SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/title"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

;; WebDriver Timeouts.
(defclass webdriver-timeouts nil
  ((script :initform 30 :initarg :script :type (or integer null)
           :documentation "Timeout limit for script evaluation.")
   (pageLoad :initform 300 :initarg :pageLoad :type (or integer null)
             :documentation "Timeout limit for an explicit navigation attempt.")
   (implicit :initform 0 :initarg :implicit :type (or integer null)
             :documentation "Timeout limit for locating an element."))
  "Representation of a WebDriver timeouts configuration.

Each property is stored as a number of seconds.")

(cl-defmethod webdriver-object-to-plist ((self webdriver-timeouts))
  "Represent SELF, a `webdriver-timeouts' object, as a property list.

The property list is as: (PROP VAL), where PROP is each property of SELF
and VAL is the value of that property."
  (let ((timeouts '(:script :pageLoad :implicit))
        (props '(script pageLoad implicit)))
    (cl-mapcan (lambda (timeout prop)
                 (when (slot-value self prop)
                   (list timeout (* (slot-value self prop) 1000))))
               timeouts props)))

(cl-defmethod webdriver-json-serialize ((self webdriver-timeouts))
  "JSON-serialize SELF, a `webdriver-timeouts' object.

Calls `json-serialize' with SELF represented as a property list."
  (json-serialize (webdriver-object-to-plist self)))

(cl-defmethod webdriver-get-timeouts ((self webdriver-session))
  "Get the timeout specification for the session SELF.

Returns a `webdriver-timeouts' object with the current timeout specification."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/timeouts"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-timeouts
                   :script (/ (alist-get 'script value) 1000)
                   :pageLoad (/ (alist-get 'pageLoad value) 1000)
                   :implicit (/ (alist-get 'implicit value) 1000))))

(cl-defmethod webdriver-get-timeout ((self webdriver-session) timeout)
  "Get the timeout in seconds for timeout TIMEOUT in session SELF.

TIMEOUT should be a symbol, one of script, pageLoad or implicit."
  (let ((timeouts (webdriver-get-timeouts self)))
    (if timeout
        (if (member timeout '(script pageLoad implicit))
            (slot-value timeouts timeout)
          (error (format "Webdriver error (%s): %s"
                         "invalid timeout" "")))
      timeouts)))

(cl-defmethod webdriver-set-timeouts ((self webdriver-session)
                                      (timeouts webdriver-timeouts))
  "Set the timeouts specification for the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/timeouts"
                                               (oref self id))
                                 :body (webdriver-json-serialize timeouts)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-set-timeout ((self webdriver-session) timeout secs)
  "Set timeout TIMEOUT to value SECS times 1000 in session SELF.

TIMEOUT should be a symbol, one of script, pageLoad or implicit."
  (if (member timeout '(script pageLoad implicit))
      (let* ((command (make-instance 'webdriver-command
                                     :method "POST"
                                     :name (format "session/%s/timeouts"
                                                   (oref self id))
                                     :body (list (intern (format ":%s" timeout))
                                                 (* secs 1000))))
             (value (webdriver-send-command self command)))
        (webdriver-check-for-error value))
    (error (format "Webdriver error (%s): %s"
                   "invalid timeout" ""))))

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
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/window"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-close-window ((self webdriver-session))
  "Close the current window handle associated to the session SELF.

If there are no more open top-level windows, stop SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "DELETE"
                                 :name (format "session/%s/window"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    ;; Close session when there are no more windows.
    (when (seq-empty-p value)
      (webdriver-session-stop self))
    value))

(cl-defmethod webdriver-switch-to-window ((self webdriver-session) handle)
  "Switch to the window with handle HANDLE in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window"
                                               (oref self id))
                                 :body (list :handle handle)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-get-window-handles ((self webdriver-session))
  "Get all window handles associated to the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/window/handles"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-create-new-window ((self webdriver-session) type)
  "Create a new window handle of type TYPE for the session SELF.

TYPE defaults to \"tab\", and can be one of \"tab\" or \"window\"."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window/new"
                                               (oref self id))
                                 :body (json-serialize
                                        `(:type ,(or type "tab")))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-window-rect ((self webdriver-session))
  "Get the window rectangle for the current window handle of the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/window/rect"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-rect
                   :width (alist-get 'width value)
                   :height (alist-get 'height value)
                   :x (alist-get 'x value)
                   :y (alist-get 'y value))))

(cl-defmethod webdriver-set-window-rect ((self webdriver-session)
                                         (rect webdriver-rect))
  "Set rectangle to RECT for the current window handle of session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window/rect"
                                               (oref self id))
                                 :body (webdriver-json-serialize rect)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-maximize-window ((self webdriver-session))
  "Maximize the current window associated to the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window/maximize"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-minimize-window ((self webdriver-session))
  "Minimize the current window associated to the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window/minimize"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-fullscreen-window ((self webdriver-session))
  "Fullscreen the current window associated to the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/window/fullscreen"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-switch-to-frame ((self webdriver-session) id)
  "Switch to the frame ID in the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/frame"
                                               (oref self id))
                                 :body (json-serialize (list :id id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-switch-to-parent-frame ((self webdriver-session))
  "Switch to the parent frame in the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/frame/parent"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

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
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element"
                                               (oref self id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements ((self webdriver-session)
                                       (by webdriver-by))
  "Find all elements in the current page visited by SELF with strategy BY."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/elements"
                                               (oref self id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (mapcar (lambda (el)
              (make-instance 'webdriver-element :reference (cdar el)))
            value)))

(cl-defmethod webdriver-find-element-from-element ((self webdriver-session)
                                                   (element webdriver-element)
                                                   (by webdriver-by))
  "Starting from element ELEMENT, find an element by BY in the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element/%s/element"
                                               (oref self id)
                                               (oref element id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements-from-element ((self webdriver-session)
                                                    (element webdriver-element)
                                                    (by webdriver-by))
  "Starting from element ELEMENT, find all elements by BY in the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element/%s/elements"
                                               (oref self id)
                                               (oref element id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
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
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/shadow/%s/element"
                                               (oref self id)
                                               (oref shadow id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-find-elements-from-shadow-root
  ((self webdriver-session)
   (shadow webdriver-shadow)
   (by webdriver-by))
  "Start from shadow root SHADOW and find all elements by BY in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/shadow/%s/elements"
                                               (oref self id)
                                               (oref shadow id))
                                 :body (webdriver-json-serialize by)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (mapcar (lambda (el)
              (make-instance 'webdriver-element :reference (cdar el)))
            value)))

(cl-defmethod webdriver-get-active-element ((self webdriver-session))
  "Get the active element in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/active"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-element :reference (cdar value))))

(cl-defmethod webdriver-get-element-shadow-root ((self webdriver-session)
                                                 (element webdriver-element))
  "Get the shadow root for element ELEMENT in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/shadow"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-shadow :reference (cdar value))))

(cl-defmethod webdriver-element-selected-p ((self webdriver-session)
                                            (element webdriver-element))
  "Non-nil if element ELEMENT in session SELF is selected."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/selected"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (cond ((eq value json-false) nil)
          (t t))))

(cl-defmethod webdriver-get-element-attribute ((self webdriver-session)
                                               (element webdriver-element)
                                               attr)
  "For element ELEMENT in session SELF, return the value of attribute ATTR."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name
                                 (format "session/%s/element/%s/attribute/%s"
                                         (oref self id)
                                         (oref element id)
                                         attr)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-element-property ((self webdriver-session)
                                              (element webdriver-element)
                                              prop)
  "For element ELEMENT in session SELF, return the value of property PROP."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name
                                 (format "session/%s/element/%s/property/%s"
                                         (oref self id)
                                         (oref element id)
                                         prop)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-element-css-value ((self webdriver-session)
                                               (element webdriver-element)
                                               css-value)
  "For element ELEMENT in session SELF, return the value of CSS-VALUE."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/css/%s"
                                               (oref self id)
                                               (oref element id)
                                               css-value)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-element-text ((self webdriver-session)
                                          (element webdriver-element))
  "For element ELEMENT in session SELF, return its text."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/text"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-element-tag-name ((self webdriver-session)
                                              (element webdriver-element))
  "For element ELEMENT in session SELF, return its tag name."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/name"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-element-rect ((self webdriver-session)
                                          (element webdriver-element))
  "For element ELEMENT in session SELF, return its rectangle."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/rect"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (make-instance 'webdriver-rect
                   :x (alist-get 'x value)
                   :y (alist-get 'y value)
                   :width (alist-get 'width value)
                   :height (alist-get 'height value))))

(cl-defmethod webdriver-element-enabled-p ((self webdriver-session)
                                           (element webdriver-element))
  "Non-nil if element ELEMENT in session SELF is enabled."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/element/%s/enabled"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (setq value (alist-get 'value value))
    (cond ((eq value json-false) nil)
          (t t))))

(cl-defmethod webdriver-element-click ((self webdriver-session)
                                       (element webdriver-element))
  "Click the element ELEMENT in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element/%s/click"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-element-clear ((self webdriver-session)
                                       (element webdriver-element))
  "Clear the element ELEMENT in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element/%s/clear"
                                               (oref self id)
                                               (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-element-send-keys ((self webdriver-session)
                                           (element webdriver-element)
                                           text)
  "Send TEXT to element ELEMENT in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/element/%s/value"
                                               (oref self id)
                                               (oref element id))
                                 :body `(:text ,text)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

;; DOM interaction.
(cl-defmethod webdriver-get-page-source ((self webdriver-session))
  "Get the page source for the current page visited by SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/source"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-execute-synchronous-script ((self webdriver-session)
                                                    script args)
  "Execute script SCRIPT with arguments ARGS in session SELF, synchronously."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/execute/sync"
                                               (oref self id))
                                 :body (list :script script
                                             :args args)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-execute-asynchronous-script ((self webdriver-session)
                                                     script args)
  "Execute script SCRIPT with arguments ARGS in session SELF, asynchronously."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/execute/async"
                                               (oref self id))
                                 :body (list :script script
                                             :args args)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

;; WebDriver Cookies.
(cl-defmethod webdriver-get-all-cookies ((self webdriver-session))
  "Get all cookies in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/cookie"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-get-cookie ((self webdriver-session)
                                    name)
  "Get a cookied named NAME in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/cookie/%s"
                                               (oref self id)
                                               name)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-add-cookie ((self webdriver-session)
                                    cookie)
  "Add a cookie COOKIE to the session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/cookie"
                                               (oref self id))
                                 :body (list :cookie cookie)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-delete-cookie ((self webdriver-session)
                                       name)
  "Delete the cookie named NAME in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "DELETE"
                                 :name (format "session/%s/cookie/%s"
                                               (oref self id)
                                               name)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-delete-all-cookies ((self webdriver-session))
  "Delete all cookies in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "DELETE"
                                 :name (format "session/%s/cookie"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

;; WebDriver Actions.
(defclass webdriver-action-sequence nil
  ((id :initform ""
       :initarg :input-id
       :type string
       :documentation "ID of input source, a string.")
   (type :initform none
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
    (cl-mapcan (lambda (prop slot)
                 (list prop (slot-value self prop))))))

(cl-defmethod webdriver-json-serialize ((self webdriver-action-sequence))
  "JSON-serialize SELF, a `webdriver-action-sequence' object.

Calls `json-serialize' with SELF represented as a property list."
  (json-serialize (webdriver-object-to-plist self)))

(cl-defmethod webdriver-perform-actions ((self webdriver-session)
                                         (actions webdriver-action-sequence))
  "Perform the actions stored in slot actions of ACTIONS, in the session SELF."
  (webdriver-perform-actions
   (make-instance 'webdriver-actions
                  :actions (webdriver-object-to-plist self))))

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
  (let* ((comand (make-instance 'webdriver-command
                                :method "POST"
                                :name (format "session/%s/actions"
                                              (oref self id))
                                :body (json-serialize
                                       (list :actions
                                             (oref actions actions)))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-release-actions ((self webdriver-session))
  "Release all keys and pointer buttons currently depressed in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "DELETE"
                                 :name (format "session/%s/actions"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

;; WebDriver Alerts.
(cl-defmethod webdriver-dismiss-alert ((self webdriver-session))
  "Dismiss an alert in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/alert/dismiss"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-accept-alert ((self webdriver-session))
  "Accept an alert in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/alert/accept"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(cl-defmethod webdriver-get-alert-text ((self webdriver-session))
  "Return the alert text in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/alert/text"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-send-alert-text ((self webdriver-session)
                                         text)
  "Send text TEXT to the alert in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/alert/text"
                                               (oref self id))
                                 :body (list :text text)))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

;; WebDriver Screenshots.
(cl-defmethod webdriver-take-screenshot ((self webdriver-session))
  "Return an encoded string of a screenshot capture in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format "session/%s/screenshot"
                                               (oref self id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-take-element-screenshot ((self webdriver-session)
                                                 (element webdriver-element))
  "As `webdriver-take-screenshot', but for an element ELEMENT in session SELF."
  (let* ((command (make-instance 'webdriver-command
                                 :method "GET"
                                 :name (format
                                        "session/%s/element/%s/screenshot"
                                        (oref self id)
                                        (oref element id))))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)
    (alist-get 'value value)))

(cl-defmethod webdriver-print-page ((self webdriver-session)
                                    printobj)
  "Print the current page visited by SELF as a pdf file, using PRINTOBJ.

PRINTOBJ is a plist with all the parameters to pass to the print command."
  (let* ((command (make-instance 'webdriver-command
                                 :method "POST"
                                 :name (format "session/%s/print"
                                               (oref self id))
                                 :body printobj))
         (value (webdriver-send-command self command)))
    (webdriver-check-for-error value)))

(provide 'webdriver)
;;; webdriver.el ends here
