;;; webdriver-tests.el --- Tests for webdriver.el    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mauro Aranda

;; Author: Mauro Aranda <mauro@maurooaranda.com>
;; Keywords: 

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

;;; Code:
(ert-deftest webdriver-test-service-start-and-stop ()
  "Test that starting and stopping a service works."
  (let ((service (make-instance 'webdriver-service-firefox :executable "geckodriver"
                                :buffer "*webdriver*")))
    (webdriver-service-start service)
    (unwind-protect
        (progn
          (with-current-buffer (url-retrieve-synchronously
                                (webdriver-service-url service))
            (goto-char (point-min))
            (re-search-forward "\n\n")
            (should (string-equal
                     (buffer-substring (point) (point-max))
                     "HTTP method not allowed"))))
      (webdriver-service-stop service))
    (should (condition-case _
                (progn
                  (url-retrieve-synchronously (webdriver-service-url service))
                  nil)
              (file-error t)))))

(provide 'webdriver-tests)
;;; webdriver-tests.el ends here
