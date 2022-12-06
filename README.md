# webdriver: Control a browser with Emacs Lisp

> A WebDriver local end implementation in Emacs Lisp

## Table of Contents

- [Example](#example)
- [Installation](#installation)
- [License](#license)

---

## Example

Here is a usage example:

```
(let ((session (make-instance 'webdriver-session)))
  (webdriver-session-start session)
  (webdriver-goto-url session "https://www.example.org")
  (let ((element
         (webdriver-find-element session (make-instance 'webdriver-by
                                                        :strategy "tag name"
                                                        :selector "h1"))))
    (message (webdriver-get-element-text session element)))
  (webdriver-session-stop session))
```

---

Here is another, taken from the Firefox docs:
```
(require 'webdriver)
(let ((session (make-instance 'webdriver-session))
      element results)
  (webdriver-session-start session)
  (webdriver-goto-url session "https://www.google.com/ncr")
  (setq element (webdriver-find-element session (make-instance
                                                 'webdriver-by
                                                 :strategy "tag name"
                                                 :selector "input")))
  (webdriver-send-keys session element "cheese\ue006")
  (setq results (webdriver-find-elements session (make-instance
                                                  'webdriver-by
                                                  :strategy "css selector"
                                                  :selector "h3")))
  (message (webdriver-get-element-text session (nth 0 results))))
```
## Installation

Download the webdriver.el file, byte-compile it and add the directory
to your load path.

---

## License

- **[GPL 3](https://www.gnu.org/licenses/gpl-3.0-standalone.html)**
- Copyright 2022 Mauro Aranda
