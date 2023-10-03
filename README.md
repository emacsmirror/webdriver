# webdriver: Control a browser with Emacs Lisp

> A WebDriver local end implementation in Emacs Lisp

## Table of Contents

- [Example](#example)
- [Installation](#installation)
- [License](#license)

---

## Examples

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
  (webdriver-set-timeout session 'implicit 1)
  (setq element (webdriver-find-element session (make-instance
                                                 'webdriver-by
                                                 :strategy "tag name"
                                                 :selector "textarea")))
  (webdriver-element-send-keys session element "cheese\ue006")
  (setq results (webdriver-find-elements session (make-instance
                                                  'webdriver-by
                                                  :strategy "css selector"
                                                  :selector "h3")))
  (message (webdriver-get-element-text session (nth 0 results)))
  (webdriver-stop-session))
```

If you want to run the above examples with a headless firefox session,
use a `webdriver-capabilities` object:
```
(let ((caps (make-instance 'webdriver-capabilities-firefox)))
  (webdriver-capabilities-firefox-add-arg caps "-headless" t)
  (make-instance 'webdriver-session :requested-capabilities caps))
```

## Installation

This package is available on [MELPA](https://melpa.org), so it can be
installed with
`M-x package-install RET webdriver RET`

Alternatively, you can get a clone of this repo, run make and arrange
for adding the directory to your load path.

---

## License

- **[GPL 3](https://www.gnu.org/licenses/gpl-3.0-standalone.html)**
- Copyright 2022-2023 Mauro Aranda
