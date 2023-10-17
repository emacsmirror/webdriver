# Copyright (C) 2022-2023 Mauro Aranda

# This file is part of webdriver.

# webdriver is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# webdriver is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with webdriver.  If not, see <https://www.gnu.org/licenses/>.

## Programs used.
EMACS = emacs
EMACSFLAGS = -batch -L . -f batch-byte-compile
MAKEINFO ?= makeinfo

## Variables (some might not be used right now).
PACKAGE = webdriver
PACKAGE_BUGREPORT = maurooaranda@gmail.com
PACKAGE_NAME = webdriver
PACKAGE_STRING = webdriver 0.1
PACKAGE_TARNAME = webdriver-0.1
PACKAGE_URL = 
PACKAGE_VERSION = 0.1
DISTDIR = $(PACKAGE_TARNAME)
DISTFILES = COPYING README.md Makefile webdriver.el webdriver-chrome.el

MAKEHTML_FLAGS = --html --output=$(PACKAGE_HTML_MANUAL_DIR)

## Targets.

.PHONY: all info clean dist

all: webdriver.elc webdriver-chrome.elc

info: webdriver.info

html: webdriver.html

webdriver.elc: webdriver.el
	$(EMACS) $(EMACSFLAGS) webdriver.el

webdriver-chrome.elc: webdriver-chrome.el
	$(EMACS) $(EMACSFLAGS) webdriver-chrome.el

webdriver.info: webdriver.texi
	$(MAKEINFO) webdriver.texi

webdriver.html: webdriver.texi
	$(MAKEINFO) $(MAKEHTML_FLAGS) webdriver.texi

clean:
	-rm -f webdriver.elc
	-rm -f webdriver-chrome.elc
	-rm -f $(PACKAGE_TARNAME).tar.gz
	-rm -f webdriver.info
	-rm -f -r $(PACKAGE_HTML_MANUAL_DIR)

dist: webdriver.elc webdriver-chrome.elc info
	mkdir --parents $(DISTDIR)
	cp --parents $(DISTFILES) $(DISTDIR)
	tar -cf $(PACKAGE_TARNAME).tar $(DISTDIR)
	rm -R $(DISTDIR)
	gzip $(PACKAGE_TARNAME).tar
