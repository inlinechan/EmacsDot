;; Copyright (C) 2013 Hyungchan Kim

;; Author: Hyungchan Kim <inlinechan@gmail.com>
;; Keywords: lisp
;; Version: 0.1

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

;;; Code:

(require 'cl)                           ; common lisp goodies, loop

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/hc")

(require 'hc-el-get)
(require 'hc-local-packages)
(require 'hc-general)
(require 'hc-general-key)
(require 'hc-ido)
(require 'hc-korean)
(require 'hc-ui)
(require 'hc-shell)
;; (require 'hc-cedet)
(require 'hc-newsticker)
(require 'hc-gnus)
(require 'hc-org)
(require 'js-beautify)
(require 'hc-webos)

;;; init ends here
