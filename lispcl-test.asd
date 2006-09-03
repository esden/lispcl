;;; ---------------------------------------------------------------------------- ;;;
;;;  LISCPL - Lisp Player Client - lispcl-test.asd                               ;;;
;;;  Copyright (C) 2006, Armin Mueller                                           ;;;
;;;                                                                              ;;;
;;;  This program is free software; you can redistribute it and/or modify        ;;;
;;;  it under the terms of the GNU General Public License as published by        ;;;
;;;  the Free Software Foundation; either version 2 of the License, or           ;;;
;;;  (at your option) any later version.                                         ;;;
;;;                                                                              ;;;
;;;  This program is distributed in the hope that it will be useful,             ;;;
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of              ;;;
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the               ;;;
;;;  GNU General Public License for more details.                                ;;;
;;;                                                                              ;;;
;;;  You should have received a copy of the GNU General Public License           ;;;
;;;  along with this program; if not, write to the Free Software                 ;;;
;;;  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   ;;;
;;; ---------------------------------------------------------------------------- ;;;

;; *****************************************************************************
;; ** lisp player client library test files                                   **
;; *****************************************************************************

(asdf:operate 'asdf:load-op :lispcl)

(in-package "LISPCL")

; asdf definition
(asdf:defsystem lispcl-test
  :name "Lisp player client (Test files)"
  :author "Armin Mueller <armin-mueller@users.sourceforge.net>"
  :version "svn-version"
  :maintainer "Armin Mueller <armin-mueller@users.sourceforge.net>"
  :licence "GPL v2 or higher"
  :description "Lisp player client (Test files)"
  :long-description "Lisp player client for the Player Project (Test files)"

  :components
  ( (:module "src"
      :default-component-class asdf::lispcl-source-file
      :components
      ( (:file "tests-examples") )) ))

