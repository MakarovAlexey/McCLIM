;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-SYS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: CLIM-2, Chapter 32.2 Multi-processing
;;;            for ACL
;;;   Created: 2001-05-22
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;;   License: LGPL (See file COPYING for details).
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2001 by Gilbert Baumann

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the 
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, 
;;; Boston, MA  02111-1307  USA.

(in-package :CLIM-SYS)

(defconstant *multiprocessing-p* t)

(defun make-process (function &key name)
  (mp:process-run-function name function))

(defun destroy-process (process)
  (mp:process-kill process))

(defun current-process ()
  sys:*current-process*)

(defun all-processes ()
  sys:*all-processes*)

(defun processp (object)
  (typep object 'mp:process))

(defun process-name (process)
  (mp:process-name process))

(defun process-state (process)
  (declare (ignore process))
  ;; Hmm can we somehow gain useful information here?
  nil)

(defun process-whostate (process)
  (mp:process-whostate process))

(defun process-wait (reason predicate)
  (mp:process-wait reason predicate))

(defun process-wait-with-timeout (reason timeout predicate)
  (mp:process-wait-with-timeout reason timeout predicate))

(defun process-yield ()
  (mp:process-allow-schedule))

(defun process-interrupt (process function)
  (mp:process-interrupt process function))

(defun disable-process (process)
  (mp:process-add-arrest-reason process 'suspend))

(defun enable-process (process)
  (mp:process-revoke-arrest-reason process 'suspend))

(defun restart-process (process)
  (mp:process-reset process) )

(defmacro without-scheduling (&body body)
  `(mp:without-scheduling .,body))

;; We perhaps could make use of EXCL::ATOMICALLY, which is
;; undocumented, but seems to do what we want.

(defmacro atomic-incf (place)
  `(locally (declare (optimize (safety 1) (speed 3)))
     (excl::atomically (incf (the fixnum ,place)))))

(defmacro atomic-decf (place)
  `(locally (declare (optimize (safety 1) (speed 3)))
     (excl::atomically (decf (the fixnum ,place)))))

;;; 32.3 Locks

(defun make-lock (&optional name)
  (mp:make-process-lock :name name))

(defmacro with-lock-held ((place &optional state) &body body)
  `(mp:with-process-lock (,place :norecursive t
                                 ,@(if state (list :whostate state) nil))
     .,body))

(defun make-recursive-lock (&optional name)
  (mp:make-process-lock :name name))

(defmacro with-recursive-lock-held ((place &optional state) &body body)
  `(mp:with-process-lock (,place ,@(if state (list :whostate state) nil))
     .,body))
