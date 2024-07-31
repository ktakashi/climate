;;; -*- mode:scheme; coding:utf-8; -*-
;;;
;;; Copyright 2024 Takashi Kato <ktakshi@ymail.com>
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;

;; DSL to create `climate`

#!nounbound
(library (climate dsl)
    (export climate group)
    (import (rnrs)
	    (climate types))

(define-syntax climate
  (syntax-rules ()
    ((_ name (clause ...) ...)
     (make-climate 'name (list (command clause ...) ...)))))

(define-syntax group (syntax-rules ()))

(define-syntax command
  (syntax-rules (group)
    ;; command group 
    ((_ name usage (group (clause ...) ...))
     (make-command-group 'name 'usage (list (command clause ...) ...)))
    ((_ name (group clause ...))
     (command name #f (group clause ...)))
    ;; executor
    ((_ name usage process)
     (make-command-executor 'name 'usage process))
    ((_ name process)
     (command name #f process))))

)
