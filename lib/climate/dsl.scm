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
    (export climate group options)
    (import (rnrs)
	    (climate types)
	    (srfi :37 args-fold))

(define-syntax climate
  (syntax-rules ()
    ((_ name (clause ...) ...)
     (make-climate 'name (list (command clause ...) ...)))))

(define-syntax group (syntax-rules ()))
(define-syntax options (syntax-rules ()))

(define-syntax command
  (syntax-rules (group options)
    ;; command group 
    ((_ name usage (group (clause ...) ...))
     (make-command-group 'name 'usage (list (command clause ...) ...)))
    ((_ name (group clause ...))
     (command name #f (group clause ...)))
    ;; with options
    ((_ name usage process (options option ...))
     (command-options name usage process (option ...)))
    ((_ name process (options opts ...))
     (command name #f process (options opts ...)))
    ;; executor
    ((_ name usage process)
     (make-command-executor 'name 'usage process '() #f))
    ((_ name process)
     (command name #f process))
))

(define-syntax command-options
  (syntax-rules ()
    ((_ name usage process (option ...))
     (command-options "walk" name process (usage) () () (option ...)))
    ((_ "walk" name process (usage ...) (arg ...) (opt ...)
	((key names arg? default msg) options ...))
     (command-options "walk" name process
		      (usage ... (make-option-usage 'names msg arg? default))
		      (arg ... tmp)
		      (opt ... (key names arg? default))
		      (options ...)))
    ((_ "walk" name process (usage ...) (arg ...) (opt ...) ())
     (command-options "ret" name process (usage ...) (arg ...) (opt ...)
		      () () () ()))
    ((_ "ret" name process usage (arg args ...)
	((key names argument? default) opts ...) (p ...) (v ...) (d ...)
	(next ...))
     (command-options "ret" name process usage (args ...) (opts ...)
      (p ... arg)
      (v ... (list key arg))
      (d ... (list key default))
      (next ... (names argument?))))
    ((_ "ret" name process usage () () (p ...) (v ...) (d ...) (next ...))
     (command-options "opt" name process usage (p ...) (v ...) (d ...)
		      (next ...) ()))

    ((_ "opt" name process usage (a ...) (v ...) (d ...)
	((names argument?) next ...)
	(opts ...))
     (command-options "opt" name process usage (a ...) (v ...) (d ...) (next ...)
	(opts ... (option 'names argument? #f
			  (lambda (option name a ... argv)
			    (values v ... argv))))))
    ((_ "opt" name process (u ...) (a ...) (v ...) (d ...) () (opts ...))
     (make-command-executor 'name (list u ...) process
      (list opts ...)
      (lambda (command args)
	(let-values (((a ... argv)
		      (args-fold args
				 (command-executor-options command)
				 (lambda (option n arg . seeds)
				   (error 'command-executor-option-parser
					  "Unknown option"
					  n))
				 (lambda (operand a ... argv)
				   (values a ... (cons operand argv)))
				 d ... '())))
	  (append a ... argv)))))))
)
