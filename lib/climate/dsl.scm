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
    (export climate group arguments options)
    (import (rnrs)
	    (sagittarius)
	    (climate types)
	    (srfi :1 lists)
	    (srfi :37 args-fold))

(define-syntax climate
  (syntax-rules ()
    ((_ name (clause ...) ...)
     (make-climate 'name (list (command clause ...) ...)))))

(define-syntax group (syntax-rules ()))
(define-syntax arguments (syntax-rules ()))
(define-syntax options (syntax-rules ()))

(define-syntax command
  (syntax-rules (group options arguments)
    ;; command group 
    ((_ name usage (group (clause ...) ...))
     (make-command-group 'name 'usage (list (command clause ...) ...)))
    ((_ name (group clause ...))
     (command name #f (group clause ...)))
    ;; with arguments
    ((_ name usage process (arguments args option ...))
     (command-options name usage process args (option ...)))
    ((_ name process (arguments args ...))
     (command name #f process (arguments args ...)))
    ;; with options
    ((_ name usage process (options option ...))
     (command-options name usage process #f (option ...)))
    ((_ name process (options opts ...))
     (command name #f process (options opts ...)))
    ;; executor
    ((_ name usage process)
     (make-command-executor 'name 'usage process #f '() #f))
    ((_ name process)
     (command name #f process))
))

(define-syntax command-options
  (syntax-rules ()
    ((_ name usage process (required ...) (option ...))
     (command-options "walk"
		      (name process (required ...))
		      (usage (required-usage required) ...)
		      () () (option ...)))
    ((_ name usage process #f (option ...))
     (command-options "walk"
		      (name process #f)
		      (usage)
		      () () (option ...)))
    ((_ "walk" info (usage ...) (arg ...) (opt ...)
	((key names arg? default msg) options ...))
     (command-options "walk" info
		      (usage ... (make-option-usage msg 'names arg? default))
		      (arg ... tmp)
		      (opt ... (key names arg? default))
		      (options ...)))
    ((_ "walk" info (usage ...) (arg ...) (opt ...) ())
     (command-options "ret" info (usage ...) (arg ...) (opt ...) () () () ()))
    ((_ "ret" info usage (arg args ...)
	((key names argument? default) opts ...) (p ...) (v ...) (d ...)
	(next ...))
     (command-options "ret" info usage (args ...) (opts ...)
      (p ... arg)
      (v ... (list key arg))
      (d ... (list key default))
      (next ... (names argument?))))
    ((_ "ret" info usage () () (p ...) (v ...) (d ...) (next ...))
     (command-options "opt" info usage (p ...) (v ...) (d ...)
		      (next ...) ()))

    ((_ "opt" info usage (a ...) (v ...) (d ...)
	((names argument?) next ...)
	(opts ...))
     (command-options "opt" info usage (a ...) (v ...) (d ...) (next ...)
	(opts ... (option 'names argument? #f
			  (lambda (option name a ... argv)
			    (values v ... argv))))))
    ((_ "opt" (name process required) (u ...) (a ...) (v ...) (d ...)
	() (opts ...))
     (make-command-executor
      'name (list u ...) process
      (extract-required-names required)
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
	  (unless (or (not 'required) (= (length argv) (length 'required)))
	    (assertion-violation 'command-executor-option-parser
	      (format "Wrong number of arguments, required ~s" 'required)))
	  (append (reverse! argv) a ...)))))))

(define-syntax required-usage
  (syntax-rules ()
    ((_ name) (make-required-usage #f 'name))
    ((_ (name usage)) (make-required-usage usage 'name))))

(define-syntax extract-required-names
  (syntax-rules ()
    ((_ (required ...))
     (extract-required-names "collect" (required ...) ()))
    ((_ required) #f)
    ((_ "collect" ((name usage) rest ...) (names ...))
     (extract-required-names "collect" (rest ...) (names ... name)))
    ((_ "collect" (name rest ...) (names ...))
     (extract-required-names "collect" (rest ...) (names ... name)))
    ((_ "collect" () (names ...)) '(names ...))))
    
)
