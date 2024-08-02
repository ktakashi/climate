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

;; commands structure
;; <root>
;;   + <command-group>              ;; main command
;;       + <command-executor>       ;; sub command
;;
;;   + <command-group>              ;; main command
;;       + <command-group>          ;; sub command
;;          + <command-executor>    ;; sub sub command
;;
;;   + <command-executor>           ;; main command


#!nounbound
(library (climate types)
    (export climate? make-climate climate-name climate-commands
	    climate-command
	    command? command-name command-usage
	    command-group? make-command-group command-group-commands
	    command-group-command command-group-usage
	    command-executor? make-command-executor
	    command-executor-usage
	    command-executor-process
	    command-executor-options
	    invoke-command-executor

	    usage? usage-message
	    format-usage format-command-line-usage
	    required-usage? make-required-usage required-usage-name
	    option-usage? make-option-usage
	    option-usage-names 
	    
	    result? make-success-result make-error-result
	    result-success? result-value)
    (import (rnrs)
	    (sagittarius))

(define-record-type climate
  (fields name commands))

(define (climate-command (climate climate?) name)
  (search-command name (climate-commands climate)))

;; parent record
(define-record-type command
  (fields name
	  usage))

;; Sub commands holder
;; $ process command sub-sommand
(define-record-type command-group
  (parent command)
  (fields commands)) ;; sub commands

(define (command-group-command (command command-group?) name)
  (search-command name (command-group-commands command)))

(define (command-group-usage command parents)
  (let-values (((out e) (open-string-output-port)))
    (cond ((command-usage command) =>
	   (lambda (usage)
	     (display "Description: " out)
	     (display usage out)
	     (newline out))))
    (display "Usage:" out)
    (newline out)
    (display "$ " out)
    ;; name command [sub-command ...] [options ...]
    (for-each (lambda (name) (display name out) (display " " out))
	      (reverse parents))
    (display (command-name command) out)
    (newline out)
    (newline out)
    (display "SUB COMMANDS:" out) (newline out)
    (for-each (lambda (command)
		(display "  - " out)
		(display (command-name command) out)
		(cond ((command-usage command) =>
		       (lambda (usage)
			 (cond ((string? usage)
				(display ": " out) (display usage out))
			       ((and (pair? usage) (car usage))
				(display ": " out) (display (car usage) out))))))
		(newline out))
	      (command-group-commands command))
    (e)))

(define-record-type command-executor
  (parent command)
  (fields process
	  arguments
	  options
	  options-mapper))

(define (command-executor-usage command parents)
  (let-values (((out e) (open-string-output-port)))
    (let ((arguments (cond ((command-usage command) =>
			    (lambda (usage)
			      (when (car usage)
				(display "Description: " out)
				(display (car usage) out)
				(newline out))
			      (cdr usage))))))
      (display "Usage:" out)
      (newline out)
      (display "$ " out)
      ;; name command [sub-command ...] [options ...]
      (for-each (lambda (name) (display name out) (display " " out))
		(reverse parents))
      (display (command-name command) out)
      (for-each (lambda (u)
		  (display " " out)
		  (format-command-line-usage u out)) arguments)
      (newline out)
      (let ((required (filter required-usage? arguments))
	    (options (filter option-usage? arguments)))
	(unless (null? options)
	  (newline out)
	  (display "ARGUMENTS:" out) (newline out)
	  (for-each (lambda (u)
		      (display "  " out) (format-usage u out) (newline out))
		    required))
	(unless (null? options)
	  (newline out)
	  (display "OPTIONS:" out) (newline out)
	  (for-each (lambda (u)
		      (display "  " out) (format-usage u out) (newline out))
		    options))))
    (e)))

(define (invoke-command-executor command args)
  (let ((args (cond ((command-executor-options-mapper command) =>
		     (lambda (parser) (parser command args)))
		    (else args))))
    (apply (command-executor-process command) args)))

;; for help
(define-record-type usage
  (fields message))

(define-record-type required-usage
  (parent usage)
  (fields name))

(define-record-type option-usage
  (parent usage)
  (fields names need-argument? default))
(define (format-usage usage out)
  (define message (usage-message usage))
  (cond ((option-usage? usage)
	 (let ((opt (option-usage-option-format usage #t))
	       (default (option-usage-default usage)))
	   (display opt out) (display ": " out)
	   (when message (display message out))
	   (when (and default (not (undefined? default)))
	     (when message (display ", " out))
	     (display "default value is " out)
	     (display default out))))
	(else
	 (let ((name (required-usage-name usage)))
	   (display (or name "arg") out) (display ": " out)
	   (when message (display message out))))))

(define (format-command-line-usage usage out)
  (cond ((option-usage? usage)
	 (let ((opt (option-usage-option-format usage #f))
	       (need-argument? (option-usage-need-argument? usage)))
	   (display "[" out)
	   (display opt out)
	   (when need-argument?
	     (display " $" out)
	     (display (cadr (option-usage-names usage)) out))
	   (display "]" out)))
	(else
	 (display "$" out)
	 (display (or (required-usage-name usage) "arg") out))))

(define (option-usage-option-format usage long?)
  (let-values (((out e) (open-string-output-port)))
    (let ((names (option-usage-names usage))
	  (message (usage-message usage)))
      (display "-" out) (display (car names) out)
      (when long?
	(display "," out) (display "--" out) (display (cadr names) out))
      (e))))

;; result
(define-record-type result
  (fields success? value))

(define (make-error-result msg) (make-result #f msg))
(define (make-success-result value) (make-result #t value))

(define (search-command name commands)
  (define (name=? command) (equal? (command-name command) name))
  (cond ((memp name=? commands) => car)
	(else #f)))

)
