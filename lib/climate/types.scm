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
	    command-group-command
	    command-executor? make-command-executor
	    command-executor-process
	    command-executor-options
	    invoke-command-executor

	    option-usage? make-option-usage
	    option-usage-names option-usage-message
	    option-usage-format option-usage-command-line-format
	    
	    result? make-success-result make-error-result
	    result-success? result-value)
    (import (rnrs))

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

(define-record-type command-executor
  (parent command)
  (fields process
	  options
	  options-mapper))

(define (invoke-command-executor command args)
  (let ((args (cond ((command-executor-options-mapper command) =>
		     (lambda (parser) (parser command args)))
		    (else args))))
    (apply (command-executor-process command) args)))


;; for help
(define-record-type option-usage
  (fields names message need-argument? default))
(define (option-usage-format usage out)
  (let ((opt (option-usage-option-format usage #t))
	(message (option-usage-message usage))
	(default (option-usage-default usage)))
    (display opt out) (display ": " out)
    (display message out)
    (when default
      (display ", default value is " out)
      (display default out))))

(define (option-usage-command-line-format usage out)
  (let ((opt (option-usage-option-format usage #f))
	(need-argument? (option-usage-need-argument? usage)))
    (display "[" out)
    (display opt out)
    (when need-argument?
      (display " $" out)
      (display (cadr (option-usage-names usage)) out))
    (display "]" out)))

(define (option-usage-option-format usage long?)
  (let-values (((out e) (open-string-output-port)))
    (let ((names (option-usage-names usage))
	  (message (option-usage-message usage)))
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
