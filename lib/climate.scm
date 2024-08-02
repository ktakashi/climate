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

#!nounbound
(library (climate)
    (export climate group arguments options
	    climate? climate-commands climate-command
	    execute-climate
	    command-group? command-group-commands
	    command-executor? command-executor-process
	    result? result-success? result-value)
    (import (rnrs)
	    (climate dsl)
	    (climate types)
	    (sagittarius))

(define (execute-climate climate args)
  (cond ((null? args) (climate-usage climate "No command is given" #f))
	((climate-command climate (string->symbol (car args))) =>
	 (lambda (command)
	   (execute-command (list (climate-name climate)) command (cdr args))))
	(else (climate-usage climate "Command not found" (car args)))))

(define (climate-usage climate message irr)
  (let-values (((out e) (open-string-output-port)))
    (display message out)
    (when irr (display " " out) (display irr out))
    (newline out)
    ;; $ name command [sub-command ...] [options ...]
    (display "$ " out)
    (display (climate-name climate) out)
    (display " command [sub-command ...] [options ...]" out)
    (newline out)
    ;; list of available commands
    (display "  Available commands:" out) (newline out)
    (for-each (lambda (command)
		(display "  - " out)
		(display (command-name command) out)
		(newline out))
	      (climate-commands climate))
    (make-error-result (e))))

(define (execute-command exec-tree command args)
  (cond ((command-executor? command)
	 ;; TODO args to options
	 (guard (e (else (command-usage-result command exec-tree
					       (condition-message e)
					       args)))
	   (make-success-result
	    (invoke-command-executor command args))))
	((command-group? command)
	 (cond ((null? args)
		(command-usage-result command exec-tree
				      "No sub command is given" #f))
	       ((command-group-command command (string->symbol (car args))) =>
		(lambda (cmd)
		  (execute-command (cons (command-name command) exec-tree)
				   cmd (cdr args))))
	       (else (command-usage-result command exec-tree
					   "Command not found" (car args)))))
	(else (make-error-result "[BUG] unknown command"))))

(define (command-usage-result command tree msg irr)
  (let-values (((out e) (open-string-output-port)))
    (display msg out)
    (when irr (display " " out) (display irr out))
    (newline out)

    (let ((usage (cond ((command-group? command)
			(command-group-usage command tree))
		       (else
			(command-executor-usage command tree)))))
      (display usage out)
      (make-error-result (e)))))

)
