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
	    command-executor? make-command-executor command-executor-process

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
  (fields process))

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
