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
(library (json pp)
    (export json-pp)
    (import (rnrs)
	    (text json)
	    (climate))

(define (json-pp in)
  (let ((tin (transcoded-port (argument->input-port in) (native-transcoder))))
    (let-values (((out e) (open-string-output-port)))
      (json-pretty-print (json-read tin) out)
      (e))))

(define (json-pretty-print json :optional (out (current-output-port)))
  (define (write-indent n out)
    (do ((i 0 (+ i 1)))
	((= i (* n 2)))
      (display #\space out)))
  (define (write-ht vec out indent)
    (display "{" out) (newline out)
    (do ((len (vector-length vec)) (n (+ indent 1)) (i 0 (+ i 1)))
	((= i len))
      (write-indent n out)
      (let* ((e (vector-ref vec i))
	     (k (car e))
	     (v (cdr e)))
	(cond ((symbol? k) (write (symbol->string k) out))
	      ((string? k) (write k out))
	      (else (error 'json-pp "Invalid JSON object key" k)))
	(display ": " out)
	(write-any v out n))
      (newline out))
    (write-indent indent out) (display "}" out))
  (define (write-array x out indent)
    (define new-indent (+ indent 1))
    (display "[" out)
    (let loop ((comma? #f) (x x))
      (unless (null? x)
	(when comma? (display "," out))
	(newline out)
	(write-indent new-indent out)
	(write-any (car x) out new-indent)
	(loop #t (cdr x))))
    (newline out)
    (write-indent indent out) (display "]" out))
    
  (define (write-any x out indent)
    (cond ((vector? x) (write-ht x out indent))
	  ((list? x) (write-array x out indent))
	  ((eq? x 'null) (display "null" out))
	  ((symbol? x) (write (symbol->string x) out))
	  ((or (string? x) (number? x)) (write x out))
	  ((boolean? x) (display (if x "true" "false") out))
	  (else (error 'json-pp "Invalid JSON object" x))))

  (write-any json out 0))

)
	    
