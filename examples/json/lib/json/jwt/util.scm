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
(library (json jwt util)
    (export pem->key)
    (import (rnrs)
	    (clos user)
	    (climate input)
	    (sagittarius crypto keys)
	    (sagittarius crypto pem)
	    (sagittarius crypto pkcs keys)
	    (sagittarius crypto x509))

(define (pem->key arg) (pkcs-key->key (decode-pem-argument arg)))

(define-method pkcs-key->key ((k <pkcs-one-asymmetric-key>))
  (one-asymmetric-key->private-key
   (pkcs-one-asymmetric-key->one-asymmetric-key k)))

(define-method pkcs-key->key ((k <x509-certificate>))
  (x509-certificate-public-key k))

(define-method pkcs-key->key ((k <subject-public-key-info>))
  (subject-public-key-info->public-key k))


(define (decode-pem-argument arg)
  (call-with-argument-input-port arg
   (lambda (in) (pem-object->object (read-pem-object in)))
   :transcoder (native-transcoder)))
  
)

	    
