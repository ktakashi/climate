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
(library (json jwt jws)
    (export jws-verify)
    (import (rnrs)
	    (rfc jws)
	    (rfc jwk)
	    (json jwt util)
	    (climate input))

(define (jws-verify jws :key (public-key #f) (jwks #f))
  (define jws-string
    (utf8->string (get-bytevector-all (argument->input-port jws))))
  (let* ((jws-object (jws:parse jws-string))
	 (key (if public-key
		  (load-jwk public-key)
		  (load-jwk-from-jwks jwks (jws-object-header jws-object))))
	 (verifier (public-key->jws-verifier key)))
    (if (jws:verify jws-object verifier)
	"valid"
	"invalid")))

(define (load-jwk key)
  (let-values (((loc type) (parse-attributed-argument key "jwk")))
    (case (string->symbol type)
      ((jwk) (call-with-argument-input-port loc read-jwk
	      :transcoder (native-transcoder)))
      ((pem) (key->jwk (pem->key loc)))
      (else (assertion-violation 'jws "Unknown key type" type)))))

(define (load-jwk-from-jwks jwks header)
  (error 'jws "Not yet"))
)

