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
(library (json jwt)
    (export jwt-command)
    (import (rnrs)
	    (climate)
	    (json jwt jws))

(define jws-verify-command
  (climate:command verify "Verify JWS signature" jws-verify
   (arguments 
    ((jws "JWS string"))
    (:public-key (#\p "public-key") #t #f "Public key to verify. Can be PEM or JWK format")
    (:jwks (#\u "jwks-uri") #t #f "JWKS location"))))

(define jws-command
  (climate:command jws "JWS command" 
   (group
    (prefab jws-verify-command)
    (show "Decode and print JWS" jws-show
	  (arguments
	   ((jws "JWS string"))
	   (:pretty (#\p "pretty") #f #f "Pretty print")
	   (:no-header (#\H "no-header") #f #f "Only payload")
	   (:json (#\j "json") #f #f "Decode payload as JSON"))))))

(define jwt-command
  (climate:command jwt "JWT command"
   (group
    (prefab jws-command)
    (jwe "JWE command")
    (jwk "JWK command"))))
)
	    
