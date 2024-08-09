CLImate - a CLI generating library
==================================

CLImate (the same pronunciation as climate) is a CLI generating library.
Provising CLI definitions, then the library handles argument check,
generates usage message.

Examples
--------

- [JSON CLI](./examples/json/json-cli)


APIs
----

### Library `(climate)`

##### *Syntax* `climate` _name_ _command_ _..._
##### *Auxiliary syntax* `group` _command_ _..._
##### *Auxiliary syntax* `arguments` _(required _..._)_ _option_ _..._
##### *Auxiliary syntax* `options` _option_ _..._

Builds `climate` object. The syntax looks like this

```scheme
(climate $name
  ($command "usage"
    (group ($sub-command0 "usage of $sub-command0" $processor0
             (arguments 
               ((arg0 "arg0 description")
                 arg1)  ;; description can be omit
               ;; keyword (short long) argument-required default-value usage
               (:opt0 (#\o "option0") #t #f "opt0 descrition")
               (:opt1 (#\O "option1") #f #f "opt1 descrition")))
            ($sub-command1 "usage of $sub-command1" $processor1
             (options 
               (:opt0 (#\o "option0") #t #f "opt0 descrition")
               (:opt1 (#\O "option1") #f #f "opt1 descrition"))))))
```

This generates below CLI commands

- `$name $sub-command0 {arg0} {arg1} [-o {opt0}] [-O {opt1}]`
- `$name $sub-command1 [-o {opt0}] [-O {opt1}]`

The `$processor` must be a procesure which accepts the arguments described
in the CLI specification. For example, for the `$sub-command0`, then it must
be like this

```scheme
(define (processor0 arg0 arg1 :key opt0 opt1) ...)
```


##### *Procedure* `climate?` _obj_
Returns `#t` if the given _obj_ is climate object, otherwise `#f`.

##### *Procedure* `execute-climate` _climate_ _args_
Executes the given _climate_ with the given _args_

The _args_ must be a list of strings represents the command line argument

```scheme
(define example-cli (climate ...))
;; using main
(define (main args)
  (let ((result (execute-climate example-cli (cdr args))))
    (display (result-value result)) (newline)
	(exit (if (result-success? result) 0 1))))
```

##### *Procedure* `result?` _obj_
Returns `#t` if the given _obj_ is climate result object, otherwise `#f`.

##### *Procedure* `result-success?` _result_
Returns `#t` if the execution is successful otherwise `#f`.

##### *Procedure* `result-value` _result_
Retrieves the result value. When the execution failed due to an unexpected
error, then usage message is returned.


Executor APIs
-------------

The below APIs are also exported from `(climate)`

### Library `(climate input)`
Input parsing utilties library. 

##### *Procedure* `argument->input-port` _arg_
The _arg_ must be a string.

Converts the given _arg_ into a binary input port. This procedure
applies some conversion rules if _arg_ is;

1. A string `-`, then `(standard-input-port)` is returned
2. A string starts with `@`, then reading from a file.
   e.g. If _arg_ is `@input.txt`, then `input.txt` is read
3. A string doesn't apply the above, then 
   `(open-bytevector-input-port (string->utf8 _arg_)` is applied.

##### *Procedure* `argument->string-content` _arg_
Converts given _arg_ to string applying the rule of `argument->input-port`.

##### *Procedure* `call-with-argument-input-port` _arg_ _proc_ :key (_transcoder_ `#f`)
Applying given _proc_ with the result of `argument->input-port`. If the
keyword argument _transcoder_ is specified, then _proc_ receives a
transcoded port.

##### *Procedure* `parse-attributed-argument` _arg_ :optional (_default_ `#f`)
Split the given _arg_ with `|` and returns 2 values. If the _arg_ doesn't
contain `|`, then _arg_ and _default_ are returned.

If _arg_ is `"foo|bar"`, then the return values are `"foo"` and `"bar"`.  
If _arg_ is `"foo"`, then the return values are `"foo"` and `#f`.  
If _arg_ is `"foo"` and _default_ is `"buz"`, then return values are
`"foo"` and `"buz"`
