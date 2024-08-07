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

*Syntax* `climate` _name_ _command_ _..._   
*Auxiliary syntax* `group` _command_ _..._   
*Auxiliary syntax* `arguments` _(required _..._)_ _option_ _..._   
*Auxiliary syntax* `options` _option_ _..._   

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


