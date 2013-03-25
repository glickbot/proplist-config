## Erlang Proplist File Editor
A command line tool to edit erlang configuration files.

### Why?

Erlang configuration files ( i.e. Riak's app.config ) are often stored as proplists ( lists of named tuples ).

These configuration files are hard to automate without converting to/from a different format and destroying comments.

### Purpose

This editor sets out to:
* Provide a command-line api to non-destructively read/modify proplist based configuration files
* Provide a method to pre-define available configuration parameters/usage info to validate config files

### Current status
* Pre-alpha - 'set', 'del', 'add' commands "work", but their implementation may change

### TODO
* validations via 'proplists' before sending to proplists_mod
* read/get commands
* proper error handling
* help functions
* tests
* config definition/usage framework
