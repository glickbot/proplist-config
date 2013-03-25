## Erlang Proplist File Editor
A command line tool to edit erlang configuration files.

#### Why?

Erlang configuration files ( i.e. Riak's app.config ) are often stored as proplists ( lists of named tuples ).

These configuration files are hard to automate without converting to/from a different format and destroying comments.

#### Purpose

This editor sets out to:
* Provide a command-line api to non-destructively read/modify proplist based configuration files
* Provide a method to pre-define available configuration parameters/usage info to validate config files

### API

Set pb_port to 5 in Riak's app.config:
	config set riak_api.pb_port 5

Delete the pb_port tuple in Riak's app.config:
	config del riak_api.pb_port

Add pb_port back to Riak's app.config:
	config add riak_api.pb_port 5

#### Current status
* Pre-alpha - 'set', 'del', 'add' commands "work", but their implementation may change

#### NOTES
* *Doesn't change the file inline yet, adds <filename>.new instead. It'll change the file inline when it's out of alpha
* Doesn't return anything useful when it's successful.
* Pukes in erlang when something goes wrong.

### Installation:
Get the code:
``` bash
$ git clone git://github.com/glickbot/proplist-config.git
```
Make the code:
``` bash
cd proplist-config
make
```
Use the code:
``` bash
bin/config set riak_api.pb_port 42 -f /etc/riak/app.config
```

### TODO
* validations via 'proplists' before sending to proplists_mod
* read/get commands
* proper error handling
* help functions
* tests
* config definition/usage framework
