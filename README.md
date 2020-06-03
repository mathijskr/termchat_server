# termchat_server

## Prerequisites:
* An installation of the erlang interpreter

## Usage:
* Compile with erl -make
* Startup erl
* Install the database with termchat:install()
* Start the server:
    * application:start(mnesia)
    * application:start(termchat)

## Configuration:
The configuration of the server can be modified in ebin/termchat.app.
* The number of concurrent listeners can be set by changing "nworkers"
* The port on which te server listen can be set by changing "port"
