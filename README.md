# termchat_server

Prerequisites:
* An installation of the erlang interpreter

Usage:
* Compile with erl -make
* Startup erl
* Install the database with termchat:install()
* Start the server:
    * application:start(mnesia)
    * application:start(termchat)
