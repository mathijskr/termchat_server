![Erlang CI](https://github.com/mathijskr/termchat_server/workflows/Erlang%20CI/badge.svg?branch=release)
# termchat_server

## Prerequisites:
* Erlang
* Rebar3

## Usage:
* Compile with rebar3 compile
* Start the server with rebar3 shell

## Configuration:
The configuration of the server can be modified in ebin/termchat.app.
* The number of concurrent listeners can be set by changing "nworkers"
* The port on which the server listens can be set by changing "port"
