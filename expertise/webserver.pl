:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(hello_world), say_hi, [_Arg]).

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

say_hi(Request) :-
	format('Content-type: text/plain~n~n'),
	format('Hello World!~n'),
	writeln(Request),
	member(path_info(Args), Request),
	writeln(Args).
