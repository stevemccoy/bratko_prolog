:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).

:- http_handler(root(sudoku), handle_request, [_Arg]).

:- consult('sudoku').

server(Port) :-
	http_server(http_dispatch, [port(Port)]).

handle_request(Request) :-

	format('Content-type: text/plain~n~n'),
	format('Hello World!~n'),
	writeln(Request),

	http_parameters(Request,
		[ input(InputString, [])
		]),
	puzzle_handler(InputString, OutputString),
    reply_html_page(title('Sudoku Solver'),
    	[	h2('Input Pattern  : ' - InputString),
    		h2('Output Pattern : ' - OutputString)
		]).
