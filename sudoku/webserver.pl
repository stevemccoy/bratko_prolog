:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_error)).
:- use_module(library(uri)).

:- http_handler(root(list_modules), list_modules, []).
:- http_handler(root(hello_world), say_hi, []).
:- http_handler(root(sudoku), handle_sudoku, []).

server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- consult('sudoku').


% Hello World.

say_hi(_Request) :-
	InputString = "-9-----23 ---7---8- --39----7 1-7-6---- -6--4--7- ----5-6-8 2----19-- -8---4--- 31-----5-",
%	OutputString = "Dummy Output String",
	puzzle_handler(InputString, OutputString),
    reply_html_page(title('Hello World'), [
    	h1('Hello World'),
    	p([	'This example demonstrates generating HTML ',
    		'messages from Prolog'
		]),
		p(['Input String: ', InputString]),
		p(['Output String: ', OutputString])
    ]).


% Rule Definitions.

header -->
        html(tr([th('Module'), th('File')])).

modules([]) -->	[].
modules([H|T]) --> module(H), modules(T).

module(Module) -->
        { module_property(Module, file(Path)) }, !,
        html(tr([td(Module), td(Path)])).
module(Module) -->
        html(tr([td(Module), td(-)])).


% Loaded Modules.
list_modules(_Request) :-
    findall(M, current_module(M), List),
    sort(List, Modules),
    reply_html_page(title('Loaded Prolog modules'),
	    [ h1('Loaded Prolog modules'),
	      table([ \header	    % rule-invocation
	            | \modules(Modules) % rule-invocation
	            ])
	    ]).

% Sudoku solver.
handle_sudoku(Request) :-
	http_parameters(Request,
		[ puzzle(InputString, [])
		]),
	puzzle_handler(InputString, OutputString),
    reply_html_page(title('Sudoku Solver'),
    	[	h2(InputString),
    		h2(OutputString)
		]).
