-module(mynifmod).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-on_load(init/0).

init() ->
    {ok, Lib} = find_library("mynifmod", "mynifmod"),
    ok = erlang:load_nif(Lib, 0).

%% butchered version of https://github.com/goertzenator/find_crate/blob/master/src/find_crate.erl
find_library(CrateName, LibName) ->
	Wildcard = "priv/crates/" ++ CrateName ++ "/{lib,}" ++ LibName ++ ".{so,dll}",
	case filelib:wildcard(Wildcard) of
		[Lib] -> {ok, filename:rootname(Lib)};
		[] -> {error, not_found};
		_ -> {error, multiple_matches}
	end.

int()           -> exit(nif_library_not_loaded).
add_ints1(_,_)  -> exit(nif_library_not_loaded).
tuple1(_)       -> exit(nif_library_not_loaded).
tuple2(_,_,_,_) -> exit(nif_library_not_loaded).
tuple0(_,_)     -> exit(nif_library_not_loaded).
catbin(_,_)     -> exit(nif_library_not_loaded).
staticatom()    -> exit(nif_library_not_loaded).
mathcommand(_,_,_) -> exit(nif_library_not_loaded).
makeresources(_,_) -> exit(nif_library_not_loaded).
incresources(_,_,_)  -> exit(nif_library_not_loaded).
getresources(_,_)    -> exit(nif_library_not_loaded).

tests1_test_() -> [
	?_assertEqual(12345, int()),
	?_assertEqual(12, add_ints1(5,7)),
	?_assertEqual({4,1,2,3}, tuple1({1,2,3,4})),
	?_assertEqual({2,3,4,1}, tuple2(1,2,3,4)),
	?_assertEqual({{},{},{}}, tuple0({},{})),
	?_assertEqual(<<"hellokitty">>, catbin(<<"hello">>,<<"kitty">>)),
	?_assertEqual(ok, staticatom()),
	?_assertEqual({ok, 9}, mathcommand(add, 4, 5)),
	?_assertEqual({ok, -1},mathcommand(sub, 4, 5)),
	?_assertEqual({ok, 20}, mathcommand(mul, 4, 5)),
	?_assertEqual({ok, 5}, mathcommand('div', 10, 2)),
	?_assertEqual({error, division_by_zero}, mathcommand('div', 10, 0)),
	?_assertEqual({81,103}, test_resources(77,99)),
	[]
	].

test_resources(X,Y) ->
	{R1,R2} = makeresources(X,Y),
	ok = incresources(R1,R2,4),
	getresources(R1,R2).
