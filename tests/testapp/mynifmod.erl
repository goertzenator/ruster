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


simple_test_() -> [
		?_assertEqual(6, times2(3)),
		?_assertEqual(self(), test_enif_make_pid())
	].



times2(_X)           -> exit(nif_library_not_loaded).
test_enif_make_pid() -> exit(nif_library_not_loaded).





dynamic_atom() -> exit(nif_library_not_loaded).
dynamic_atom_test() -> ?assertEqual(an_atom, dynamic_atom()).

prebuilt_atom() -> exit(nif_library_not_loaded).
prebuilt_atom_test() -> ?assertEqual(prebuilt_atom, prebuilt_atom()).



int()  -> exit(nif_library_not_loaded).
int_test() -> ?assertEqual(12345, int()).


doubleit(_)  -> exit(nif_library_not_loaded).
doubleit_test() ->
true = ( erlang:abs(2*3.14159 - doubleit(3.14159)) < 0.0001).

selfsend() -> exit(nif_library_not_loaded).
selfsend_test() ->
	selfsend(),
	?assertEqual(9998,
		receive
			X -> X
		after
			1000 -> false
		end).

decode_int(_) -> exit(nif_library_not_loaded).
decode_int_test() ->  ?assertEqual(28, decode_int(14)).

tuple_math(_) -> exit(nif_library_not_loaded).
tuple_math_test() ->  ?assertEqual({17,16}, tuple_math({7,8})).

param2tuple(_,_) -> exit(nif_library_not_loaded).
param2tuple_test() -> ?assertEqual({17,16}, param2tuple(7,8)).

make_resource(_) -> exit(nif_library_not_loaded).
render_resource(_,_) -> exit(nif_library_not_loaded).
mutate_resource(_,_,_) -> exit(nif_library_not_loaded).

make_resource_test() ->
	make_resource(1),
	begin
		R1 = make_resource(0),
		?assertEqual(1239, render_resource(0,R1))
	end,
	erlang:garbage_collect(self()),
	ok.



reverse_binary(_) -> exit(nif_library_not_loaded).
reverse_binary_test() ->
	Bin = <<1,2,3,4,5,6,7,8,0>>,
	?assertEqual(<<0,8,7,6,5,4,3,2,1>>, reverse_binary(Bin)),
	ok.
