-module(test_nif).

-compile(export_all).

-on_load(init/0).

-include_lib("eunit/include/eunit.hrl").

init() ->
	{ok, Lib} = find_crate:find_library(test_app, "test_nif", "test_nif"),
    ok = erlang:load_nif(Lib, 0).

atom() -> exit(nif_library_not_loaded).
int()  -> exit(nif_library_not_loaded).


atom_test() -> ?assertEqual(an_atom, atom()).
int_test()  -> ?assertEqual(12345, int()).
