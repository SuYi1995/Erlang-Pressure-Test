%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 二月 2019 17:53
%%%-------------------------------------------------------------------
-module(robot_login).
-author("suyang").

%% API
-export([test/1]).

-include("common.hrl").
-include("tester.hrl").

test(N) ->
    util_math:for(1, N, fun(Index) -> do_test(Index) end).

do_test(Index) ->
    timer:sleep(50),
    spawn(fun() -> do_test_2(Index, 1) end).

do_test_2(_Index, 0) -> ignore;
do_test_2(Index, Count) ->
    _Pid = robot:robot(Index),
%%    timer:sleep(100),
%%    test:logout(Pid),
%%    timer:sleep(100),
    do_test_2(Index, Count - 1).
