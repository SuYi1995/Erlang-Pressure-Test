%%%-------------------------------------------------------------------
%%% @author Su Yang
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协议处理映射表
%%% @end
%%% Created : 07. 十二月 2018 21:29
%%%-------------------------------------------------------------------
-module(mapping).
-author("Su Yang").

%% API
-export([module/2]).

%% @spec module(Type, Cmd) -> {ok, Type, Caller, Proto, ModName} | {ok, Proto, ModName} | {error, Reason}
%% Type = atom()
%% Cmd = int()
%% NeedAuth = bool()
%% Caller = connector | object
%% Proto = atom()
%% ModName = atom()
%% @doc 模块映射信息
module(Type, Cmd) -> code(Type, Cmd).

%% @doc
code(game_server, _) -> {ok, false, connector, pcligs, rp_robot};

code(Type, Code) -> {error, {Type, Code}}.

