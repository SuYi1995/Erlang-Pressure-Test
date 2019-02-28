%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 机器人测试
%%% @end
%%% Created : 26. 二月 2019 16:35
%%%-------------------------------------------------------------------
-module(robot).
-author("suyang").

%% API
-export([login/3,
    login/5,
    logout/1,
    logout_by_acc/1,
    display/1,
    display/0,
    pack_send/3,
    login/2,
    get_player_info/1,
    awich_list/1,
    robot/1]).

-include("common.hrl").
-include("tester.hrl").
-include("pcligs.hrl").

%% @spec login(Account, Name) -> pid()
%% Account = string()
%% 登录帐号
login(Account, Name, Index) ->
    login(Account, Name, Index, "cloud.fanqietech.cn", 8000).
login(Account, Name, Index, Host, Port) ->
    login(Account, Name, Index, undefined, Host, Port).
login(_Acc, Name, Index, _ProtoMod, _H, _P) ->
    List = get_player_info(Index),
    {Uid, Url, Token} = awich_list(List),
%%    ?INFO("============Uid:~p", [Uid]),
%%    websocket_client:start_link(erlang:binary_to_list(Url), ?MODULE, []).
    case svr_robot:start(Uid, Name, Token, erlang:binary_to_list(Url)) of
        {ok, Pid} ->
            ?INFO("===========Pid:~w", [Pid]),
            rp_robot:login(Pid),
            Pid;
        Else -> Else
    end.

robot(Index) ->
    List = get_player_info(Index),
    {Uid, Url, Token} = awich_list(List),
    Name = lists:concat(["robot", "_", util_type:to_list(Index)]),
    case svr_robot:start(Uid, Name, Token, erlang:binary_to_list(Url)) of
        {ok, Pid} ->
            rp_robot:login(Pid),
            Pid;
        Else -> Else
    end.

%% @spec logout(Pid) ->
%% Pid = pid()
%% 退出登录
logout(Pid) ->
    svr_robot:stop(Pid).

logout_by_acc(Acc) ->
    case ets:match_object(tester_online, #tester{acc_name = Acc, _ = '_'}) of
        [] -> ok;
        [#tester{pid = Pid} | _] -> logout(Pid)
    end.


display() ->
    display(ets:tab2list(tester_online)).
display(Acc) when is_binary(Acc) ->
    case ets:match_object(tester_online, #tester{acc_name = Acc, _ = '_'}) of
        [] -> ok;
        T -> display(T)
    end;
display([]) -> ok;
display([H | T]) ->
    ?INFO("~w~n", [?record_kv(H, tester)]),
    display(T).

%% @spec pack_send(Pid, Cmd, Data) ->
%% Pid = pid()
%% Cmd = integer()
%% Data = term()
%% 发送协议
pack_send(Pid, Cmd, Data) ->
    svr_robot:pack_send(Pid, Cmd, Data).

get_player_info(Index) ->
%%    case request_get("https://cloud.hiveinfotechs.com:8000/templogin", [{wxcode, Index}]) of
    case request_get("https://cloud.fanqietech.cn:8000/templogin", [{wxcode, Index}]) of
        {ok, Reply} ->
            case json:decode(Reply) of
                {struct, List} -> List;
                _R -> {error, _R}
            end;
        _R -> {error, _R}
    end.


%% @doc 请求网页数据
request_get(Url, Arguments) ->
    case do_request_get(Url, Arguments) of
        {ok, {_, _, Rs}} ->
            {ok, Rs};
        _Error -> {error, false}
    end.
%% @spec request_get(Url,Arguments) -> {ok,{_,_,Rs}} | {error,Error}
do_request_get(Url, Arguments) ->
    Url2 =
        case Arguments of
            [] ->
                Url;
            _ ->
                Arguments2 = lists:map(fun({K, V}) -> lists:concat([K, "=", V]) end, Arguments),
                Arguments3 = util_type:list_to_string(Arguments2, [], "&", []),
                case lists:member($?, Url) of
                    true ->
                        Url ++ "&" ++ Arguments3;
                    false ->
                        Url ++ "?" ++ Arguments3
                end
        end,
    httpc:request(get, {Url2, []}, [{timeout, 3000}], []).

awich_list(List) ->
    Uid =
    case lists:keyfind(<<"uid">>, 1, List) of
        {_, Val} -> Val;
        false -> 0
    end,
    Gsip =
    case lists:keyfind(<<"gsip">>, 1, List) of
        {_, V} -> V;
        false -> ""
    end,
    Token =
        case lists:keyfind(<<"token">>, 1, List) of
            {_, Vn} -> Vn;
            false -> ""
        end,
    {Uid, Gsip, Token}.

login(Uid, Pid) ->
    UserInfo = [{struct, [{"nickName", "asdda"}, {"gender", 1}, {"language", "zh_CN"}, {"city", "ShenZhen"}, {"province", "GuangDong"}, {"country", "China"}, {"avatarUrl", "https://fq-1256666226.cos.ap-guangzhou.myqcloud.com/userAvarta1.png"}]}],
    Msg = lib_proto:pack(10001, #'GameLoginReq'{uid = Uid, accessToken = "temp", signature = "hahahahaha", referrerUid = 879, userInfoRawData = json:encode(UserInfo)}),
    send(Pid, Msg).

send(Pid, Msg) ->
    websocket_client:cast(Pid, {binary, Msg}).