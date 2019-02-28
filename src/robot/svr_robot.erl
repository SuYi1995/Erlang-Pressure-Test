%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 二月 2019 16:39
%%%-------------------------------------------------------------------
-module(svr_robot).
-author("suyang").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-export([start/4,
    stop/1,
    stop_all/0,
    cmd/3,
    pack_send/3,
    pack_send/2,
    all_tester/0,
    heartbeat/1
]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("tester.hrl").
-include("common.hrl").
-include("pcligs.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @spec start(Uid, Name, Token, Url) -> ok
%% @doc 创建测试进程
start(Uid, Name, Token, Url) ->
    case http_uri:parse(Url, [{scheme_defaults, [{ws,80}, {wss,443}]}]) of
        {ok, {Protocol, _, Host, Port, Path, Query}} ->
            gen_server:start(?MODULE, [Uid, Name, Token, Protocol, Host, Port, Path ++ Query], []);
        {error, _} = Error -> Error
    end.

%% @spec stop(Pid) -> ok
%% Pid = pid() | integer()
%% @doc 退出
stop(Rid) when is_integer(Rid) ->
    case ets:lookup(tester_online, Rid) of
        [T] ->
            stop(T#tester.pid);
        _ ->
            ignore
    end;
stop(Pid) ->
    Pid ! logout.

stop_all() ->
    [stop(Pid) || #tester{pid = Pid} <- ets:tab2list(tester_online)].

cmd(Pid, Cmd, Data) ->
    Pid ! {cmd, Cmd, Data}.

heartbeat(Pid) ->
    Pid ! heartbeat.

%% @spec pack_send(Cmd, Data)-> ok
%% Cmd = integer()
%% Data = tuple()
%% @doc 打包并发送消息到服务器
pack_send(Pid, Cmd, Data) ->
    Pid ! {pack_send, Cmd, Data}.

%% 返回所有的模拟客户端
all_tester() ->
    ets:tab2list(tester_online).


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Uid, Name, Token, _Protocol, Host, Port, Path]) ->
    case ssl:connect(Host, Port, [
        {mode, binary},
        {packet, 0},
        {active, false},
        {verify, verify_none}
    ], 6000) of
        {ok, Socket} ->
            Key = generate_ws_key(),
            HeanBin = [
                "GET ", Path, " HTTP/1.1\r\n"
                "Host: ", Host, "\r\n"
                "Connection: Upgrade\r\n"
                "Sec-WebSocket-Version: 13\r\n"
                "Sec-WebSocket-Key: ", Key, "\r\n"
                "Upgrade: websocket\r\n",
                "\r\n"
            ],
            ssl:send(Socket, HeanBin),
            {ok, HandshakeResponse} = receive_handshake(<<>>, ssl, Socket),
            validate_handshake(HandshakeResponse, Key),
            ssl:setopts(Socket, [{active, once}]),
            State = #tester{id = Uid, acc_name = binary_to_list(Token), name = Name, socket = Socket, pid = self(), connect_time = util_time:get_now()},
            ets:insert(tester_online, State),
            put(socket, Socket),
            put(is_robot, true),
            self() ! read_next,
%%            self() ! heartbeat,
            {ok, State};
        _R ->
            {stop, normal}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(read_next, State) ->
    read_next(State);
%% 发送数据
handle_info({pack_send, Cmd, Data}, State) ->
    pack_send(Cmd, Data),
    {noreply, State};
handle_info({cmd, Cmd, Data}, State) ->
    Mod = get_tester_mod(Cmd, State),
    handle(Mod, Cmd, Data, State);
%% 服务端断开了连接
handle_info({ssl_closed, _Socket}, State) ->
    ?DEBUG("socket close: ~w", [_Socket]),
    {stop, normal, State};
%% 收到正常数据
handle_info({ssl, _Socket, Data}, State = #tester{name = _Name, read_bin = true}) ->
    handle_data(Data, State);
%% 接收socket数据时发生了未预料的错误
handle_info({ssl, _Socket, {error, _Reason}}, State = #tester{acc_name = Account}) ->
    ?ERR("account[~s]read socket data err:~w", [Account, _Reason]),
    {stop, normal, State};
%% 退出
handle_info(logout, State) ->
    {stop, normal, State};
%% 心跳包
handle_info(heartbeat, State) ->
    rp_robot:handle(12007, #'TimestampCorrectionReq'{}, State),
    erlang:send_after(20000, self(), heartbeat),
    {noreply, State};
handle_info(_Info, State) ->
    ?DEBUG("received unknown message: ~w, State:~w", [_Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State = #tester{socket = Socket}) ->
    ?INFO("~s pid logout", [State#tester.acc_name]),
    catch (ets:delete(tester_online, State#tester.pid)),
    ssl:close(Socket),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_cmd(Cmd, Bin, State) ->
    case mapping:module(game_server, Cmd) of
        {ok, _, _, Proto, _Mod} ->
            MsgName = get_name(Cmd),
            Data = Proto:decode_msg(Bin, MsgName),
            Mod = get_tester_mod(Cmd, State),
            handle(Mod, Cmd, Data, State);
        _Other ->
            ?ERR("unknow command: ~w", [Cmd]),
            read_next(State)
    end.

get_name(20001) -> 'GameLoginRes';
%%get_name(52001) -> 'GameLoginRes';
%%get_name(52002) -> 'GameLoginRes';
%%get_name(52003) -> 'GameLoginRes';
get_name(52004) -> 'MyInfoRes';
get_name(52005) -> 'RecordInfoRes';
get_name(52006) -> 'BagInfoRes';
%%get_name(52007) -> 'GameLoginRes';
%%get_name(52008) -> 'GameLoginRes';
get_name(52009) -> 'ShopBuyRes';
get_name(52010) -> 'AchieveInfoRes';
get_name(52011) -> 'AchieveRewardRes';
get_name(52012) -> 'SignInfoRes';
get_name(52013) -> 'SignTryRes';
get_name(52014) -> 'GiftInfoRes';
get_name(52015) -> 'GiftInfoRes';
get_name(52016) -> 'WorldRankInfoRes';
get_name(52017) -> 'WechatRunDataRes';
get_name(52018) -> 'WechatRunRewardRes';
get_name(52019) -> 'FreeDiamondAdRes';
get_name(_) -> undifiend.

%% 获取tester模块
get_tester_mod(_, #tester{test_mod = _TestMod}) ->
    rp_robot.

handle(undefined, _Cmd, _Data, State = #tester{acc_name = _Account}) ->
    read_next(State#tester{read_bin = false});
handle(Mod, Cmd, Data, State = #tester{acc_name = Account, name = Name}) ->
    case catch Mod:handle(Cmd, Data, State) of
        {ok} ->
            read_next(State#tester{read_bin = false});
        {ok, NewState} when is_record(NewState, tester) ->
            ets:insert(tester_online, NewState),
            read_next(NewState#tester{read_bin = false});
        {stop} ->
            {stop, normal, State#tester{read_bin = false}};
        {error, _Reason} ->
            ?ERR("handle command err[Account:~s Name:~s Mod:~w Cmd:~w Why:~w]: ~w", [Account, Name, Mod, Cmd, _Reason, Data]),
            read_next(State#tester{read_bin = false});
        _Reason ->
            ?ERR("handle command unknow err[Account:~s  Name:~s Mod:~w Cmd:~w]: ~w", [Account, Name, Mod, Cmd, _Reason]),
            read_next(State#tester{read_bin = false})
    end.

%% 通知连接器读取下一条指令
read_next(State = #tester{socket = Socket, read_bin = false}) ->
%%    case ssl:recv(Socket, 0, 6000) of
%%        {ok, Data} ->
%%            handle_data(Data, State#tester{read_bin = true});
%%        {error, Reason} ->
%%            ?ERR("=============>Reason:~w", [Reason]);
%%        _ERR ->
%%            ?ERR("=============>_ERR:~w", [_ERR])
%%    end,
    ssl:setopts(Socket, [{active, once}]),
    {noreply, State#tester{read_bin = true}};
%%    {noreply, State#tester{read_bin = true}};
read_next(State) ->
    %% 上一个数据包还未读取完成，忽略掉
    {noreply, State}.

pack_send(Cmd, Data) ->
    case get(is_robot) of
        true ->
            Socket = get(socket),
            case lib_proto:pack(Cmd, Data) of
                {ok, Bin} ->
                    ssl:send(Socket, Bin);
                {error, Reason} -> ?ERR("pack data err[Reason:~w]", [Reason]);
                _R -> ?INFO("==============_R:~w", [_R])
            end;
        _ -> ?ERR("not robot process", [])
    end.

%% @doc 数据处理
handle_data(BinData, State) ->
    case lib_proto:decode_data(BinData) of
        {incomplete} -> read_next(State#tester{read_bin = false});
        {_, UnmaskedData, <<>>} ->
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            case L =< 0 of
                false -> handle_cmd(Cmd, Data, State);
                true -> handle_cmd(Cmd, <<>>, State)
            end;
        {_, UnmaskedData, Extra} ->
            {L, Cmd, Data} = lib_proto:read_bin(UnmaskedData),
            {noreply, StateN} =
                case L =< 0 of
                    false -> handle_cmd(Cmd, Data, State);
                    true -> handle_cmd(Cmd, <<>>, State)
                end,
            handle_data(Extra, StateN);
        {unknown, _OpCode, _D, _Extra} ->
            ?INFO("=============_OpCode:~w, _D:~w, _Extra:~w", [_OpCode, _D, _Extra]),
            read_next(State#tester{read_bin = false})
    end.

%% @doc 生成key
generate_ws_key() ->
    base64:encode(crypto:strong_rand_bytes(16)).

receive_handshake(Buffer, Transport, Socket) ->
    case re:run(Buffer, "\\r\\n\\r\\n") of
        {match, _} ->
            {ok, Buffer};
        _ ->
            {ok, Data} = Transport:recv(Socket, 0, 6000),
            receive_handshake(<< Buffer/binary, Data/binary >>,
                Transport, Socket)
    end.

validate_handshake(HandshakeResponse, Key) ->
    Challenge = base64:encode(
        crypto:hash(sha, << Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11" >>)),
    %% Consume the response...
    {ok, Status, Header, Buffer} = consume_response(HandshakeResponse),
    {_Version, Code, Message} = Status,
    case Code of
        % 101 means Switching Protocol
        101 ->
            %% ...and make sure the challenge is valid.
            Challenge = proplists:get_value(<<"Sec-Websocket-Accept">>, Header),
            {ok, Buffer};
        _ -> {error, {Code, Message}}
    end.

consume_response(Response) ->
    {ok, {http_response, Version, Code, Message}, Header} = erlang:decode_packet(http_bin, Response, []),
    consume_response({Version, Code, Message}, Header, []).

consume_response(Status, Response, HeaderAcc) ->
    case erlang:decode_packet(httph_bin, Response, []) of
        {ok, {http_header, _Length, Field, _Reserved, Value}, Rest} ->
            consume_response(Status, Rest, [{Field, Value} | HeaderAcc]);

        {ok, http_eoh, Body} ->
            {ok, Status, HeaderAcc, Body}
    end.