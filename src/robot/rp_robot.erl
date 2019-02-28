%%%-------------------------------------------------------------------
%%% @author suyang
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 26. 二月 2019 17:55
%%%-------------------------------------------------------------------
-module(rp_robot).
-author("suyang").

%% API
-export([login/1, heartbeat/1, myinfo/1, recordinfo/1, baginfo/1, shopbuy/1, achievereward/1, achieveinfo/1,
    signinfo/1, signtry/1, giftinfo/1, giftreward/1, rankinfo/1, wechatrunreward/1, wechatrundata/1, freediamondadreq/1]).
-export([handle/3]).

-include("pcligs.hrl").
-include("common.hrl").
-include("tester.hrl").

login(Pid) -> svr_robot:cmd(Pid, 10001, #'GameLoginReq'{}).
heartbeat(Pid) -> svr_robot:heartbeat(Pid).
myinfo(Pid) -> svr_robot:cmd(Pid, 51004, #'MyInfoReq'{}).
recordinfo(Pid) -> svr_robot:cmd(Pid, 51005, #'RecordInfoReq'{}).
baginfo(Pid) -> svr_robot:cmd(Pid, 51006, #'BagInfoReq'{}).
shopbuy(Pid) -> svr_robot:cmd(Pid, 51009, #'ShopBuyReq'{}).
achieveinfo(Pid) -> svr_robot:cmd(Pid, 51010, #'AchieveInfoReq'{}).
achievereward(Pid) -> svr_robot:cmd(Pid, 51011, #'AchieveRewardReq'{}).
signinfo(Pid) -> svr_robot:cmd(Pid, 51012, #'SignInfoReq'{}).
signtry(Pid) -> svr_robot:cmd(Pid, 51013, #'SignTryReq'{}).
giftinfo(Pid) -> svr_robot:cmd(Pid, 51014, #'GiftInfoReq'{}).
giftreward(Pid) -> svr_robot:cmd(Pid, 51015, #'GiftInfoReq'{}).
rankinfo(Pid) -> svr_robot:cmd(Pid, 51016, #'WorldRankInfoReq'{}).
wechatrundata(Pid) -> svr_robot:cmd(Pid, 51017, #'WechatRunDataReq'{}).
wechatrunreward(Pid) -> svr_robot:cmd(Pid, 51018, #'WechatRunRewardReq'{}).
freediamondadreq(Pid) -> svr_robot:cmd(Pid, 51019, #'FreeDiamondAdReq'{}).

%% @doc 登录测试
handle(10001, #'GameLoginReq'{}, #tester{id = Uid, name = Name, acc_name = Token}) ->
    ?INFO("Account login Uid:~w, Name:~w, Token:~w", [Uid, Name, Token]),
    UserInfo = [{struct, [{"nickName", Name}, {"gender", 1}, {"language", "zh_CN"}, {"city", "ShenZhen"}, {"province", "GuangDong"}, {"country", "China"}, {"avatarUrl", "https://fq-1256666226.cos.ap-guangzhou.myqcloud.com/userAvarta1.png"}]}],
    svr_robot:pack_send(10001, #'GameLoginReq'{uid = Uid, accessToken = Token, signature = "hahahahaha", userInfoRawData = json:encode(UserInfo), referrerUid = 100}),
    {ok};

handle(20001, #'GameLoginRes'{code = Code, loginType = LoginType, matchUserInfos = MatchUserInfos}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game Login Res Code:~w, LoginType:~w, MatchUserInfos:~w", [Uid, Name, Code, LoginType, MatchUserInfos]),
    case Code of
        'LG_OK' ->
            % 心跳请求
            heartbeat(Pid),
            % 玩家相关信息请求
            myinfo(Pid),
            recordinfo(Pid),
            baginfo(Pid),
            achieveinfo(Pid),
            signinfo(Pid),
            giftinfo(Pid);
        _R -> ?ERROR_MSG("Id:~w, Name[~s] Game Login Error Code:~w, LoginType:~w", [Uid, Name, Code, LoginType])
    end,
    {ok};

%% @doc 心跳测试
handle(12007, #'TimestampCorrectionReq'{}, #tester{id = _Uid, name = _Name, acc_name = _Token}) ->
%%    ?INFO("Account heartbeat Uid:~w, Name:~w, Token:~w", [Uid, Name, Token]),
    svr_robot:pack_send(12007, #'TimestampCorrectionReq'{clientTimestamp = util_time:unixtime()}),
    {ok};

handle(12008, #'TimestampCorrectionRes'{serverTimestamp = ServerTime}, #tester{id = Uid, name = Name}) ->
    ?INFO("Id:~w, Name[~s] Game Login Res ServerTime:~w", [Uid, Name, ServerTime]),
    {ok};

%% @doc 玩家相关信息请求
handle(51004, #'MyInfoReq'{}, _Test) ->
    svr_robot:pack_send(51004, #'MyInfoReq'{}),
    {ok};
handle(52004, #'MyInfoRes'{code = Code, info = Info}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game MyInfores Code:~w Info:~w", [Uid, Name, Code, Info]),
    shopbuy(Pid),
    {ok};
handle(51005, #'RecordInfoReq'{}, _Test) ->
    svr_robot:pack_send(51005, #'RecordInfoReq'{}),
    {ok};
handle(52005, #'RecordInfoRes'{code = Code, mvpCount = MvpCount, battleCount = BattleCount, winCount = WinCount, evenKillCount = EventKillCount, allKillCount = AllKillCount, loginDay = LoginDay, starNum = StarNum}, #tester{id = Uid, name = Name}) ->
    ?INFO("Id:~w, Name[~s] Game RecordInfoRes Code:~w mvpCount:~w, BattleCount:~w, WinCount:~w, EventKillCount:~w, AllKillCount:~w, LoginDay:~w, StarNum:~w", [Uid, Name, Code, MvpCount, BattleCount, WinCount, EventKillCount, AllKillCount, LoginDay, StarNum]),
    {ok};
handle(51006, #'BagInfoReq'{}, _Test) ->
    svr_robot:pack_send(51006, #'BagInfoReq'{}),
    {ok};
handle(52006, #'BagInfoRes'{code = Code, items = ItemS}, #tester{id = Uid, name = Name}) ->
    ?INFO("Id:~w, Name[~s] Game BagInfoRes Code:~w, ItemS:~w", [Uid, Name, Code, ItemS]),
    {ok};
handle(51010, #'AchieveInfoReq'{}, _Test) ->
    svr_robot:pack_send(51010, #'AchieveInfoReq'{}),
    {ok};
handle(52010, #'AchieveInfoRes'{code = Code, items = ItemS}, #tester{id = Uid, name = Name}) ->
    ?INFO("Id:~w, Name[~s] Game AchieveInfoRes Code:~w, ItemS:~w", [Uid, Name, Code, ItemS]),
    {ok};
handle(51012, #'SignInfoReq'{}, _Test) ->
    svr_robot:pack_send(51010, #'SignInfoReq'{}),
    {ok};
handle(52012, #'SignInfoRes'{code = Code, finishDays = FinishDays, isTodayFinish = IsTodayFinish}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game SignInfoRes Code:~w, FinishDays:~w, IsTodayFinish:~w", [Uid, Name, Code, FinishDays, IsTodayFinish]),
    signtry(Pid),
    {ok};
handle(51014, #'GiftInfoReq'{}, _Test) ->
    svr_robot:pack_send(51014, #'GiftInfoReq'{}),
    {ok};
handle(52014, #'GiftInfoRes'{code = Code, newbeeGiftReceived = New, inviteRecord = Inv, sceneGiftReceived = SG, secondDayGiftReceived = SDG, accountCreateTime = ACT}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game GiftInfoRes Code:~w, newbeeGiftReceived:~w, inviteRecord:~w, sceneGiftReceived:~w, secondDayGiftReceived:~w, accountCreateTime:~w", [Uid, Name, Code, New, Inv, SG, SDG, ACT]),
    giftreward(Pid),
    {ok};

%% @doc 商店相关
handle(51009, #'ShopBuyReq'{}, _Test) ->
    svr_robot:pack_send(51009, #'ShopBuyReq'{id = 5}),
    {ok};
handle(52009, #'ShopBuyRes'{code = Code, id = Id}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game ShopBuyRes Code:~w Id:~w", [Uid, Name, Code, Id]),
    baginfo(Pid),
    wechatrundata(Pid),
    {ok};

%% @doc 成就相关
handle(51011, #'AchieveRewardReq'{}, _Test) ->
    svr_robot:pack_send(51011, #'AchieveRewardReq'{id = 1}),
    svr_robot:pack_send(51011, #'AchieveRewardReq'{id = 2}),
    {ok};
handle(52011, #'AchieveRewardRes'{code = Code, id = Id}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game AchieveRewardRes Code:~w Id:~w", [Uid, Name, Code, Id]),
    achieveinfo(Pid),
    freediamondadreq(Pid),
    {ok};

%% @doc 签到相关
handle(51013, #'SignTryReq'{}, _Test) ->
    svr_robot:pack_send(51013, #'SignTryReq'{type = 1}),
    {ok};
handle(52013, #'SignTryRes'{code = Code, type = Type}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game SignTryRes Code:~w Id:~w", [Uid, Name, Code, Type]),
    signinfo(Pid),
    achievereward(Pid),
    {ok};

%% @doc 礼包相关
handle(51015, #'GiftRewardReq'{}, _Test) ->
    svr_robot:pack_send(51015, #'GiftRewardReq'{type = 1, uid = 0}),
    {ok};
handle(52015, #'GiftRewardReq'{type = Type, uid = U}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game GiftRewardReq Type:~w U:~w", [Uid, Name, Type, U]),
    giftinfo(Pid),
    rankinfo(Pid),
    {ok};

%% @doc 排行榜相关
handle(51016, #'WorldRankInfoReq'{}, _Test) ->
    svr_robot:pack_send(51016, #'WorldRankInfoReq'{mode = 0}),
    svr_robot:pack_send(51016, #'WorldRankInfoReq'{mode = 1}),
    {ok};
handle(52016, #'WorldRankInfoRes'{code = Code, mode = Mode, items = Items}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game WorldRankInfoRes Code:~w, Mode:~w, Items:~w", [Uid, Name, Code, Mode, Items]),
    giftinfo(Pid),
    rankinfo(Pid),
    {ok};

%% @doc 步数相关
handle(51017, #'WechatRunDataReq'{}, _Test) ->
    svr_robot:pack_send(51017, #'WechatRunDataReq'{encryptedData = "otsv5LJfSP4VdUnDlVwCPeqFcZKvMJ95C989qyz8Hj5M9ZG/skPLZREpTOcKY8PnxlVFKd85rYhRFfP55ZicTE1tw9rlsYCVeQEa5GFwotCV/NNJQHOWSrbqOaw3+bjLRz7IkZbn7HMCkbq20uzpFGYh5tuqbo9ul8ZUDqIyAa0s7onar6h8jnwoeBCpGNV6SKYSlz1WqngSzlG2fxMMnakLhma82zF3kd1tgB5/rvNmT1JA3GonAc5PNhz6T4gT2CTSxys2bd19KAxbQ90fmM1aAVQkTv3yIv/Jjn7/zJBLSG5yD1cfKXiZ61MiF462m3jwOAa39K924f87lExOHoIJC5RBNLDgkzmGpwtOSU48hNTbPpf0QcLdJ9Ak6sWSELo1w3fvclqLxwJfQYqykvOFe+iK7VWMSq9IJtHRXRUUo+wf+Q24omIXV297ny2c+5E4l0ZtuV18CXX6A6Z26u06E9DXyD0XZQ24AevGR60q2519v4wGhtn4TR6zoX9pqq7lKE3RTr5qub6Sz+3767qsoy6Ps3BarkCvI1Ez7Gy/zgtuDeTm3eqlzfKTIod08P25MlcO06xZ6eEr0a8x6mv1cs5+ALA17rlB8zD5vJbdc7oXKgT/QatbAJABP5d7ZUPLHeR1eJXl/as1qe0Lc9o0V0BVHM+YSsbEAfa0BYvMDm0nLBm+a1yq0AgeCV3Up+DxFTQJW29nk5BDih2vZJY0wFdXs9zsmLP3ZQpH72WDOMMnD43cz8T6zYwBPI8NfAMQixMecGaoRZParxZqh4EjjiqG8jnYlSNOpQ+5wFoxiHlky8T1MmHx9aV5nODK+eYeZSN84tRj7sIKZGUggVwennVVoy2fJYpTmWv5874VVFqmmd7v66PrFa2Azh5SfnwSiDA3iSG96c5QmHEFd/SW/ftE2jQH8qq2mmxChAxTDkRZPd/2kln1R2HHAP3vTHyYZ7lsf4/y+DFf7lLkvLP166RPYow5lKN715fAjU4UhuyquMvSQlAcgpS5pGCY+dUYhEfLTmXTZTyGCjPMEQUCioE27u2g87oZgV0sTETzQvmn40bjpd6rn8hbJyH3fzW0JL5T9j1Ht5dnoiu1E08TOZZWHjGg5it3gIMwHn95cMiVV8JIz9BlNr7PyFvR0NYgyEMSJgNr8rH9GirmMLWzn8/CvwgYeF2XUfA6Kv4AAOLLaX89NlpxCRFU7zNVsLHpWayCg4OfSxDhxFVAbDP4BLzyR41RsiyImRsrQGclayrtxPpLkhAB+cFmbV6/s+8fcFvXicUjW1ldD0TOZx8kfQr8+GKKUG4X/xva9/YHTD4ME6CnM7eYh7U89TVtMHGIuaGveWKaZSx3dM0MeyhaZlKHQObWNJCOHjNiU5hRc0I/46vFCdm5/G8gFmnVxPMMgUJKri1+x8Ix7LmXwkNgzU11XjxZp3YWWiC4aAY3LZA7+6le6O++GfDJrg5rGr9KqgCUfm3yaP0VprVrJE2j5IpYREpWV8vofEel8dtE/O0LHJ3SQLlDo17Q0/FRTxQwvVatN0VEEdqvOnSG2tzAOw+X0LQf0rHct8Sw5EwibPuqcwzK6QvcH36MJ5SkH/Er9ycvtqMR2bKsoEyUvBnx6zhq2Wt8lBSFDzQ+AV4=", iv = "TWKIMVuF5q+dcwr8T3z3vg=="}),
    {ok};
handle(52017, #'WechatRunDataRes'{code = Code, walkStep = WalkStep, rewardStep = RewardStep}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game WechatRunDataRes Code:~w, WalkStep:~w, RewardStep:~w", [Uid, Name, Code, WalkStep, RewardStep]),
    wechatrunreward(Pid),
    {ok};
handle(51018, #'WechatRunRewardReq'{}, _Test) ->
    svr_robot:pack_send(51018, #'WechatRunRewardReq'{isDouble = true}),
    {ok};
handle(52018, #'WechatRunRewardRes'{code = Code, rewardStep = RewardStep, rewardDiamond = RewardDiamond}, #tester{id = Uid, name = Name}) ->
    ?INFO("Id:~w, Name[~s] Game WechatRunRewardRes Code:~w, RewardStep:~w, RewardDiamond:~w", [Uid, Name, Code, RewardStep, RewardDiamond]),
    {ok};

%% @doc 免费钻石
handle(51019, #'FreeDiamondAdReq'{}, _Test) ->
    svr_robot:pack_send(51019, #'FreeDiamondAdReq'{}),
    {ok};
handle(52019, #'FreeDiamondAdRes'{code = Code}, #tester{id = Uid, name = Name, pid = Pid}) ->
    ?INFO("Id:~w, Name[~s] Game FreeDiamondAdRes Code:~w", [Uid, Name, Code]),
    myinfo(Pid),
    {ok};

%% 容错处理
handle(_Cmd, _Data, #tester{name = _Name}) ->
    {ok}.
