%%%----------------------------------------------------------------------

%%% File    : mod_remote_log.erl
%%% Author  : Stoned <xd_huang1986@163.com>
%%% Purpose : Forward messages to log server
%%% Created : 25 May 2014 by Stoned <xd_huang1986@163.com>
%%%
%%%
%%% Copyright (C) 2014   Stoned
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%%
%%%----------------------------------------------------------------------

-module(mod_block_log).
-author('xd_huang1986@163.com').

-behaviour(gen_server).
-behaviour(gen_mod).

-export([start/2,
   stop/1,
   save_blocklist_event/3]).

% supervisor
-export([start_link/2]).

% gen_server
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-define(PROCNAME, mod_block_log).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(state, {vhost, address, apikey}).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ejabberd starts module
start(VHost, Opts) ->
    ?INFO_MSG("Starting mod_block_log", [] ),
    ChildSpec =
        {gen_mod:get_module_proc(VHost, ?PROCNAME),
         {?MODULE, start_link, [VHost, Opts]},
         permanent,
         1000,
         worker,
         [?MODULE]},
    % add child to ejabberd_sup
    supervisor:start_child(ejabberd_sup, ChildSpec).

% supervisor starts gen_server
start_link(VHost, Opts) ->
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    {ok, Pid} = gen_server:start_link({local, Proc}, ?MODULE, [VHost, Opts], []),
    Pid ! start,
    {ok, Pid}.

init([VHost, _Opts]) ->
    ?INFO_MSG("init mod_block_log Host ~s", [VHost] ),
    process_flag(trap_exit, true),
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(blocklist_event_hook, VHost, ?MODULE, save_blocklist_event, 90),
    APIKeys = gen_mod:get_module_opt(VHost, ?MODULE, apikeys, fun(A) -> A end, [] ),
    Addresses = gen_mod:get_module_opt(VHost, ?MODULE, addresses, fun(A) -> A end, [] ),
    AddressArray = lists:keyfind(VHost, 1, Addresses),
    ?INFO_MSG("AddressArray ~p", [AddressArray] ),
    Address = element(2,AddressArray),
    APIKey = lists:keyfind(VHost, 1, APIKeys),
    APIToken = element(2,APIKey),
    ?INFO_MSG("Found API Key for ~s. Will post message to ~s.~n", [element(1,APIKey), Address] ),

    {ok, #state{vhost=VHost,
                address=Address,
                apikey=APIToken
                }}.

stop(VHost) ->
    ?INFO_MSG("Stopping mod_block_log", [] ),
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    %gen_server:call(Proc, {cleanup}),
    %timer:sleep(10000),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% Hooks
%%====================================================================


save_blocklist_event(User, Server, Event) -> 
    ?INFO_MSG("save_blocklist_event params ~p", [{User, Server, Event}]),
    Proc = gen_mod:get_module_proc(Server, ?PROCNAME),
    gen_server:cast(Proc, {addlog, User, Event}).

handle_call(Msg, _From, State) ->
    ?INFO_MSG("Got call Msg: ~p, State: ~p", [Msg, State]),
    {noreply, State}.

% ejabberd_hooks call
handle_cast({addlog, User, Event}, #state{apikey=APIKey, address=Address} = State) ->
    check_and_forward(User, Event, APIKey, Address),
    {noreply, State};

handle_cast(Msg, State) ->
    ?INFO_MSG("Got cast Msg:~p, State:~p", [Msg, State]),
    {noreply, State}.

handle_info(Info, State) ->
    ?INFO_MSG("Got Info:~p, State:~p", [Info, State]),
    {noreply, State}.

terminate(Reason, State) ->
    ?INFO_MSG("Reason: ~p", [Reason]),
    cleanup(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

cleanup(#state{vhost=VHost} = _State) ->
  ?INFO_MSG("Stopping ~s for ~p", [?MODULE, VHost]),
  ejabberd_hooks:delete(blocklist_event_hook, VHost,
        ?MODULE, save_blocklist_event, 90).

% verifier si le trafic est local
% Modified from original version: 
%    - registered to the user_send_packet hook, to be called only once even for multicast
%    - do not support "private" message mode, and do not modify the original packet in any way
%    - we also replicate "read" notifications
check_and_forward(User, unblock_all, APIKey, Address)->
  check_and_forward(User, {unblock_all, []}, APIKey, Address);  

check_and_forward(User, {Action, Params}, APIKey, Address)->
  Sep = "&",
  [{BlockUser, _, _}| _] = Params,
  Post = [
    "api_token=", APIKey, Sep,
    "user=", User, Sep,
    "block_action=", atom_to_list(Action), Sep,
    "block_user=", BlockUser],
    % "params[]=", string:join(Params, ",")],
  ?INFO_MSG("Found API Key for ~s. Will post message to ~s, Body is ~s.~n", [APIKey, Address, Post] ),
  Content = list_to_binary(Post),
  httpc:request(post, {binary_to_list(Address), [], "application/x-www-form-urlencoded", Content},[],[]);
            
check_and_forward(_User, _, _, _)-> ok.

