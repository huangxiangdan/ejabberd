%%%----------------------------------------------------------------------

%%% File    : mod_offline_prowl.erl
%%% Author  : Robert George <rgeorge@midnightweb.net>
%%% Purpose : Forward offline messages to prowl
%%% Created : 31 Jul 2010 by Robert George <rgeorge@midnightweb.net>
%%%
%%%
%%% Copyright (C) 2010   Robert George
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
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_offline_prowl).
-author('rgeorge@midnightweb.net').

-behaviour(gen_mod).
-behaviour(gen_server).

-export([start/2,
	 stop/1,
	 send_notice/3]).

% supervisor
-export([start_link/2]).

% gen_server
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).

-define(PROCNAME, ejabberd_offline_prowl).

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-record(state, {vhost, address, apikey}).

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ejabberd starts module
start(VHost, Opts) ->
    ?INFO_MSG("Starting mod_offline", [] ),
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
    ?INFO_MSG("init mod_offline_prowl Host ~s", [VHost] ),
    process_flag(trap_exit, true),
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, VHost, ?MODULE, send_notice, 10),
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
    ?INFO_MSG("Stopping mod_remote_log", [] ),
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    %gen_server:call(Proc, {cleanup}),
    %timer:sleep(10000),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

%%====================================================================
%% Hooks
%%====================================================================


send_notice(From, To, P) -> 
    ?INFO_MSG("aaa ~p", [{From, To}]),
    VHost = From#jid.lserver,
    Proc = gen_mod:get_module_proc(VHost, ?PROCNAME),
    gen_server:cast(Proc, {addlog, From, To, P}).

handle_call(Msg, _From, State) ->
    ?INFO_MSG("Got call Msg: ~p, State: ~p", [Msg, State]),
    {noreply, State}.

% ejabberd_hooks call
handle_cast({addlog, From, To, Packet}, #state{apikey=APIKey, address=Address} = State) ->
    check_and_forward(From, To, Packet, APIKey, Address),
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
  ejabberd_hooks:delete(user_send_packet, VHost,
        ?MODULE, send_notice, 10).

% verifier si le trafic est local
% Modified from original version: 
%    - registered to the user_send_packet hook, to be called only once even for multicast
%    - do not support "private" message mode, and do not modify the original packet in any way
%    - we also replicate "read" notifications
check_and_forward(_From, _To, #xmlel{attrs = Attrs} = Packet, APIKey, Address)->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    FromID = element(2, _From),
    FromServer = element(3, _From),
    From = [FromID, "@", FromServer],
    Body = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
    if
    (Type /= <<"error">>) and (Type /= <<"groupchat">>)
 and (Type /= <<"headline">>) and (Body /= <<"">>) ->
      ToSB   = xml:get_tag_attr_s(<<"to">>, Packet),
      ToS = binary_to_list(ToSB),
      case ToS of
        [] ->
          ok;
        _ ->
          Sep = "&",
          Post = [
            "api_token=", APIKey, Sep,
            "message=", Body, Sep,
            "from=", From, Sep,
            "to=", string:sub_word(ToS,1,$/)],
          ?INFO_MSG("Found API Key for ~s. Will post message to ~s, Body is ~s.~n", [APIKey, Address, Post] ),
          Content = list_to_binary(Post),
          httpc:request(post, {binary_to_list(Address), [], "application/x-www-form-urlencoded", Content},[],[]),
          ok
      end;
    true ->
      ok
    end.
