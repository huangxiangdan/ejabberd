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

-export([start/2,
	 init/2,
	 stop/1,
	 send_notice/3]).

-define(PROCNAME, ?MODULE).

-include("ejabberd.hrl").
-include("jlib.hrl").

start(Host, Opts) ->
    ?INFO_MSG("Starting mod_offline_prowl", [] ),
    register(?PROCNAME,spawn(?MODULE, init, [Host, Opts])),  
    ok.

init(Host, _Opts) ->
    inets:start(),
    ssl:start(),
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, send_notice, 10),
    ok.

stop(Host) ->
    ?INFO_MSG("Stopping mod_offline_prowl", [] ),
    ejabberd_hooks:delete(offline_message_hook, Host,
			  ?MODULE, send_notice, 10),
    ok.

send_notice(_From, To, Packet) ->
    Type = xml:get_tag_attr_s("type", Packet),
    % FromS = xml:get_tag_attr_s("from", Packet),
    FromID = element(2, _From),
    FromServer = element(3, _From),
    From = [FromID, "@", FromServer],
    Body = xml:get_path_s(Packet, [{elem, "body"}, cdata]),
    if
	(Type == "chat") and (Body /= "") ->
    ToS   = xml:get_tag_attr_s("to", Packet),
    case ToS of
      [] ->
        ok;
      _ ->
        ToJID = string:sub_word(ToS,1,$/),
        ToServer = string:sub_word(ToJID,2,$@),
        APIKeys = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, apikeys, [] ),
        Addresses = gen_mod:get_module_opt(To#jid.lserver, ?MODULE, addresses, [] ),
        AddressArray = lists:keyfind(ToServer, 1, Addresses),
        
        case AddressArray of
          false ->
            ok;
          _ ->
            Address = element(2,AddressArray),
            APIKey = lists:keyfind(ToServer, 1, APIKeys),
            ?INFO_MSG("Found API Key for ~s. Will post message to ~s.~n", [element(1,APIKey), Address] ),
            Sep = "&",
            Post = [
              "api_token=", element(2,APIKey), Sep,
              % "application=XMPP", Sep,
              % "event=New%20Chat", Sep,
              "message=", Body, Sep,
              "from=", From, Sep,
              % "priority=-1", Sep,
              "to=", string:sub_word(ToS,1,$/) ],
            Content = list_to_binary(Post),

            httpc:request(post, {Address, [], "application/x-www-form-urlencoded", Content},[],[]),
            ok
        end
    end;
  true ->
    ok
    end.

