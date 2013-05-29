%%%-------------------------------------------------------------------
%%% File    : mod_amp.erl
%%% Author  : Paul Guyot, Thierry Bomandouki
%%% Purpose : XEP-0079: Advanced Message Processing v1.2 partial
%%% Created : 16 Nov 2007 by Paul Guyot <paul@violet.net>
%%%           and Thierry Bomandouki <thierry@violet.net>
%%% Id      : $Id$
%%%-------------------------------------------------------------------

%%%
%%% This module requires the patch that adds filter_local_packet hook.
%%%
%%% Not implemented:
%%% - service discovery
%%% - handling of servers that do not support XEP-0079
%%% - most error conditions
%%% - attributes of AMP element are not read
%%%
%%% Implemented Conditions:
%%% + deliver
%%% + expire-at
%%% + match-resource
%%%
%%% Implemented 'deliver' values:
%%% + direct
%%% + stored
%%%
%%% Not implemented 'deliver' values:
%%% - forward
%%% - gateway
%%% - none
%%%
%%% Implemented Actions:
%%% + alert
%%% + drop
%%% + error
%%% + notify
%%% + store: custom action that stores the packet using mod_offline
%%%

-module(mod_amp).
-author('paul@violet.net').

-behaviour(gen_mod).

-export([start/2,
   stop/1,
   filter_hook/1,
   filter_local_hook/1,
   offline_hook/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(NS_AMP,         "http://jabber.org/protocol/amp").
-define(NS_AMP_ERRORS,  "http://jabber.org/protocol/amp#errors").

start(Host, _Opts) ->
    ejabberd_hooks:add(filter_packet, ?MODULE, filter_hook, 50),
    ejabberd_hooks:add(filter_local_packet, ?MODULE, filter_local_hook, 50),
    %% 25 < 50, so we're called before mod_offline.
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_hook, 25).

stop(Host) ->
    ejabberd_hooks:delete(filter_packet, ?MODULE, filter_hook, 50),
    ejabberd_hooks:delete(filter_local_packet, ?MODULE, filter_local_hook, 50),
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE, offline_hook,25).

%% filter_hook/1
%% {From, To, Packet}
%% {NewFrom, NewTo, NewPacket} -> alter the packet.
%% drop -> drop the packet.
%%filter_hook(From, To, {xmlelement, "message", _Attrs, _SubEl} = Packet) ->
%%  process_message(From, To, Packet, "direct");
filter_hook({From, To, Packet}) -> {From, To, Packet};
filter_hook(Val) -> Val.

%% filter_local_hook/1
%% {From, To, Packet}
%% {NewFrom, NewTo, NewPacket} -> alter the packet.
%% drop -> drop the packet.
filter_local_hook({From, To, {xmlelement, "message", _, _} = Packet}) ->
    ACTION = process_message(From, To, Packet, "direct"),
    case ACTION of
        ok -> {From, To, Packet};
        stop -> drop
    end;
filter_local_hook(Val) -> Val.

%% offline_hook/3
%% (From, To, Packet)
%% stop -> stop processing
%% _    -> continue to next hook
offline_hook(From, To, {xmlelement, "message", _Attrs, _SubEl} = Packet) ->
    process_message(From, To, Packet, "stored");
offline_hook(_From, _To, _Packet) -> ok.

process_message(From, To, {xmlelement, "message", MAttrs, SubEl} = Packet,
    Deliver) ->
    case xml:get_attr_s("type", MAttrs) of
        "error" -> ok;
        _ ->
      Local = jlib:make_jid("", From#jid.server, ""),
      AmpEl = get_ampel(SubEl),
      _NewSubEl = remove_amp_element(SubEl, []),
      case AmpEl of
    none -> ok;
    {xmlelement, "amp", Attrs, RulesEls} ->
        Options = process_amp_attrs(Attrs),
        process_action(RulesEls, From, To, Local, Options, Packet,
           Deliver, MAttrs, Attrs, AmpEl)
            end
    end.


process_action(RulesEl, From, To, Local, Options, Packet, Deliver, MAttrs,
         Attrs, _AmpEl) ->
    {xmlelement, "message", _PacketAttrs, _PacketSubEl} = Packet,
    {Action, CurRule} = process_rules(RulesEl, Options, From, To, Packet,
              Deliver),
    case Action of
  "store" ->
      %% Specification extension by Violet
      mod_offline:store_packet(From, To, Packet),
      stop;
  "alert" ->
      send_amp_notify_response(Local, From, To, MAttrs, Attrs, CurRule,
             "alert"),
            stop;
        "drop" ->
      stop;
        "notify" ->
      send_amp_notify_response(Local, From, To, MAttrs, Attrs, CurRule,
             "notify"),
            ok;
        "error" ->
      send_amp_error_response(Local, From, To, MAttrs, Attrs, CurRule,
            {"undefined-condition", 500}),
      stop;
  default ->
      ok;
  _ ->
            %% Including error, sending response message with error info
            NewMAttrs = replace_attr("type", "error", MAttrs),
            SubMessageError = [{xmlelement, "unsupported-actions",
        [{"xmlns", ?NS_AMP}], [CurRule]}],
      Attrs2 = [{"status","error"},
          {"from", jlib:jid_to_string(From)},
          {"to", jlib:jid_to_string(To)} |
          Attrs],
            SubMessage = [{xmlelement, "amp", Attrs2,
         [CurRule]},
        {xmlelement, "error", [{"type", "modify"},
             {"code", "400"}],
         SubMessageError}],
      Msg = {xmlelement, "message", NewMAttrs, SubMessage},
      ejabberd_router:route(Local, From, Msg),
            stop
    end.

%% get_ampel/1
%% SubEl -> El | none
%% Get the first (and unique?) amp element.
get_ampel([{xmlelement, "amp", Attrs, _SubEl} = AmpEl | _Tl]) ->
      case xml:get_attr_s("status", Attrs) of
    "" -> AmpEl;
    _ -> none
      end;
get_ampel([_El | Tl]) -> get_ampel(Tl);
get_ampel([]) -> none.


%% process_amp_attrs/1
%% Attrs -> something
process_amp_attrs(_Attrs) -> tbd.

%% process_rules/6
%% RulesEl (array of {xmlelement,...}), Options (tbd), From, To, Packet, Deliver
%% action, rule
process_rules([], _Options, _From, _To, _Packet, _Deliver) -> {default, none};
process_rules([{xmlelement, "rule", Attrs, _SubEl} = CurRule  | RulesTl],
        Options, From, To, Packet, Deliver) ->
    Action = xml:get_attr_s("action", Attrs),
    Value = xml:get_attr_s("value", Attrs),
    Condition = xml:get_attr_s("condition", Attrs),
    Result = case Condition of
     "deliver" ->
         case Value of
       Deliver -> {Action, CurRule};
       _ -> process_rules(RulesTl, Options, From, To, Packet,
              Deliver)
         end;
     "match-resource" ->
         %% To#jid.resource,
         Resources = ejabberd_sm:get_user_resources(To#jid.user,
                To#jid.server),
         ActualResource = To#jid.lresource,
         case {Value,
         is_matched_resource(ActualResource, Resources)} of
       {"any", _} ->
           {Action, CurRule};
       {"exact", true} ->
           {Action, CurRule};
       {"other", false} ->
           {Action, CurRule};
       _ ->
           process_rules(RulesTl, Options, From, To, Packet,
             Deliver)
         end;
     "expire-at" ->
         {TimeExpMegaS, TimeExpS, TimeExpMilliS} =
       jlib:datetime_string_to_timestamp(Value),
         {NowMegaS, NowS, NowMilliS} = erlang:now(),
         After =
       if
           NowMegaS > TimeExpMegaS -> true;
           NowMegaS == TimeExpMegaS ->
         if
             NowS > TimeExpS ->
           true;
             NowS == TimeExpS ->
           NowMilliS > TimeExpMilliS;
             true ->
           false
         end;
           true -> false
       end,
         case After of
       true -> {Action, CurRule};
       _ -> process_rules(RulesTl, Options, From, To, Packet,
              Deliver)
         end
       end,
    Result;

process_rules([_ | RulesTl], Options, From, To, Packet, Deliver) ->
    process_rules(RulesTl, Options, From, To, Packet, Deliver).

%% replace_attr/3
%% (Attr, Value, Attrs)
replace_attr(Attr, Value, Attrs) ->
    case lists:keysearch(Attr, 1, Attrs) of
        false -> Attrs1 = Attrs;
        _ -> Attrs1 = lists:keydelete(Attr, 1, Attrs)
    end,
    Attrs2 = [{Attr, Value} | Attrs1],
    Attrs2.

%% remove_amp_element/2
%% jid, sessions, acc -> sessions
remove_amp_element([], Acc) ->
    Acc;
remove_amp_element([{xmlelement, "amp", _Attrs, _SubEl} | Tl], Acc) ->
    remove_amp_element(Tl, Acc);
remove_amp_element([Hd | Tl], Acc) ->
    remove_amp_element(Tl, [Hd | Acc]).

%% send_amp_notify_response/6
send_amp_notify_response(Local, From, To, MAttrs, Attrs, RulesEl, Status) ->
    Attrs2 = [{"status",Status},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)} |
        Attrs],
    SubMessage = [{xmlelement, "amp", Attrs2, [RulesEl]}],
    Msg = {xmlelement, "message", MAttrs, SubMessage},
    ejabberd_router:route(Local, From, Msg),
    ok.

%% send_amp_error_response/7
send_amp_error_response(Local, From, To, MAttrs, Attrs, RulesEl,
      {NameErr,CodeErr} = _Code) ->
    NewMAttrs = replace_attr("type", "error", MAttrs),
    SubMessageError = [{xmlelement, NameErr, [{"xmlns", ?NS_STANZAS}], []},
           {xmlelement, "failed-rules",
      [{"xmlns", ?NS_AMP_ERRORS}], [RulesEl]}],
    Attrs2 = [{"status","error"},
        {"from", jlib:jid_to_string(From)},
        {"to", jlib:jid_to_string(To)} |
        Attrs],
    SubMessage = [{xmlelement, "amp", Attrs2,
       [RulesEl]},
      {xmlelement, "error", [{"type", "modify"}, {"code", CodeErr}],
       SubMessageError}],
    Msg = {xmlelement, "message", NewMAttrs, SubMessage},
    ejabberd_router:route(Local, From, Msg),
    ok.

%% is_matched_resource/2
%% (Resource, Resources)
is_matched_resource(Resource, [Resource | _ResourcesTl]) ->
    true;
is_matched_resource(Resource, [_ResourcesHd | ResourcesTl]) ->
    is_matched_resource(Resource, ResourcesTl);
is_matched_resource(_Resource, []) ->
    false.