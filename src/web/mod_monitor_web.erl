%%%----------------------------------------------------------------------
%%% File    : mod_monitor_web.erl
%%% Author  : Badlop
%%% Purpose : Provide an HTTP page with simple status information
%%% Created :
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2010   ProcessOne
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

-module(mod_monitor_web).

-export([process/2]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_http.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

process(Path, #request{method = 'GET',
		       data = []}) ->
    Status = get_status(Path),
    StatusString = convert_string(Status),
    {200, [html], get_human_html_xmlel(StatusString)};
process(_Path, _Request) ->
    ?DEBUG("Bad Request: ~p", [_Request]),
    {400, [html], {xmlelement, "h1", [],
                    [{xmlcdata, "400 Bad Request"}]}}.

tests() -> ["crypto", "ejabberd", "mnesia"].

%% To support any command (don't enable in a publicly accessible URL!!):
%%get_status(["command", CommandNameStr]) ->
%%    ejabberd_commands:execute_command(list_to_atom(CommandNameStr), []);
get_status(["crypto"]) ->
    lists:keymember(crypto, 1, application:which_applications());
get_status(["ejabberd"]) ->
    lists:keymember(ejabberd, 1, application:which_applications());
get_status(["mnesia"]) ->
    lists:keymember(mnesia, 1, application:which_applications());
get_status([]) ->
    tests();
get_status(["all"]) ->
    lists:all(
      fun(Type) -> convert_string(get_status([Type])) == "1" end,
      tests()).

convert_string(true) -> "1";
convert_string(false) ->"0";
convert_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
convert_string(List) when is_list(List) ->
    "Available tests: all " ++ string:join(List, " ").

get_human_html_xmlel(StatusString) ->
    {xmlelement, "html", [{"xmlns", "http://www.w3.org/1999/xhtml"}],
     [{xmlelement, "body", [],
       [{xmlelement, "p", [],
         [{xmlcdata, StatusString}
	 ]}
       ]}]}.
