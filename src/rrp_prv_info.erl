%% -------------------------------------------------------------------
%%
%% Copyright (c) 2018 Rebar3Riak Contributors
%% Copyright (c) 2016-2017 Basho Technologies, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%
%% @doc RRP provider for the `rrp-info' command.
%%
-module(rrp_prv_info).

%% provider behavior
-ifndef(RRP_VALIDATE).
-behaviour(rrp).
-endif.
-export([do/1, format_error/1, spec/0]).

-include("rrp.hrl").

-define(PROVIDER_ATOM,  'rrp-info').
-define(PROVIDER_STR,   "rrp-info").
-define(PROVIDER_DEPS,  []).
-define(PROVIDER_OPTS,  []).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec do(State :: rrp:rebar_state()) -> {ok, rrp:rebar_state()}.
%%
%% @doc Display provider information.
%%
do(State) ->
    {rrp_io:write_info(standard_io), State}.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Placeholder to fill out the `provider' API, should never be called.
%%
format_error(Error) ->
    rrp:format_error(Error).

-spec spec() -> [{atom(), term()}].
%%
%% @doc Return the proplist that will be supplied to providers:create/1.
%%
spec() ->
    [
        {name,          ?PROVIDER_ATOM},
        {module,        ?MODULE},
        {bare,          true},
        {deps,          ?PROVIDER_DEPS},
        {example,       "rebar3 " ?PROVIDER_STR},
        {short_desc,    short_desc()},
        {desc,          long_desc()},
        {opts,          ?PROVIDER_OPTS}
    ].

%%====================================================================
%% Help Text
%%====================================================================

-spec short_desc() -> string().
short_desc() ->
    "Information about " ?APP_NAME_DISPLAY.

-spec long_desc() -> string().
long_desc() ->
    short_desc().

%%====================================================================
%% Internal
%%====================================================================

