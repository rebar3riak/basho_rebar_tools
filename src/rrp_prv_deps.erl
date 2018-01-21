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
%% @doc RRP provider for the `rrp-deps' command.
%%
-module(rrp_prv_deps).

%% provider behavior
-ifndef(RRP_VALIDATE).
-behaviour(rrp).
-endif.
-export([do/1, format_error/1, spec/0]).

-include("rrp.hrl").

-define(PROVIDER_ATOM,  'rrp-deps').
-define(PROVIDER_STR,   "rrp-deps").
-define(PROVIDER_DEPS,  [compile]).
-define(PROVIDER_OPTS,  [
    {check, $c, "check", boolean,
        "Verify that the true dependencies match the rebar configuration."},
    {list, $l, "list", boolean,
        "List the full dependency specifications."},
    {short, $s, "short", boolean,
        "List the dependency names [default]."},
    ?RRP_RECURSIVE_OPT,
    {rebar2, $2, "rebar2", boolean,
        "Write dependencies in Rebar2 format."
        "Only meaningful in list (-l|--list) mode."},
    ?RRP_VERBOSITY_OPTS
]).

%% ===================================================================
%% Behavior
%% ===================================================================

-spec do(State :: rrp:rebar_state())
        -> {ok, rrp:rebar_state()} | rrp:prv_error().
%%
%% @doc Execute the provider command logic.
%%
do(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    ConfigVsn = case proplists:get_value(rebar2, Opts) of
        true ->
            rrp_rebar:config_format(rebar2);
        _ ->
            rrp_rebar:config_format()
    end,
    Result = handle_command(Opts, State),
    _ = rrp_rebar:config_format(ConfigVsn),
    Result.

-spec format_error(Error :: term()) -> iolist().
%%
%% @doc Format errors for display.
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

-spec short_desc() -> nonempty_string().
short_desc() ->
    "Lists or verifies the true dependencies of the current project.".

-spec long_desc() -> nonempty_string().
long_desc() ->
    short_desc() ++ "\n".

%%====================================================================
%% Internal
%%====================================================================

-type context() ::  {rrp_xref:xref(), boolean()}.

-spec handle_command(
    Opts :: [proplists:property()], State :: rrp:rebar_state())
        -> {ok, rrp:rebar_state()} | rrp:prv_error().
handle_command(Opts, State) ->
    ?LOG_INFO("Calculating dependencies...", []),
    case rrp_xref:new(State) of
        {ok, XRef} ->
            Targets = case proplists:get_value(recurse, Opts) of
                true ->
                    all;
                _ ->
                    rrp_rebar:prj_app_specs(State)
            end,
            Result = case proplists:get_value(check, Opts) of
                true ->
                    CCtx = {XRef, true},
                    case rrp_rebar:fold(Targets, fun check/3, CCtx, State) of
                        {ok, {_, true}} ->
                            {ok, State};
                        {ok, _} ->
                            {error, {?MODULE, deps_mismatch}};
                        CErr ->
                            CErr
                    end;
                _ ->
                    DCtx = {XRef, proplists:get_value(list, Opts, false)},
                    case rrp_rebar:fold(Targets, fun display/3, DCtx, State) of
                        {ok, _} ->
                            {ok, State};
                        DErr ->
                            DErr
                    end
            end,
            rrp_xref:stop(XRef),
            Result;
        Error ->
            Error
    end.

-spec check(
        App     :: rrp:app_spec(),
        Context :: context(),
        State   :: rrp:rebar_state())
        -> {ok, context()} | rrp:prv_error().

check({Name, _, _} = App, {XRef, _} = Context, State) ->
    ?LOG_DEBUG("~s:check/3: App = ~p", [?MODULE, App]),
    case rrp_xref:app_deps(XRef, Name) of
        {ok, XrefApps} ->
            AppInfo = rrp_rebar:app_info(Name, State),
            ConfDeps = lists:sort([rrp:to_atom(R)
                || R <- rebar_app_info:deps(AppInfo)]),
            CalcDeps = lists:sort(XrefApps -- [Name]),
            case ConfDeps =:= CalcDeps of
                true ->
                    ?LOG_INFO("Dependencies match: ~s", [Name]),
                    {ok, Context};
                _ ->
                    ?LOG_WARN("Dependency mismatch: ~s", [Name]),
                    {ok, {XRef, false}}
            end;
        Error ->
            Error
    end.

-spec display(
        App     :: rrp:app_spec(),
        Context :: context(),
        State   :: rrp:rebar_state())
        -> {ok, context()} | rrp:prv_error().

display({Name, _, _} = App, {XRef, Long} = Context, _State) ->
    ?LOG_DEBUG("~s:display/3: App = ~p", [?MODULE, App]),
    case rrp_xref:app_deps(XRef, Name) of
        {ok, XrefApps} ->
            Prod = XrefApps -- [Name],
            Test = rrp_fudge:test_deps(App) -- XrefApps,
            io:format("~s:~n", [Name]),
            display_deps(Long, 1, Prod, Test),
            {ok, Context};
        Error ->
            Error
    end.

-spec display_deps(
        Long    :: boolean(),
        Indent  :: non_neg_integer() | iolist(),
        Prod    :: [rrp:app_name()],
        Test    :: [rrp:app_name()])
        -> ok.

display_deps(Long, Indent, Prod, []) ->
    display_deps(Long, rrp_io:inc_indent(Indent), Prod);

display_deps(Long, Level, Prod, Test) ->
    Indent = rrp_io:indent(Level),
    SubInd = rrp_io:inc_indent(Indent),
    io:put_chars([Indent, "Prod:\n"]),
    display_deps(Long, SubInd, Prod),
    io:put_chars([Indent, "Test:\n"]),
    display_deps(Long, SubInd, Test).

-spec display_deps(
        Long    :: boolean(),
        Indent  :: iolist(),
        Apps    :: [rrp:app_name()])
        -> ok.

display_deps(true, Indent, Apps) ->
    rrp_io:write_deps(standard_io, Indent, Apps);

display_deps(false, Indent, Apps) ->
    lists:foreach(fun(App) -> io:format("~s~p~n", [Indent, App]) end, Apps).

