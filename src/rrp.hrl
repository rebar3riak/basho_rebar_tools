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

-ifndef(rrp_included).
-define(rrp_included, true).

-define(APP_NAME_DISPLAY,   "Riak Rebar Plugin").

-define(APP_NAME_ATOM,      'riak_rebar_plugin').
-define(APP_NAME_STRING,    "riak_rebar_plugin").

-define(PRV_MOD_PREFIX,     "rrp_prv_").
-define(PRV_CMD_PREFIX,     "rrp-").

-ifdef(EDOC).
-define(opaque, -opaque).
-else.
-define(opaque, -type).
-endif.

%
% Keep these usable as guard conditions.
%
-define(is_tuple_exact(Size, Term),
    erlang:is_tuple(Term) andalso erlang:tuple_size(Term) == Size).
-define(is_tuple_exact(First, Size, Term),
    ?is_tuple_exact(Size, Term) andalso erlang:element(1, Term) =:= First).
-define(is_tuple_minmax(Min, Max, Term), erlang:is_tuple(Term)
    andalso erlang:tuple_size(Term) >= Min
    andalso erlang:tuple_size(Term) =< Max).
-define(is_tuple_minmax(First, Min, Max, Term),
    ?is_tuple_minmax(Min, Max, Term) andalso erlang:element(1, Term) =:= First).

-define(is_app_name(Name), erlang:is_atom(Name)).

%
% In Rebar3, a dependency is almost always a 2-tuple, but legacy Rebar2 deps
% may have as many as 4 elements.
%
-define(is_rebar_dep(Term), ?is_tuple_minmax(2, 4, Term)
    andalso erlang:is_atom(erlang:element(1, Term))).
-define(is_rebar_dep(Name, Term), ?is_tuple_minmax(Name, 2, 4, Term)).

%
% As of this writing, sizes of the Rebar types, including the record name atom,
% are:
%   rebar_state:t()     19
%   rebar_app_info:t()  21
%
% Allow +-4 on their sizes.
%
-define(is_rebar_state(Term), ?is_tuple_minmax(state_t, 15, 23, Term)).
-define(is_rebar_app_info(Term), ?is_tuple_minmax(app_info_t, 17, 25, Term)).

%
% The earliest and latest legitimate Basho copyright years.
% Basho was founded in 2008, but it was in January, so there are some 2007
% copyrights - a lot of them, actually - that are arguably ok.
% Anything after Basho closed in 2017 is assumed to be bad.
%
-define(BASHO_YEAR_MIN, 2007).
-define(BASHO_YEAR_MAX, 2017).

%
% Common provider options.
%
-define(RRP_CHECKOUTS_OPT,
    {checkouts, $c, "checkouts", boolean,
        "When operating recursively (-r|--recursive), restrict operations to "
        "the current project and its checkouts directory."}
).
-define(RRP_LOOSE_OPT,
    {loose, $l, "loose", boolean,
        "Issue a warning, instead of an error, if the input file has an "
        "ambiguous or non-Basho copyright. "
        "The output file is [over]written with the input's original or a "
        "current-year Basho copyright (based on how ambiguous it is) and "
        "MUST be reviewed before being committed."}
).
-define(RRP_RECURSIVE_OPT,
    {recurse, $r, "recurse", boolean,
        "Apply the operation to all (true) dependencies, recursively."}
).
-define(RRP_VERBOSITY_OPTS,
    {quiet, $q, "quiet", boolean,
        "Restrict output to errors, "
        "effective only after the plugin is loaded."},
    {warn, $w, "warn", boolean,
        "Restrict output to warnings and errors, "
        "effective only after the plugin is loaded."}
).

-define(LOG_DEBUG(Fmt, Arg),  rebar_api:debug(Fmt, Arg)).
-define(LOG_INFO(Fmt, Arg),   rebar_api:info(Fmt, Arg)).
-define(LOG_WARN(Fmt, Arg),   rebar_api:warn(Fmt, Arg)).
-define(LOG_ERROR(Fmt, Arg),  rebar_api:error(Fmt, Arg)).

%
% For temporary tracing ONLY!
%
-define(RRP_VAR(Var),   io:format(
    standard_error, "~s:~b: ~s = ~p\n", [?MODULE, ?LINE, ??Var, Var])).

% Syntactic sugar.
-define(else,   true).

-endif. % rrp_included
