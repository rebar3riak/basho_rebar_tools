%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
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

%% @private
%% @doc Adjust the Rebar state for Basho tools.
%%
%% It would be preferable to do this with hooks, but Rebar hooks can't
%% change the state seen by subsequent tasks, so we do it globally at
%% initialization.
%%
-module(brt_inject_state).

% Private API
-export([
    inject/1
]).

-include("brt.hrl").

-type define()  :: atom() | {atom(), term()}.

-record(tgt, {
    profile             :: atom(),
    app                 :: atom(),
    define  = default   :: define() | [define()],
    header  = undefined :: string() | default | undefined
}).

-define(INJECT_TARGETS, [
    #tgt{profile = test,  app = eqc,  header = default}
]).

%% ===================================================================
%% Private API
%% ===================================================================

-spec inject(State :: brt:rebar_state()) -> brt:rebar_state().
%% @private
%% @doc Inject whatever changes we need into the State.
%%
inject(State) ->
    lists:foldl(fun inject/2, State, ?INJECT_TARGETS).

%% ===================================================================
%% Internal
%% ===================================================================

-spec inject(Target :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Default is to add `{d, <uppercase-App>}' to `erl_opts' in Profile if the App is
% present. If that element is already present in State, the test is skipped.
% Default is to pass expanded Target to inject_target/2.
% Add discriminating function heads to override default behavior.
%
inject(Target, State) ->
    % ?LOG_DEBUG("~s:inject(~p, State).", [?MODULE, Target]),
    Tgt = expand_defaults(Target),
    Opts = brt:get_key_list(erl_opts, get_profile(Tgt#tgt.profile, State)),
    case lists:all(fun(Def) -> lists:member(Def, Opts) end, Tgt#tgt.define) of
        true ->
            State;
        _ ->
            inject_target(Tgt, State)
    end.

-spec inject_target(Tgt :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Default is to look for a header file with the app's name as part of the test.
% When the test is positive, `{d, <uppercase-Target>}' is added to `erl_opts' in Profile.
%
% Discriminating function heads can be added here or in set_target_opts/2 to
% override default behavior.
%
inject_target(#tgt{header = undefined} = Tgt, State) ->
    ?LOG_DEBUG("Checking application '~s'", [Tgt#tgt.app]),
    case code:lib_dir(Tgt#tgt.app) of
        {error, bad_name} ->
            State;
        _ ->
            set_target_opts(Tgt, State)
    end;

inject_target(Tgt, State) ->
    ?LOG_DEBUG("Checking application '~s'", [Tgt#tgt.app]),
    case code:lib_dir(Tgt#tgt.app, include) of
        {error, bad_name} ->
            State;
        Dir ->
            Incl = filename:join(Dir, Tgt#tgt.header),
            ?LOG_DEBUG("Checking file '~s'", [Incl]),
            case filelib:is_regular(Incl) of
                true ->
                    set_target_opts(Tgt, State);
                _ ->
                    State
            end
    end.

-spec expand_defaults(Tgt :: #tgt{}) -> #tgt{}.
%
% Fill in the blanks ...
%
expand_defaults(#tgt{app = App, define = default} = Tgt) ->
    expand_defaults(Tgt#tgt{define = [brt:to_atom(string:to_upper(brt:to_string(App)))]});

expand_defaults(#tgt{define = Def} = Tgt) when not erlang:is_list(Def) ->
    expand_defaults(Tgt#tgt{define = [Def]});

expand_defaults(#tgt{app = App, header = default} = Tgt) ->
    expand_defaults(Tgt#tgt{header = lists:flatten(io_lib:format("~s.hrl", [App]))});

% would be nice to make this conditional, if only we had a preprocessor ...
expand_defaults(Tgt) when not (
        erlang:is_record(Tgt, tgt)
        andalso erlang:is_atom(Tgt#tgt.profile)
        andalso erlang:is_atom(Tgt#tgt.app)
        andalso erlang:is_list(Tgt#tgt.define)
        andalso (Tgt#tgt.header =:= undefined
                orelse erlang:is_list(Tgt#tgt.header)) ) ->
    erlang:error(badarg, [Tgt]);

expand_defaults(#tgt{define = Macros} = Tgt) ->
    Defs = lists:map(
        fun({Def, Val}) when erlang:is_atom(Def) ->
                {d, Def, Val};
            (Def) when erlang:is_atom(Def) ->
                {d, Def};
            (Bad) ->
                erlang:error(badarg, [Bad, Tgt])
        end, Macros),
    Tgt#tgt{define = Defs}.

-spec get_profile(Name :: atom(), State :: brt:rebar_state()) -> list().
%
% Returns the Named profile from State.
%
% The result is always a list, even if the profile is undefined.
%
get_profile(Name, State) ->
    case rebar_state:get(State, profiles) of
        undefined ->
            [];
        Profs ->
            brt:get_key_list(Name, Profs)
    end.

-spec set_profile(Name :: atom(), Value :: list(), State :: brt:rebar_state())
        -> brt:rebar_state().
%
% Updates the Named profile in State and returns the new State.
%
set_profile(Name, Value, State) ->
    Profiles = case rebar_state:get(State, profiles) of
        undefined ->
            [{Name, Value}];
        Profs ->
            lists:keystore(Name, 1, Profs, {Name, Value})
    end,
    rebar_state:set(State, profiles, Profiles).

-spec set_target_opts(Tgt :: #tgt{}, State :: brt:rebar_state()) -> brt:rebar_state().
%
% Add the appropriate options to the target profile.
% At this point whatever predicates apply have passed.
% Discriminating function heads are added to override default behavior.
%
set_target_opts(#tgt{app = eqc = Mod, define = Defs} = Tgt, State) ->
    % Use an indirect module so xref and dialyzer don't complain.
    % Conveniently, it's the App name.
    [Maj, Min | _] = Vsn = case Mod:version() of
        % As of this writing EQC returns the version as a float.
        % I'm reasonably sure I've seen it documented somewhere, but not
        % finding it so I'm winging it here ...
        FVsn when erlang:is_float(FVsn) ->
            V1 = erlang:trunc(FVsn),
            R1 = (FVsn - V1),
            V2 = erlang:trunc(R1 * 100),
            R2 = ((R1 * 100) - V2),
            V3 = erlang:trunc(R2 * 10),
            [V1, V2, V3];
        SVsn ->
            brt:parse_version(SVsn)
    end,
    NewDefs = if
        % Assume breaking API changes if we ever see a major version > 1.
        Maj > 1 ->
            ?LOG_WARN("EQC version ~p may not be handled properly!", [Vsn]),
            Def = brt:to_atom(io_lib:format("EQC_API_~b", [Maj])),
            [{d, Def} | Defs];

        % Lots of API changes at version 1.35.x.
        Maj =:= 1 andalso Min >= 35 ->
            [{d, 'EQC_API_1_35'} | Defs];

        ?else ->
            Defs
    end,
    NextDefs = [{d, 'EQC_VERSION', Vsn} | NewDefs],
    set_target_opts_final(Tgt#tgt{define = NextDefs}, State);

set_target_opts(Tgt, State) ->
    set_target_opts_final(Tgt, State).

-spec set_target_opts_final(Tgt :: #tgt{}, State :: brt:rebar_state())
        -> brt:rebar_state().
%
% Add the specified options to the target profile.
%
set_target_opts_final(#tgt{profile = Profile, define = Defs}, State) ->
    ?LOG_DEBUG("Inject into profile '~s': ~p", [Profile, Defs]),
    Orig = get_profile(Profile, State),
    Opts = lists:foldl(
        fun(Def, OptsIn) ->
            case lists:member(Def, OptsIn) of
                true ->
                    OptsIn;
                _ ->
                    % Add at the end - if there's a macro value conflict, we
                    % want the one we're adding to trigger the redefinition
                    % error, not the one that was already there.
                    lists:append(OptsIn, [Def])
            end
        end, brt:get_key_list(erl_opts, Orig), Defs),
    NewProf = lists:keystore(erl_opts, 1, Orig, {erl_opts, Opts}),
    set_profile(Profile, NewProf, State).
