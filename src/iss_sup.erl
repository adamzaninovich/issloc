%%% @doc
%%% ISS Loc Top-level Supervisor
%%%
%%% See: http://erlang.org/doc/design_principles/applications.html
%%% @end

-module(iss_sup).
-vsn("0.1.0").
-license("MIT").

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).


-spec start_link() -> {ok, pid()}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    RestartStrategy = {one_for_one, 0, 60},
    Clients   = {iss_con,
                 {iss_con, start_link, []},
                 permanent,
                 5000,
                 worker,
                 [iss_con]},
    Children  = [Clients],
    {ok, {RestartStrategy, Children}}.
