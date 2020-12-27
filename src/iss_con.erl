%%% @doc
%%% ISS Loc Controller
%%%
%%% This process is in charge of maintaining the program's state.
%%% @end

-module(iss_con).
-vsn("0.1.0").
-license("MIT").

-behavior(gen_server).
-export([get_iss_loc/0]).
-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).
-include("$zx_include/zx_logger.hrl").

%%% Type and Record Definitions

-record(s,
        {window = none :: none | wx:wx_object()}).

-type state() :: #s{}.


%% Interface

-spec stop() -> ok.

stop() ->
    gen_server:cast(?MODULE, stop).

get_iss_loc() ->
    gen_server:call(?MODULE, get_iss_loc).

%%% Functions

-spec start_link() -> Result
    when Result :: {ok, pid()}
                 | {error, Reason},
         Reason :: {already_started, pid()}
                 | {shutdown, term()}
                 | term().

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, none, []).


-spec init(none) -> {ok, state()}.

init(none) ->
    ok = log(info, "Starting"),
    application:start(inets),
    Window = iss_gui:start_link("ISS Position"),
    ok = log(info, "Window: ~p", [Window]),
    State = #s{window = Window},
    {ok, State}.


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().

handle_call(get_iss_loc, _, State) ->
    Data = do_get_iss_loc(),
    {reply, Data, State};
handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().

handle_cast(stop, State) ->
    ok = log(info, "Received a 'stop' message."),
    {stop, normal, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().

handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    zx:stop().


do_get_iss_loc() ->
  Response = httpc:request(get, {"http://api.open-notify.org/iss-now.json", []}, [], []),
  case Response of
    {ok, {{_, 200, "OK"}, _Headers, JSONString}} ->
      decode(JSONString);
    Error ->
      Error
  end.


decode(JSON) ->
  case zj:decode(JSON) of
    {ok, Data} -> Data;
    Error -> Error
  end.

