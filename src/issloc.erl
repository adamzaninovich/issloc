%%% @doc
%%% ISS Loc
%%% @end

-module(issloc).
-vsn("0.1.0").
-license("MIT").

-behavior(application).
-export([start/2, stop/1]).


-spec start(normal, Args :: term()) -> {ok, pid()}.
%% @private
%% Called by OTP to kick things off. This is for the use of the "application" part of
%% OTP, not to be called by user code.
%%
%% See: http://erlang.org/doc/apps/kernel/application.html

start(normal, _Args) ->
    iss_sup:start_link().


-spec stop(term()) -> ok.
%% @private
%% Similar to start/2 above, this is to be called by the "application" part of OTP,
%% not client code. Causes a (hopefully graceful) shutdown of the application.

stop(_State) ->
    ok.
