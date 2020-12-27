%%% @doc
%%% ISS Loc GUI
%%%
%%% This process is responsible for creating the main GUI frame displayed to the user.
%%%
%%% Reference: http://erlang.org/doc/man/wx_object.html
%%% @end

-module(iss_gui).
-vsn("0.1.0").
-license("MIT").

-behavior(wx_object).
-include_lib("wx/include/wx.hrl").
-export([start_link/1]).
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2, handle_event/2]).
-include("$zx_include/zx_logger.hrl").

-record(s,
        {timer  = none :: none | pid(),
         frame  = none :: none | wx:wx_object(),
         button = none :: none | wx:wx_object(),
         text   = none :: none | wx:wx_object()}).

-type state() :: term().

%%% Labels

-define(sysBUTTON, 10).

%%% Functions

start_link(Title) ->
    wx_object:start_link({local, ?MODULE}, ?MODULE, Title, []).


init(Title) ->
    ok = log(notice, "GUI starting..."),
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, Title),
    LayoutState = build_interface(Frame),
    Timer = start_ticking(),
    State = LayoutState#s{timer=Timer},
    {Frame, State}.


build_interface(Frame) ->
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    TextControl = wxTextCtrl:new(Frame, ?wxID_ANY, [{style, ?wxDEFAULT bor ?wxTE_MULTILINE}]),
    Button = wxButton:new(Frame, ?sysBUTTON, [{label, "Stop Updating"}]),

    wxSizer:add(MainSizer, TextControl, [{flag, ?wxEXPAND}, {proportion, 1}]),
    wxSizer:add(MainSizer, Button, [{flag, ?wxEXPAND}, {proportion, 0}]),
    wxFrame:setSizer(Frame, MainSizer),
    wxSizer:layout(MainSizer),

    ok = wxFrame:connect(Frame, close_window),
    ok = wxFrame:connect(Frame, command_button_clicked),
    ok = wxFrame:setSize(Frame, {0, 0, 285, 115}),
    ok = wxFrame:center(Frame),
    true = wxFrame:show(Frame),

    #s{frame = Frame, text = TextControl, button = Button}.


-spec handle_call(Message, From, State) -> Result
    when Message  :: term(),
         From     :: {pid(), reference()},
         State    :: state(),
         Result   :: {reply, Response, NewState}
                   | {noreply, State},
         Response :: ok
                   | {error, {listening, inet:port_number()}},
         NewState :: state().

handle_call(Unexpected, From, State) ->
    ok = log(warning, "Unexpected call from ~tp: ~tp~n", [From, Unexpected]),
    {noreply, State}.


-spec handle_cast(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().

handle_cast(update, State) ->
    ok = do_update(State),
    {noreply, State};
handle_cast(Unexpected, State) ->
    ok = log(warning, "Unexpected cast: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_info(Message, State) -> {noreply, NewState}
    when Message  :: term(),
         State    :: state(),
         NewState :: state().

handle_info(tick, State = #s{timer = OldTimer}) ->
    erlang:cancel_timer(OldTimer),
    update(),
    Timer = erlang:send_after(1000, self(), tick),
    {noreply, State#s{timer=Timer}};
handle_info(Unexpected, State) ->
    ok = log(warning, "Unexpected info: ~tp~n", [Unexpected]),
    {noreply, State}.


-spec handle_event(Event, State) -> {noreply, NewState}
    when Event    :: term(),
         State    :: state(),
         NewState :: state().

handle_event(#wx{event = #wxCommand{type = command_button_clicked}}, State) ->
    Timer = do_toggle_updating(State),
    NewState = State#s{timer=Timer},
    {noreply, NewState};
handle_event(#wx{event = #wxClose{}}, State = #s{frame = Frame}) ->
    ok = iss_con:stop(),
    ok = wxWindow:destroy(Frame),
    {noreply, State};
handle_event(Event, State) ->
    ok = log(info, "Unexpected event ~tp State: ~tp~n", [Event, State]),
    {noreply, State}.


code_change(_, State, _) ->
    {ok, State}.


terminate(Reason, State) ->
    ok = log(info, "Reason: ~tp, State: ~tp", [Reason, State]),
    wx:destroy().


start_ticking() ->
    erlang:send_after(1, self(), tick).


do_toggle_updating(#s{timer = none, button = Button}) ->
    Timer = start_ticking(),
    wxButton:setLabel(Button, "Stop Updating"),
    Timer;
do_toggle_updating(#s{timer = Timer, button = Button}) ->
    erlang:cancel_timer(Timer),
    wxButton:setLabel(Button, "Start Updating"),
    none.


do_update(#s{text = TextControl}) ->
    ISSLocData = iss_con:get_iss_loc(),
    #{"iss_position" := #{"latitude" := Lat, "longitude" := Lng}} = ISSLocData,
    #{"timestamp" := Timestamp} = ISSLocData,
    FormattedTime = calendar:system_time_to_rfc3339(Timestamp),
    String = io_lib:format("Current Position: ~tp, ~tp~n~nTimestamp: ~tp", [Lat, Lng, FormattedTime]),
    wxTextCtrl:changeValue(TextControl, String).
