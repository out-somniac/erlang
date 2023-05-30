%%%-------------------------------------------------------------------
%% @doc poll public API
%% @end
%%%-------------------------------------------------------------------

-module(poll).

-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([add_station/2, add_value/4, remove_value/3]).
-export([greet/0, read/0, get_station/1, get_station_mean/2, get_daily_mean/2, get_correlation/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE}, ?MODULE, pollution:create_monitor(), []).
init(M)        -> {ok, M}.
 
%% SYNCHRONOUS API %%
add_station(Name, Point) -> 
    gen_server:cast(?MODULE, {add_station, Name, Point}).

add_value(StationIdentifier, Datetime, Type, Value) -> 
    gen_server:cast(?MODULE, {add_value, StationIdentifier, Datetime, Type, Value}).

remove_value(StationIdentifier, Datetime, Type) ->
    gen_server:cast(?MODULE, {remove_value, StationIdentifier, Datetime, Type}).


% %% ASYNCHRONOUS API %%
greet() ->
    gen_server:call(?MODULE, greet).

read() -> 
    gen_server:call(?MODULE, read).

get_station(StationIdentifier) ->
    gen_server:call(?MODULE, {get_station, StationIdentifier}).

get_station_mean(StationIdentifier, Type) ->
    gen_server:call(?MODULE, {get_station_mean, StationIdentifier, Type}).

get_daily_mean(Type, Date) ->
    gen_server:call(?MODULE, {get_daily_mean, Type, Date}).

get_correlation(Type_1, Type_2) -> 
    gen_server:call(?MODULE, {get_correlation, Type_1, Type_2}).


%% CASTS %%
handle_cast({add_station, Name, Point}, M) -> 
    {noreply, pollution:add_station(Name, Point, M)};

handle_cast({add_value, StationIdentifier, Datetime, Type, Value}, M) -> 
    {noreply, pollution:add_value(StationIdentifier,Datetime, Type, Value, M)};

handle_cast({remove_value, StationIdentifier, Datetime, Type}, M) -> 
    {noreply, pollution:remove_value(StationIdentifier, Datetime, Type, M)}.

%% CALLS %%
handle_call(greet, _From, M) ->
    {reply, "Hello from server!", M};

handle_call(read, _From, M) -> 
    {reply, M, M};

handle_call({get_station, StationIdentifier}, _From, M) -> 
    {reply, pollution:get_station(StationIdentifier, M#monitor.stations), M};

handle_call({get_station_mean, StationIdentifier, Type}, _From, M) -> 
    {reply, pollution:get_station_mean(StationIdentifier, Type, M), M};

handle_call({get_daily_mean, Type, Date}, _From, M) -> 
    {reply, pollution:get_daily_mean(Type, Date, M), M};

handle_call({get_correlation, Type_1, Type_2}, _From, M) -> 
    {reply, pollution:get_correlation(Type_1, Type_2, M), M}.

