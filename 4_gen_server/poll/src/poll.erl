%%%-------------------------------------------------------------------
%% @doc poll public API
%% @end
%%%-------------------------------------------------------------------

-module(poll).

-behaviour(gen_server).



%% API
-export([start_link/0, close/0, crash/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([read/0, add_station/2]).

%% START %%
start_link()   -> gen_server:start_link({local,?MODULE}, ?MODULE, pollution:create_monitor(), []).
init(M)        -> {ok, M}.
 
%% API %%
read() -> gen_server:call(?MODULE, read).
add_station(Name, Point) -> gen_server:cast(?MODULE, {add_station, Name, Point}).
close()     -> gen_server:call(?MODULE, terminate).
crash()     -> gen_server:cast(?MODULE, crash).
 
%% OBSŁUGA WIADOMOŚCI %%
handle_cast({add_station, Name, Point}, M) -> {noreply, pollution:add_station(Name, Point, M)}.
 
handle_call(read, _From, M)      -> {reply, M, M};
handle_call(terminate,_From,N) -> {stop, normal, ok, N}.
 
terminate(normal, N) -> io:format("The number is: ~B~nBye.~n",[N]), ok.