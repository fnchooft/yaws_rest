%%%-------------------------------------------------------------------
%% @doc yaws_rest public API
%% @end
%%%-------------------------------------------------------------------

-module(yaws_rest_app).

-behaviour(application).

-export([start/2, stop/1]).

-include_lib("yaws_rest/include/rest_airport.hrl").


start(_StartType, _StartArgs) ->
    application:start(mnesia),
    mnesia:create_table(airport, [
	    {attributes,record_info(fields, airport)},
	    {index, [country]}
    ]),
        
    yaws_rest_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
