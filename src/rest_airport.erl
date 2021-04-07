-module(rest_airport).
-include_lib("stdlib/include/qlc.hrl").
-include_lib("yaws/include/yaws_api.hrl").

-include("../include/rest_airport.hrl").


-export([out/1, addAirport/4, handle/2,do/1]).
%-compile(export_all).

out(Arg) ->
    Method = method(Arg) ,
    io:format("~p:~p ~p Request ~n", [?MODULE, ?LINE, Method]),
    handle(Method, Arg).

method(Arg) ->
  Rec = Arg#arg.req,
  Rec#http_request.method.


convert_to_json(Lines) ->
    Data = [{struct, 
	     [{airport, Line#?RECORD_TYPE.code},
	      {city,    Line#?RECORD_TYPE.city},
	      {country, Line#?RECORD_TYPE.country},
	      {name,    Line#?RECORD_TYPE.name}]}
	    || Line <- Lines],
    JsonData = {struct, [{data, Data}]},
    json2:encode(JsonData).

addAirport(Code, City, Country, Name) ->
    NewRec = #?RECORD_TYPE{ 
		 ?RECORD_KEY_FIELD	= Code,
		 city			= City,
		 country		= Country,
		 name			= Name},
    io:format("~p:~p Adding Airport ~p~n",
	      [?MODULE,?LINE, NewRec]),
    Add = fun() ->
                         mnesia:write(NewRec)
                 end,
    {atomic, _Rec} = mnesia:transaction(Add),
    NewRec.


handle('GET', _Arg) ->
    io:format("~n ~p:~p GET Request ~n", [?MODULE, ?LINE]),
    Records = do(qlc:q([X || X <- mnesia:table(?RECORD_TYPE)])),
    Json = convert_to_json( Records),
    io:format("~n ~p:~p GET Request Response ~p ~n", [?MODULE, ?LINE, Json]),
    {html, Json};

handle('POST', Arg) ->
    io:format("~n ~p:~p POST Arg:~p ~n", [?MODULE, ?LINE,Arg]),
    
    {ok, Json} = json2:decode_string(binary_to_list(Arg#arg.clidata)),
    io:format("~n~p:~p POST request ~p~n", 
              [?MODULE, ?LINE, Json]),
    Airport	= json2:obj_fetch("airport",Json, <<>>),
    City	= json2:obj_fetch("city",Json, <<>>),
    Country	= json2:obj_fetch("country",Json, <<>>),
    Name	= json2:obj_fetch("name",Json, <<>>),
    _Status = addAirport(Airport, City, Country, Name),
    [{status, 201},
     {html, Arg#arg.clidata}];



handle('PUT', Arg) ->
    [IndexValue,_] = string:tokens(Arg#arg.pathinfo),    
    {ok, Json} = json2:decode_string(binary_to_list(Arg#arg.clidata)),
    io:format("~p:~p PUT request ~p ~p~n",
              [?MODULE, ?LINE, IndexValue, Json]),
    Airport	= json2:obj_fetch("airport",Json, <<>>),
    City	= json2:obj_fetch("city",Json, <<>>),
    Country	= json2:obj_fetch("country",Json, <<>>),
    Name	= json2:obj_fetch("name",Json, <<>>),

    NewRec = #?RECORD_TYPE{
		 ?RECORD_KEY_FIELD	= Airport,
		 city			= City,
		 country		= Country,
		 name			= Name},

    io:format("~p:~p Renaming ~p", 
              [?MODULE, ?LINE, NewRec]),
    ChangeName = fun() ->
			 mnesia:delete(
			   {?RECORD_KEY_FIELD, IndexValue}),			     
                         mnesia:write(NewRec)
                 end,
    {atomic, _Rec} = mnesia:transaction(ChangeName),
    [{status, 200},
     {html, IndexValue}];


handle('DELETE', Arg) ->

    [IndexValue, _ ] = string:tokens(Arg#arg.pathinfo),    
    io:format("~p:~p DELETE request ~p",
              [?MODULE, ?LINE, IndexValue]),

    Delete = fun() ->
                     mnesia:delete(
                       {?RECORD_KEY_FIELD, IndexValue})
             end,

    Resp = mnesia:transaction(Delete),
    case Resp of
        {atomic, ok} ->
            [{status, 204}];
        {_, Error} ->
            io:format("~p:~p Error ~p ", 
                      [?MODULE, ?LINE, Error]),
            [{status, 400},
             {html, Error}]
    end;


handle(Method,_) ->
    [{error, "Unknown method " ++ Method},
     {status, 405},
     {header, "Allow: GET, HEAD, POST, PUT, DELETE"}
     ].


do(Q)->
    F = fun() ->
                qlc:e(Q) 
	end,
    {atomic, Value} = mnesia:transaction(F),
    Value.
