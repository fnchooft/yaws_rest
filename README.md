yaws_rest
=========

Implementation of Chapter 4.1 from:

```
Building Web Applications with Erlang
by
Released June 2012
Publisher(s): O'Reilly Media, Inc.
ISBN: 9781449309961
```

Alterations
-----------

The older rest-module example was using rfc4627.erl which could not be found.
The latest version from YAWS comes with its own json2-parser.


The following function was added to json2 in yaws:
```bash
$ git diff --patch-with-stat 
 src/json2.erl | 11 ++++++++++-
 1 file changed, 10 insertions(+), 1 deletion(-)

diff --git a/src/json2.erl b/src/json2.erl
index 64a8a4f9..de157de7 100644
--- a/src/json2.erl
+++ b/src/json2.erl
@@ -16,7 +16,7 @@
 
 -module(json2).
 -export([encode/1, decode_string/1, decode/2]).
--export([is_obj/1, obj_new/0, obj_fetch/2, obj_find/2, obj_is_key/2]).
+-export([is_obj/1, obj_new/0, obj_fetch/2, obj_fetch/3, obj_find/2, obj_is_key/2]).
 -export([obj_store/3, obj_from_list/1, obj_fold/3]).
 -export([test/0]).
 -author("Jim Larson <jalarson@amazon.com>, Robert Wai-Chi Chu <robchu@amazon.com>").
@@ -607,6 +607,15 @@ obj_fetch(Key, {struct, Props}) when is_list(Props) ->
             Value
     end.
 
+obj_fetch(Key, {struct, Props},Default) when is_list(Props) ->
+    case proplists:get_value(Key, Props) of
+        undefined ->
+            Default;
+        Value ->
+            Value
+    end.
+
+
 %% Fetch an object member's value, or indicate that there is no such member.
 %% Return {ok, Value} or 'error'.
```

The differences from the original O-Reilly rest file vs the newer rest_airport.erl:

```bash
$ diff rest.reference rest_airport.erl -y --suppress-common-lines
-module(rest).						      |	-module(rest_airport).
-include("/usr/lib/erlang/lib/stdlib-1.17.3/include/qlc.hrl") |	-include_lib("stdlib/include/qlc.hrl").
-include("/usr/lib/yaws/include/yaws_api.hrl").		      |	-include_lib("yaws/include/yaws_api.hrl").
-export([out/1, addAirport/4, handle/2]).		      <
%-compile(export_all).					      <
							      >	-include("../include/rest_airport.hrl").
-define(RECORD_TYPE,      airport).			      <
-define(RECORD_KEY_FIELD, code).			      <
-record(?RECORD_TYPE,					      |	-export([out/1, addAirport/4, handle/2,do/1]).
        {?RECORD_KEY_FIELD, city, country, name }).	      |	%-compile(export_all).
    Data = [{obj, 					      |	    Data = [{struct, 
    JsonData = {obj, [{data, Data}]},			      |	    JsonData = {struct, [{data, Data}]},
    rfc4627:encode(JsonData).				      |	    json2:encode(JsonData).
    {ok, Json, _} = rfc4627:decode(Arg#arg.clidata),	      |	    io:format("~n ~p:~p POST Arg:~p ~n", [?MODULE, ?LINE,Arg]
							      >	    
							      >	    {ok, Json} = json2:decode_string(binary_to_list(Arg#arg.c
    Airport	= rfc4627:get_field(Json, "airport", <<>>),   |	    Airport	= json2:obj_fetch("airport",Json, <<>>),
    City	= rfc4627:get_field(Json, "city", <<>>),      |	    City	= json2:obj_fetch("city",Json, <<>>),
    Country	= rfc4627:get_field(Json, "country", <<>>),   |	    Country	= json2:obj_fetch("country",Json, <<>>),
    Name	= rfc4627:get_field(Json, "name", <<>>),      |	    Name	= json2:obj_fetch("name",Json, <<>>),
    {ok, Json, _} = rfc4627:decode(Arg#arg.clidata),	      |	    {ok, Json} = json2:decode_string(binary_to_list(Arg#arg.c
    Airport	= rfc4627:get_field(Json, "airport", <<>>),   |	    Airport	= json2:obj_fetch("airport",Json, <<>>),
    City	= rfc4627:get_field(Json, "city", <<>>),      |	    City	= json2:obj_fetch("city",Json, <<>>),
    Country	= rfc4627:get_field(Json, "country", <<>>),   |	    Country	= json2:obj_fetch("country",Json, <<>>),
    Name	= rfc4627:get_field(Json, "name", <<>>),      |	    Name	= json2:obj_fetch("name",Json, <<>>),
```

1. Notice the include_dir imports due to using rebar3
2. Notice the new rest_airport.hrl to extract record-definition
3. Notice use of different api rfc4627 vs json2
   NOTE: The alteration in yaws/json2.erl is needed for this code



Build
-----
```bash
$ rebar3 get-deps
$ rebar3 compile
```
    
Running
--------
```bash
$ rebar3 shell
```

Then inside the shell execute:
```erlang   
> ybed_sup:start_link().
```


Testing
-------

1. GET:
```bash
$ curl -X 'GET' 'http://localhost:8888' -H 'Content-Type: application/json'
{"data":""}
```

2. POST
```bash
curl -X 'POST' 'http://localhost:8888' -H 'Content-Type: application/json' -d '{ "airport": "JFK", "city": "New York", "country": "US", "name": "John F Kennedy" }'
{ "airport": "JFK", "city": "New York", "country": "US", "name": "John F Kennedy" }
```

3. GET
```bash
curl -X 'GET' 'http://localhost:8888' -H 'Content-Type: application/json'
{"data": [{ "airport": "JFK", "city": "New York", "country": "US", "name": "John F Kennedy" }]
```

Links
------
 - http://yaws.hyber.org/embed.yaws
 - https://www.oreilly.com/library/view/building-web-applications/9781449320621/ch04.html#rest