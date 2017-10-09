-module(steam_http).

%% API
-export([request/1, request/2, request/4, request/5]).

request(Url) ->
  get_body(httpc:request(Url)).
request(Url, Profile) ->
  get_body(httpc:request(Url, Profile)).
request(Method, Request, HttpOptions, Options) ->
  get_body(httpc:request(Method, Request, HttpOptions, Options)).
request(Method, Request, HTTPOptions, Options, Profile) ->
  get_body(httpc:request(Method, Request, HTTPOptions, Options, Profile)).

get_body(Response) ->
  ResponseObject = case Response of
                     {ok, Result} ->
                       case Result of
                         {_Status, _Headers, Body} -> jiffy:decode(Body, [return_maps]);
                         {_Status, Body} -> jiffy:decode(Body, [return_maps])
                       end
                   end,
  ResponseObject.