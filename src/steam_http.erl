-module(steam_http).
-include("../include/steam_urls.hrl").
-include("../include/http_response.hrl").
%% API
-export([
  mobile_request/5,
  post/4,
  post/5,
  get/1,
  get/3,
  get/4,
  post/2
]).

mobile_request(URL, Method, Data, Cookies, Headers) ->
  request(URL, Method, Data, Cookies, Headers, ?MOBILE_LOGIN_OAUTH).

request(URL, Method, Data, Cookies, Headers) ->
  request(URL, Method, Data, Cookies, Headers, ?STEAM_COMMUNITY_BASE).

request(URL, Method, Data, Cookies, Headers, null) ->
  request(URL, Method, Data, Cookies, Headers);
request(URL, Method, Data, Cookies, Headers, Referer) ->
  CustomHeaders = Headers ++ [
    {<<"Referer">>, list_to_binary(Referer)}
  ],
  Options = [{cookie, Cookies}],
  {ok, StatusCode, ResponseHeaders, ClientRef} = hackney:request(Method, URL, CustomHeaders, Data, Options),
  {ok, RawBody} = hackney:body(ClientRef),
  ResponseCookies = hackney:cookies(ResponseHeaders),
  Response = #http_response{status_code = StatusCode, headers = ResponseHeaders, raw_body = RawBody, cookies = ResponseCookies},
  try jiffy:decode(RawBody, [return_maps]) of
    Body -> Response#http_response{json_body = Body}
  catch
    _ -> Response
  end.


post(URL, Data) ->
  post(URL, Data, [], []).

post(URL, Data, Cookies, Headers) ->
  post(URL, Data, Cookies, Headers, null).

post(URL, Data, Cookies, Headers, Referer) ->
  CustomHeaders = Headers ++ [
    {<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=UTF-8">>},
    {<<"Content-Length">>, size(Data)}
  ],
  request(URL, post, Data, Cookies, CustomHeaders, Referer).


get(URL) ->
  get(URL, [], []).

get(URL, Cookies, Headers) ->
  get(URL, Cookies, Headers, null).

get(URL, Cookies, Headers, Referer) ->
  request(URL, get, <<>>, Cookies, Headers, Referer).
