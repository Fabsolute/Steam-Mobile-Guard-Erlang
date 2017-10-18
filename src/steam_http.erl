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
  post/2,
  mobile_get/3,
  mobile_post/4
]).


mobile_post(URL, Data, Cookies, Headers) ->
  mobile_request(URL, post, Data, Cookies, Headers).


mobile_get(URL, Cookies, Headers) ->
  mobile_request(URL, get, [], Cookies, Headers).


mobile_request(URL, Method, Data, Cookies, Headers) ->
  request(URL, Method, Data, Cookies, Headers, ?STEAM_COMMUNITY_BASE).


request(URL, Method, Data, Cookies, Headers) ->
  request(URL, Method, Data, Cookies, Headers, ?STEAM_COMMUNITY_BASE).

request(URL, Method, Data, Cookies, Headers, null) ->
  request(URL, Method, Data, Cookies, Headers);
request(URL, Method, Data, Cookies, Headers, Referer) ->
  QueryData = hackney_url:qs(Data),
  CustomHeaders = Headers ++ [
    {<<"Referrer">>, list_to_binary(Referer)}
  ] ++ case Method of
         post -> [
           {<<"Content-Type">>, <<"application/x-www-form-urlencoded">>},
           {<<"Content-Length">>, size(QueryData)}
         ];
         _ -> []
       end,
  Options = [{cookie, Cookies}],

  {ok, StatusCode, ResponseHeaders, ClientRef} = hackney:request(Method, URL, CustomHeaders, QueryData, Options),
  {ok, RawBody} = hackney:body(ClientRef),
  ResponseCookies = clean_cookies(hackney:cookies(ResponseHeaders)),
  Response = #http_response{status_code = StatusCode, headers = ResponseHeaders, raw_body = RawBody, cookies = ResponseCookies},

  Output = try jiffy:decode(RawBody, [return_maps]) of
             Body -> Response#http_response{json_body = Body}
           catch
             _ -> Response
           end,
  Output.

post(URL, Data) ->
  post(URL, Data, [], []).

post(URL, Data, Cookies, Headers) ->
  post(URL, Data, Cookies, Headers, null).

post(URL, Data, Cookies, Headers, Referer) ->
  request(URL, post, Data, Cookies, Headers, Referer).


get(URL) ->
  get(URL, [], []).

get(URL, Cookies, Headers) ->
  get(URL, Cookies, Headers, null).

get(URL, Cookies, Headers, Referer) ->
  request(URL, get, <<>>, Cookies, Headers, Referer).



clean_cookies(Cookies) ->
  clean_cookies(Cookies, []).

clean_cookies([], Response) ->
  Response;
clean_cookies([{Name, [{Name, Value}, _]} | T], Response) ->
  clean_cookies(T, [{Name, Value} | Response]);

clean_cookies([_ | T], Response) ->
  clean_cookies(T, Response).