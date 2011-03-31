-module(fel_file_reader).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-include("front_end_loader.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, get_next_request/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(LogFileName) ->
  gen_server:start_link(?MODULE, [LogFileName], []).
  
get_next_request(FileReader) ->
  gen_server:call(FileReader, get_next_request).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record (ffr_state, {regexes, log_file, done=false}).

init(LogFileName) ->
  {ok, LogFile} = file:open(LogFileName, [raw, read_ahead]),
  % 10.61.104.254 - - [23/Mar/2011:00:00:01 +0000] "GET /app_metrics HTTP/1.1" 200 458 "-" "curl/7.15.5 (x86_64-redhat-linux-gnu) libcurl/7.15.5 OpenSSL/0.9.8b zlib/1.2.3 libidn/0.6.5"
  % handle combined log format
  LogPattern = "(?<host>.+?)\\s(?<remote_logname>.+?)\\s(?<remote_user>.+?)\\s\\[(?<time>.+?)\\]\\s\"(?<request>.+?)\"\s(?<status>\\d+)\\s(?<response_bytesize>\\d+|-)\\s\"(?<referrer>.+?)\"\\s\"(?<user_agent>.+?)\"",
  {ok, CompiledLogLineRe} = re:compile(LogPattern),

  % 23/Mar/2011:00:00:01 +0000
  TimePattern = "(\\d+)/(\\w{3})/(\\d{4,}):(\\d{2}):(\\d{2}):(\\d{2}) ([+|-]?\\d{4})",
  {ok, CompiledTimeRe} = re:compile(TimePattern),

  % GET /app_metrics HTTP/1.1
  RequestPattern = "(\\w+)\\s([^\s]+)(?:\\s(.+))?$",
  {ok, RequestRe} = re:compile(RequestPattern),

  State = #ffr_state{
    regexes=[{log_line, CompiledLogLineRe}, {time, CompiledTimeRe}, {request, RequestRe}], 
    log_file=LogFile},
  {ok, State}.

handle_call(get_next_request, _From, State = #ffr_state{done = true}) ->
  {reply, eof, State};
handle_call(get_next_request, From, State) ->
  LogFile = State#ffr_state.log_file,
  Result = file:read_line(LogFile),
  case Result of
    {ok, Line}  ->
      spawn_link(fun() ->
        gen_server:reply(From, create_log_line(Line, State#ffr_state.regexes))
      end),
      {noreply, State};
    eof ->
      file:close(LogFile),
      DoneState = State#ffr_state{done = true},
      {reply, eof, DoneState}
  end.

handle_cast(_Msg, _State) ->
  error(not_implemented).

handle_info(_Info, _State) ->
    error(not_implemented).

terminate(_Reason, #ffr_state{done=true}) ->
  ok;
terminate(_Reason, State) ->
  LogFile = State#ffr_state.log_file,
  file:close(LogFile).

code_change(_OldVsn, LogFile, _Extra) ->
  {ok, LogFile}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
create_log_line(eof, _Regexes) ->
  eof;
create_log_line(Line, Regexes) ->
  LogRegexes = proplists:get_value(log_line, Regexes),
  {match, Matches} = re:run(Line, LogRegexes, [{capture, all_but_first, binary}]),
  [Host, RemoteLogname, RemoteUser, Time, Request, Status, ResponseBytesize, Referrer, UserAgent] = Matches,
  #log_line{
    host              = blank_to_undefined(Host), 
    remote_logname    = blank_to_undefined(RemoteLogname), 
    remote_user       = blank_to_undefined(RemoteUser), 
    time              = parse_date_time(Time, proplists:get_value(time, Regexes)), 
    request           = create_request(Request, proplists:get_value(request, Regexes)), 
    status            = to_i(binary_to_list(Status)), 
    response_bytesize = to_i(binary_to_list(ResponseBytesize)), 
    referrer          = blank_to_undefined(Referrer), 
    user_agent        = blank_to_undefined(UserAgent)
  }.
  
blank_to_undefined(<<"-">>) ->
  undefined;
blank_to_undefined(Anything) ->
  Anything.
  
parse_date_time(DateTimeStr, Regex) ->
  % 23/Mar/2011:00:00:01 +0000
  {match, Matches} = re:run(DateTimeStr, Regex, [{capture, all_but_first, list}]),
  [DateStr, MonthStr, YearStr, HourStr, MinuteStr, SecondStr, "+0000"] = Matches, %barf if TZ isn't UTC (+0000)
  Date = {to_i(YearStr), to_i(MonthStr), to_i(DateStr)},  
  Time = {to_i(HourStr), to_i(MinuteStr), to_i(SecondStr)},  
  {Date, Time}.

to_i(S) ->
  case string:to_lower(S) of
    "jan" -> 1;
    "feb" -> 2;
    "mar" -> 3;
    "apr" -> 4;
    "may" -> 5;
    "jun" -> 6;
    "jul" -> 7;
    "aug" -> 8;
    "sep" -> 9;
    "oct" -> 10;
    "nov" -> 11;
    "dec" -> 12;
    I   -> 
      {N, _} = string:to_integer(I),
      N
  end.
  
create_request(Request, Regex) ->
  REResult = re:run(Request, Regex, [{capture, all_but_first, binary}]),
  case REResult of
    {match, [Method, Path, HttpVersion]} ->
      #request{method=Method, path=Path, http_version=HttpVersion};
    {match, [Method, Path]} ->
      #request{method=Method, path=Path};
    nomatch ->
      undefined
  end.
  