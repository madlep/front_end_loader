-module(front_end_loader).

-export([main/0]).

-include("front_end_loader.hrl").

main() ->
  {ok, FileReaderPid} = fel_file_reader:start_link("tmp/example_logs/atlas-repo.prod.lpo.log"),
  read_all_from_file(FileReaderPid).
  
read_all_from_file(FileReaderPid) ->
  read_all_from_file(fel_file_reader:get_next_request(FileReaderPid), FileReaderPid, 0).
  
read_all_from_file(eof, _FileReaderPid, LineCount) ->
  io:format("~neof. Done! ~w lines parsed~n", [LineCount]),
  halt();
read_all_from_file(Line, FileReaderPid, LineCount) ->
  io:format("~s~n", [Line#log_line.request#request.path]),
  read_all_from_file(fel_file_reader:get_next_request(FileReaderPid), FileReaderPid, LineCount + 1).
