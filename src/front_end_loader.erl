-module(front_end_loader).

-export([main/0]).

main() ->
  {ok, FileReaderPid} = fel_file_reader:start_link("tmp/example_logs/atlas-repo.prod.lpo.log"),
  read_all_from_file(FileReaderPid).
  
read_all_from_file(FileReaderPid) ->
  read_all_from_file(fel_file_reader:get_next_request(FileReaderPid), FileReaderPid).
  
read_all_from_file(eof, _FileReaderPid) ->
  io:format("~neof. Done!~n"),
  halt();
read_all_from_file(Line, FileReaderPid) ->
  io:format("~p~n", [Line]),
  read_all_from_file(fel_file_reader:get_next_request(FileReaderPid), FileReaderPid).
