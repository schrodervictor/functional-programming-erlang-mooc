-module(index).
-export([main/1, get_file_contents/1, show_file_contents/1]).

-type line_range() :: {integer(), integer()}.
-type index_entry() :: {string(), [line_range()]}.
-type word_occurences() :: {string(), [integer()]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (public) main/1
%%
%% This is the main function (literally) of this module. It's purpose is to
%% return a list of index entries, each defined as a tuple of a word and the
%% occurences ranges in the text, according to the line numbers in which the
%% word appears.
%%
%% The function has two signatures, accepting:
%%     - tagged tuple with a filename to be processed
%%     - the list of strings that form a text to be processed
%%
-spec main([string()]) -> [index_entry()];
          ({file, string()}) -> [index_entry()].

main({file, Filename}) ->
    main(get_file_contents(Filename));
main(Lines) ->
    collect_words(Lines, 0, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) collect_words/3
%%
%% Recursive function that holds the main logic of the indexer.
%% Traverses the list of strings, always getting the first word of such line
%% and adding it to the accumulator.
%%
%% It will strip out all words smaller than 3 chars.
%%
-spec collect_words([string()], integer(), [index_entry()]) ->
    [index_entry()].

collect_words([], _, Acc) ->
    sort_index_entries(compact_ranges(Acc));
collect_words([[]|Lines], LineNum, Acc) ->
    collect_words(Lines, LineNum + 1, Acc);
collect_words([Line|Lines], LineNum, Acc) ->
    {Word, RestLine} = split_at_first_word(Line),
    NormWord = to_lower(Word),
    NewAcc = case length(NormWord) > 2 of
        true -> add_word(NormWord, LineNum, Acc);
        false -> Acc
    end,
    collect_words([RestLine|Lines], LineNum, NewAcc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) split_at_first_word/1
%%
%% Entry point of the recursive version split_at_first_word/2
%%
-spec split_at_first_word(string()) -> {string(), string()}.

split_at_first_word(Line) ->
    split_at_first_word([], Line).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) split_at_first_word/2
%%
%% Return a tuple with the first word (everything from the beginning until the
%% first non-alphanumeric character) and the rest of the string (without the
%% first non-alphanumeric character found)
%%
-spec split_at_first_word(string(), string()) -> {string(), string()}.

split_at_first_word(Word, []) ->
    {lists:reverse(Word), []};
split_at_first_word(Chars, [Char|Line]) ->
    case is_alphanumeric(Char) of
        true -> split_at_first_word([Char|Chars], Line);
        false -> {lists:reverse(Chars), Line}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) is_alphanumeric/1
%%
-spec is_alphanumeric(char()) -> boolean().

is_alphanumeric(Char) when Char >= $a andalso Char =< $z -> true;
is_alphanumeric(Char) when Char >= $A andalso Char =< $Z -> true;
is_alphanumeric(Char) when Char >= $0 andalso Char =< $9 -> true;
is_alphanumeric(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) to_lower/1
%%
-spec to_lower(string()) -> string().

to_lower([]) -> [];
to_lower([Char|Chars]) when Char >= $A andalso Char =< $Z ->
    [Char + 32|to_lower(Chars)];
to_lower([Char|Chars]) ->
    [Char|to_lower(Chars)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) add_word/1
%%
%% Adds a word occurence to the index. If the word is not present, it will
%% generate a new index entry. Each entry is formed by a tuple of the word
%% and a list of lines with the occurences in reverse order and with
%% duplicates
%%
-spec add_word(string(), integer(), [word_occurences()]) -> [word_occurences()].

add_word([], _, Results) -> Results;
add_word(Word, Line, []) -> [{Word, [Line]}];
add_word(Word, Line, [{Word, Lines}|Results]) ->
    [{Word, [Line|Lines]}|Results];
add_word(Word, Line, [Entry|Results]) ->
    [Entry|add_word(Word, Line, Results)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) compact_ranges/1
%%
%% Transforms a word occurences list into a index entries lists, by compacting
%% the plain list of occurences of each word into a correspondent list of
%% tuples in the format {Begin, End}
%%
-spec compact_ranges([word_occurences()]) -> [index_entry()].

compact_ranges([]) -> [];
compact_ranges([Entry|Entries]) ->
    [compact_range(Entry)|compact_ranges(Entries)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) compact_range/1
%%
%% Singular version of compact_ranges/1. This function treats each word
%% occurence list to transfort it into a index entry with range format.
%%
%% This is the entry point for the compact_range/2 function
%%
-spec compact_range(word_occurences()) -> index_entry().

compact_range({Word, Lines}) -> {Word, compact_range(Lines, [])}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) compact_range/2
%%
%% Transforms a single list of line occurences into a list of line ranges
%%
-spec compact_range([integer()], [line_range()]) -> [line_range()].

compact_range([], Ranges) ->
    Ranges;
compact_range([Line|Lines], Ranges = [{Line, _}|_]) ->
    compact_range(Lines, Ranges);
compact_range([Line|Lines], [{RangeBegin, RangeEnd}|Ranges])
    when Line == RangeBegin - 1 ->
    compact_range(Lines, [{Line, RangeEnd}|Ranges]);
compact_range([Line|Lines], Ranges) ->
    compact_range(Lines, [{Line, Line}|Ranges]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) sort_index_entries/1
%%
%% Takes a list of index entries and sort it lexicographically.
%% The sorting strategy used here is a classical insert sort algorithm
%%
-spec sort_index_entries([index_entry()]) -> [index_entry()].

sort_index_entries([]) -> [];
sort_index_entries([Head|Tail]) ->
    insert(Head, sort_index_entries(Tail)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) insert/2
%%
%% Inner function used by sort_index_entries/1 to perform insert sorting
%%
-spec insert(index_entry(), [index_entry()]) -> [index_entry()].

insert(Elem, []) -> [Elem];
insert(Elem, [Head|OrdList]) ->
    case compare_index_entries(Elem, Head) of
        true -> [Elem, Head|OrdList];
        false -> [Head|insert(Elem, OrdList)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) compare_index_entries/2
%%
%% Sorting function used to compare two index entries. Returns true if the
%% word of the first Argument is lexicographically smaller or false, otherwise
%%
-spec compare_index_entries(index_entry(), index_entry()) -> boolean().

compare_index_entries({Word0, _}, {Word1, _}) ->
    compare_words(Word0, Word1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (private) compare_words/2
%%
%% Helper function to compare two words. Returns true if the first word is
%% lexicographically smaller or false, otherwise
%%
-spec compare_words(string(), string()) -> boolean().

compare_words([], _) -> true;
compare_words(_, []) -> false;
compare_words([Char|Word0], [Char|Word1]) -> compare_words(Word0, Word1);
compare_words([Char0|_], [Char1|_]) -> Char0 < Char1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDEFINED FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

-spec get_file_contents(string()) -> [string()].

get_file_contents(Name) ->
    {ok, File} = file:open(Name, [read]),
    Rev = get_all_lines(File, []),
    lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

-spec get_all_lines(file:io_device(), [string()]) -> [string()].

get_all_lines(File, Partial) ->
    case io:get_line(File, "") of
        eof ->
            file:close(File),
            Partial;
        Line ->
            {Strip, _} = lists:split(length(Line) - 1, Line),
            get_all_lines(File, [Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

-spec show_file_contents([string()]) -> ok.

show_file_contents([]) -> ok;
show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls).
