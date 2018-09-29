/*Calling predicate with a string example: "hello" will huffman encode the string.*/
huffman(Msg,Msg,SortedFrequencyList,Tree,Table,Bitsequence,DMsg):-
    atom_chars(Msg,ListOfChars),
    frequency(ListOfChars,[],FrequencyList),
    sortByFreq(FrequencyList,[],SortedFrequencyList),
    huffman_tree(SortedFrequencyList,Tree),
    encode_table(Tree,Table),
    encode(ListOfChars,Table,Bitsequence),
    decode(Bitsequence,Table,DecodedMsg),
    atom_chars(DMsg,DecodedMsg).

frequency([],FrequencyList,FrequencyList).
frequency([Char|Rest],FrequencyList,Result):-
     update(FrequencyList,Char,UpdatedList),
     frequency(Rest,UpdatedList,Result).

update([],Char,Result):-
    Result = [[Char,1]].
update([[Char,Freq]|Rest],Char,Result):-
        NewFreq is Freq + 1,
        Result = [[Char,NewFreq]|Rest].
update([Head|Rest],Char,Result):-
    update(Rest,Char,NewResults),
    Result = [Head|NewResults].

sortByFreq([],SortedFrequencyList,SortedFrequencyList).
sortByFreq([Head|Rest],SortedFrequencyList,Result):-
    findBiggestElem([Head|Rest],ElemToAdd,SortedFrequencyList),
    delete3(ElemToAdd,[Head|Rest],[],UpdatedList),
    sortByFreq(UpdatedList,[ElemToAdd|SortedFrequencyList],Result).

findBiggestElem([[Head,Freq]|T], Y,SortedFrequencyList):-
    findBiggestElemH(T,[Head,Freq],Y,SortedFrequencyList).

findBiggestElemH([[Head,Freq]|T],[HHead,HFreq],Highest,SortedFrequencyList):-
    (Freq > HFreq ->findBiggestElemH(T,[Head,Freq], Highest,SortedFrequencyList);
    findBiggestElemH(T,[HHead,HFreq],Highest,SortedFrequencyList)).
findBiggestElemH([],X,X,_).

delete3(_,[],Result,Result).
delete3(X,[H|T],OKList,Result):-
    (X \= H ->delete3(X,T,[H|OKList],Result);
    delete3(X,T,OKList,Result)).

/*This function compress two tuples into one, over and over untill the whole tree is in one large tuple.*/
huffman_tree([[C1,_]],Result):-
    Result = C1.
huffman_tree([[C1,F1],[C2,F2]|Rest],Result):-
    NewFreq is F1 + F2,
    insert([[C1,C2],NewFreq],Rest,InsertResult),
    huffman_tree(InsertResult,Result).

/*This function insertes the newly created List at the correct spot , so that the
 lists stayes sorted*/
insert([C,F],[[CH,FH]|T],Result):-
    (F=<FH ->Result = [[C,F],[CH,FH]|T];
     insert([C,F],T,InsertResult),
     Result = [[CH,FH] | InsertResult]).
insert([C,F],[],Result):-
     Result = [[C,F]].

encode_table(Tree,Result):-
    tree_to_table(Tree,[],[],Result).
tree_to_table([],_,Acc,Result):-
    Result = Acc.
tree_to_table([Left,Right],Path,Acc,Result):-
    append(Path,[0],AppendInnerLeftResult),
    append(Path,[1],AppendInnerRightResult),
    tree_to_table(Left,AppendInnerLeftResult,Acc,TreeToTableLeftResult),
    tree_to_table(Right,AppendInnerRightResult,Acc,TreeToTableRightResult),
    append(TreeToTableLeftResult,TreeToTableRightResult,MiddleAppendResult),
    append(MiddleAppendResult,Acc,Result).
tree_to_table(Char,Path,_,Result):-
    Result = [[Char,Path]].


encode([],_,Result):-
    Result = [].
encode([Char|Rest],Table,Result):-
    encode_char(Char,Table,EncodeCharResult),
    encode(Rest,Table,EncodeResults),
    append(EncodeCharResult,EncodeResults,Result).
encode_char(Char,[[Char,Path]|_],Result):-
    Result = Path.
encode_char(Char,[[_,_]|Rest],Result):-
    encode_char(Char,Rest,Result).


decode([],_,Result):-
    Result = [].
decode(Seq,Table,Result):-
    decode_char(Seq,1,Table,DecodeCharResult,Char,Rest),
    (DecodeCharResult->decode(Rest,Table,DecodeResult), Result=[Char|DecodeResult]).
decode_char(Seq,N,Table,Result,CharR,RestR):-
    split(Seq,N,[],SplitResult,Code,Rest),
    SplitResult,
    (SplitResult == false->Result=true;
       keyFind(Table,Code,KeyFindResult,Char),
           (KeyFindResult->Result=true,CharR=Char,RestR=Rest;
    NewN is N + 1,decode_char(Seq,NewN,Table,Result,CharR,RestR))).

split([],N,NewList,Result,Code,Rest):-
    length(NewList,LenghtOfNewList),
    (LenghtOfNewList == N ->reverse(NewList,ReversedList),Result=true,Code=ReversedList,Rest=[]).
split([Head|Tail],N,NewList,Result,Code,Rest):-
    length(NewList,LenghtOfNewList),
    (LenghtOfNewList == N ->reverse(NewList,ReversedList),Result=true,Code=ReversedList,Rest=[Head|Tail];
    split(Tail,N,[Head|NewList],Result,Code,Rest)).

keyFind([],_,Result,_):-Result=false.
keyFind([[Char,Seq]|Rest],Code,Result,Item):-
    (Seq == Code ->Result=true,Item=Char;
          keyFind(Rest,Code,Result,Item)).


