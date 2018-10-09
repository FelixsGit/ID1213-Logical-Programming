/*This file contains a complete huffman encoder and decoder
 * with outputs printed to the console.*/

/*Call this function to run the program*/
huffman(Msg):-
    atom_chars(Msg,ListOfChars),
    frequency(ListOfChars,[],FrequencyList),
    sortByFreq(FrequencyList,[],SortedFrequencyList),
    huffman_tree(SortedFrequencyList,Tree),
    encode_table(Tree,Table),
    encode(ListOfChars,Table,Bitsequence),
    decode(Bitsequence,Table,DecodedMsg),
    atom_chars(Dmsg,DecodedMsg),
    write("Message":Msg),nl,
    write("FrequencyList":SortedFrequencyList),nl,
    write("Tree":Tree),nl,
    write("Table":Table),nl,
    write("Bitsequence":Bitsequence),nl,
    write("DecodedMessage":Dmsg).

/*This function saves the each char with its frequency in a list*/
frequency([],FrequencyList,FrequencyList).
frequency([Char|Rest],FrequencyList,Result):-
     update(FrequencyList,Char,UpdatedList),
     frequency(Rest,UpdatedList,Result).

/*This function places a the char into a the list and returns the updated list*/
update([],Char,Result):-
    Result = [[Char,1]].
update([[Char,Freq]|Rest],Char,Result):-
        NewFreq is Freq + 1,
        Result = [[Char,NewFreq]|Rest].
update([Head|Rest],Char,Result):-
    update(Rest,Char,NewResults),
    Result = [Head|NewResults].

/*This function sorts the entered frequncy list*/
sortByFreq([],SortedFrequencyList,SortedFrequencyList).
sortByFreq([Head|Rest],SortedFrequencyList,Result):-
    findBiggestElem([Head|Rest],ElemToAdd,SortedFrequencyList),
    delete3(ElemToAdd,[Head|Rest],[],UpdatedList),
    sortByFreq(UpdatedList,[ElemToAdd|SortedFrequencyList],Result).

/*This function find the biggest frequncy in a frequncy list*/
findBiggestElem([[Head,Freq]|T], Y,SortedFrequencyList):-
    findBiggestElemH(T,[Head,Freq],Y,SortedFrequencyList).
findBiggestElemH([[Head,Freq]|T],[HHead,HFreq],Highest,SortedFrequencyList):-
    (Freq > HFreq ->findBiggestElemH(T,[Head,Freq], Highest,SortedFrequencyList);
    findBiggestElemH(T,[HHead,HFreq],Highest,SortedFrequencyList)).
findBiggestElemH([],X,X,_).

/*This function deletes the entered element and returns the updated list*/
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

/*These functions transform a given huffman tree into an huffman table*/
encode_table(Tree,Result):-
    tree_to_table(Tree,[],[],Result).
tree_to_table([],_,Acc,Result):-
    Result = Acc.
tree_to_table([Left,Right],Path,Acc,Result):-
    append2(Path,[0],AppendInnerLeftResult),
    append2(Path,[1],AppendInnerRightResult),
    tree_to_table(Left,AppendInnerLeftResult,Acc,TreeToTableLeftResult),
    tree_to_table(Right,AppendInnerRightResult,Acc,TreeToTableRightResult),
    append2(TreeToTableLeftResult,TreeToTableRightResult,MiddleAppendResult),
    append2(MiddleAppendResult,Acc,Result).
tree_to_table(Char,Path,_,Result):-
    Result = [[Char,Path]].

/*This function encodes the given huffman table into a list containing the tables bitsequence*/
encode([],_,Result):-
    Result = [].
encode([Char|Rest],Table,Result):-
    encode_char(Char,Table,EncodeCharResult),
    encode(Rest,Table,EncodeResults),
    append2(EncodeCharResult,EncodeResults,Result).
encode_char(Char,[[Char,Path]|_],Result):-
    Result = Path.
encode_char(Char,[[_,_]|Rest],Result):-
    encode_char(Char,Rest,Result).


/*This function decodes a bitsequency by using the entered huffman table and returning the decoded message*/
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

/*This function splits a list and returns a list with the first N elements from the entered list.*/
split([],N,NewList,Result,Code,Rest):-
    lenght2(NewList,LenghtOfNewList),
    (LenghtOfNewList == N ->reverse2(NewList,ReversedList),Result=true,Code=ReversedList,Rest=[]).
split([Head|Tail],N,NewList,Result,Code,Rest):-
    lenght2(NewList,LenghtOfNewList),
    (LenghtOfNewList == N ->reverse2(NewList,ReversedList),Result=true,Code=ReversedList,Rest=[Head|Tail];
    split(Tail,N,[Head|NewList],Result,Code,Rest)).

/*This function returns a Char from a huffman table that has a bitsequence that match the entered Code*/
keyFind([],_,Result,_):-Result=false.
keyFind([[Char,Seq]|Rest],Code,Result,Item):-
    (Seq == Code ->Result=true,Item=Char;
          keyFind(Rest,Code,Result,Item)).

/*This function returns a single list containing the entered two list*/
append2(L1,L2,Result):-
    reverse2(L1,RevResult),
    append2Helper(RevResult,L2,Result).
append2Helper([],AppResult,AppResult).
append2Helper([H|T],Acc,AppResult):-
    append2Helper(T,[H|Acc],AppResult).

/*This function returns the entered list reversed*/
reverse2(L,Result):-
    reverse2Helper(L,[],Result).
reverse2Helper([],RevResults,RevResults).
reverse2Helper([H|T],Acc,RevResults):-
    reverse2Helper(T,[H|Acc],RevResults).

/*This function return the length of the entered list*/
lenght2(L,Result):-
    lenght2Helper(L,0,Result).
lenght2Helper([],Result,Result).
lenght2Helper([_|T],Lenght,Result):-
    NewLenght is Lenght + 1,
    lenght2Helper(T,NewLenght,Result).
