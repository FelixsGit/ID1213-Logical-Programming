flatten2(List,Result):-
    flatten2Helper(List,[],FlattenResult),
    reverse(FlattenResult,Result).
flatten2Helper([],FlattenResult,FlattenResult).
flatten2Helper([[Head|Tail]|T],Acc,FlattenResult):-
    flatten2Helper(Tail,[Head|[]],FlattenHeadResult),
    append2(FlattenHeadResult,Acc,ResultToSend),
    flatten2Helper(T,ResultToSend,FlattenResult).
flatten2Helper([H|T],Acc,FlattenResult):-
    flatten2Helper(T,[H|Acc],FlattenResult).

append2(L1,L2,Result):-
    reverse2(L1,RevResult),
    append2Helper(RevResult,L2,Result).
append2Helper([],AppResult,AppResult).
append2Helper([H|T],Acc,AppResult):-
    append2Helper(T,[H|Acc],AppResult).

reverse2(L,Result):-
    reverse2Helper(L,[],Result).
reverse2Helper([],RevResults,RevResults).
reverse2Helper([H|T],Acc,RevResults):-
    reverse2Helper(T,[H|Acc],RevResults).

sumList(List,Result):-
   sumListHelper(List,0,Result).
addition(NumOne,NumTwo,Result):-
    Result is NumOne+NumTwo.
sumListHelper([],Sum,Sum).
sumListHelper([Head|Tail],Sum,Result):-
    addition(Head,Sum,AdditionResult),
    sumListHelper(Tail,AdditionResult,Result).

listLenght2(List,Result):-
    listLength2Helper(List,0,Result).
listLength2Helper([],Length,Length).
listLength2Helper([_|Tail],Length,Result):-
    addition(1,Length,AdditionResult),
    listLength2Helper(Tail,AdditionResult,Result).

member2([X|_],X).
member2([_|T],X):-
    member2(T,X).

lenght2(L,Result):-
    lenght2Helper(L,0,Result).
lenght2Helper([],Result,Result).
lenght2Helper([_|T],Lenght,Result):-
    NewLenght is Lenght + 1,
    lenght2Helper(T,NewLenght,Result).
