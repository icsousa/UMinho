-module(myqueue).
-export([create/0, enqueue/2, dequeue/1, test/0]).

create() -> [].

enqueue(Queue, Item) ->
    Queue ++ [Item].

dequeue([]) ->
    empty;
dequeue([Item|Queue]) ->
    {Queue, Item}.

test() ->
    io:format("Create queue~n"),
    Q0 = create(),
    io:format("Q0 = ~p~n", [Q0]),

    io:format("Dequeue empty queue~n"),
    empty = dequeue(Q0),

    io:format("Enqueue 1~n"),
    Q1 = enqueue(Q0, 1),
    io:format("Q1 = ~p~n", [Q1]),

    io:format("Enqueue 2~n"),
    Q2 = enqueue(Q1, 2),
    io:format("Q2 = ~p~n", [Q2]),

    io:format("Enqueue 3~n"),
    Q3 = enqueue(Q2, 3),
    io:format("Q3 = ~p~n", [Q3]),

    io:format("Dequeue -> remove first element~n"),
    {Q4, Item1} = dequeue(Q3),
    io:format("Removed = ~p, New Queue = ~p~n", [Item1, Q4]),

    io:format("Dequeue again~n"),
    {Q5, Item2} = dequeue(Q4),
    io:format("Removed = ~p, New Queue = ~p~n", [Item2, Q5]),

    io:format("Enqueue 4~n"),
    Q6 = enqueue(Q5, 4),
    io:format("Q6 = ~p~n", [Q6]),

    io:format("Enqueue 5~n"),
    Q7 = enqueue(Q6, 5),
    io:format("Q7 = ~p~n", [Q7]),

    io:format("Final dequeues~n"),
    {Q8, Item3} = dequeue(Q7),
    io:format("Removed = ~p, Queue = ~p~n", [Item3, Q8]),

    {Q9, Item4} = dequeue(Q8),
    io:format("Removed = ~p, Queue = ~p~n", [Item4, Q9]),

    {Q10, Item5} = dequeue(Q9),
    io:format("Removed = ~p, Queue = ~p~n", [Item5, Q10]),

    empty = dequeue(Q10),

    io:format("Queue empty again~n"),
    ok.