start_A_star( InitState, PathCost, N,  MaxCount) :-
	score(InitState, 0, 0, InitCost, InitScore) ,
	search_A_star( [node(InitState, nil, nil, InitCost , InitScore ) ], [ ], PathCost, N, 0, MaxCount) .


search_A_star(Queue, ClosedSet, PathCost, NodesNumber, Step, MaxCount) :-
	Step =< MaxCount,
	newFetch(ClosedSet, Queue, NodesNumber, Nodes),
	printNodes(Nodes, Step),
	getUserInput(OrderList),
	getNodeNumber(OrderList, NodeNumber),
	getNElementOfList(Nodes, NodeNumber, Node),
	continue(Node, Queue, ClosedSet, PathCost, NodesNumber, Step, MaxCount).

search_A_star(Queue, ClosedSet, PathCost, NodesNumber, Step, MaxCount) :-
	Step > MaxCount,
	getUserDecision(Decision),
	Decision == tak,
	NewMaxCount is MaxCount + 1,
	newFetch(ClosedSet, Queue, NodesNumber, Nodes),
	NewStep is Step + 1,
	printNodes(Nodes, NewStep),
    getUserInput(OrderList),
    getNodeNumber(OrderList, NodeNumber),
    getNElementOfList(Nodes, NodeNumber, Node),
    continue(Node, Queue, ClosedSet, PathCost, NodesNumber, NewStep, NewMaxCount).


continue(node(State, Action, Parent, Cost, _), _, ClosedSet, path_cost(Path, Cost), _, _,_) :-
	goal( State), !,
	build_path(node(Parent, _ ,_ , _ , _ ) , ClosedSet, [Action/State], Path) .

continue(Node, RestQueue, ClosedSet, Path, NodesNumber, Step, MaxCount)   :-
	expand(Node, NewNodes),
	insert_new_nodes(NewNodes, RestQueue, NewQueue),
	NewStep is Step + 1,
	search_A_star(NewQueue, [Node | ClosedSet ], Path, NodesNumber, NewStep, MaxCount).

getUserDecision(Decision) :-
	writeln('Czy zwiekszyc limit krokow?'),
	nl,
	read(Decision).

newFetch(_, [], _, []).

newFetch(_, _, 0, []).

newFetch(ClosedSet, [node(State, Action, Parent, Cost, Score) | Rest], NodesNumber,
        [node(State,Action, Parent,Cost,Score) | Nodes]) :-
    \+ member(node(State, _, _, _, _), ClosedSet), !,
    NodesNumber2 is NodesNumber - 1,
    newFetch(ClosedSet, Rest, NodesNumber2, Nodes).

newFetch(ClosedSet, [_| Rest], NodesNumber,Nodes) :-
    newFetch(ClosedSet, Rest, NodesNumber, Nodes).

printNodes([], Step) :-
	write('Krok algorytmu: '),
	write(Step),
	nl.

printNodes([node(State, Action, Parent, Cost, Score) | Rest], Step) :-
			write('Node: '),
			write(State), write(' '),
			write(Action), write(' '),
			write(Parent), write(' '),
			write(Cost), write(' '),
			write(Score), write(' '),
			nl,
			printNodes(Rest, Step).


getNodeNumber([Node | _], Node).

getNodeNumber([_|List], Node) :-
	getNodeNumber(List, Node).

getNElementOfList([], _, []).

getNElementOfList([_|List], NumberOfElement, Y) :-
	NumberOfElement2 is NumberOfElement - 1,
	NumberOfElement2 >= 1,
	getNElementOfList(List, NumberOfElement2, Y).

getNElementOfList([Y|_], 1, Y).

getUserInput(OrderList) :-
    writeln('Podaj kolejnosc wezlow'),
    nl,
    read(OrderList).

expand(node(State, _ ,_ , Cost, _ ), NewNodes)  :-
	findall(node(ChildState, Action, State, NewCost, ChildScore) ,
			(succ(State, Action, StepCost, ChildState),
			    score(ChildState, Cost, StepCost, NewCost, ChildScore)),
			NewNodes).


score(State, ParentCost, StepCost, Cost, FScore)  :-
	Cost is ParentCost + StepCost ,
	hScore(State, HScore),
	FScore is Cost + HScore.


insert_new_nodes( [ ], Queue, Queue).

insert_new_nodes( [Node|RestNodes], Queue, NewQueue) :-
	insert_p_queue(Node, Queue, Queue1),
	insert_new_nodes( RestNodes, Queue1, NewQueue).


insert_p_queue(Node, [ ], [Node]) :- !.

insert_p_queue(node(State, Action, Parent, Cost, FScore),
		[node(State1, Action1, Parent1, Cost1, FScore1)|RestQueue],
			[node(State1, Action1, Parent1, Cost1, FScore1)|Rest1] )  :-
	FScore >= FScore1,  ! ,
	insert_p_queue(node(State, Action, Parent, Cost, FScore), RestQueue, Rest1).

insert_p_queue(node(State, Action, Parent, Cost, FScore),  Queue,
				[node(State, Action, Parent, Cost, FScore)|Queue]).


build_path(node(nil, _, _, _, _), _, Path, Path) :- !.

build_path(node(EndState, _, _, _, _), Nodes, PartialPath, Path) :-
	del(Nodes, node(EndState, Action, Parent, _, _), Nodes1) ,
	build_path( node(Parent,_ ,_ , _ , _ ) , Nodes1, [Action/EndState|PartialPath],Path).


del([X|R],X,R).
del([Y|R],X,[Y|R1]) :-
	X\=Y,
	del(R,X,R1).


succ(a,ab,2,b).
succ(b,bf,3,f).
succ(a,ac,3,c).
succ(b,bg,4,g).
succ(g,gm,2,m).
succ(c,cd,2,d).
succ(d,dm,2,m).
succ(c,cm,8,m).
goal(m).
hScore(a,4).
hScore(b,4).
hScore(f,7).
hScore(g,1).
hScore(m,0).
hScore(c,3).
hScore(d,1).