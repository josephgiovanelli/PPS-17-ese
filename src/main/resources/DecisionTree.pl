/*
Example of entity declaration.
*/
/*
entity(bocc, herbivore, 6, 6, 6, [6, 6]).
entity(joy, herbivore, 7, 7, 7, [2, 1]).
entity(john, herbivore, 6, 6, 6, [3, 1]).
entity(smith, carnivorous, 10, 10, 10, [3, 3]).
entity(jack, herbivore, 4, 4, 4, [5, 5]).
*/

/*
addEntity(+Name, +Kind, +Height, +Strong, +Defense, +Pos)
add entity in the world.
*/
addEntity(Name, Kind, Height, Strong, Defense, Pos) :-
	assert(entity(Name, Kind, Height, Strong, Defense, Pos)).

/*
deleteEntity(+Name)
delete entity from the world.
*/
deleteEntity(Name) :-
	retract(entity(Name, Kind, Height, Strong, Defense, Pos)).

/*
simulateAttack(+StrongX, +DefenseY)
Check if the defense of the prey Y is acceptable for the hunter X.
*/
simulateAttack(StrongX, DefenseY) :-
	Z is StrongX - DefenseY,
	Z > 3.

/*
compatiblePreysKind(+KindX, +KindY)
Check if the kind of the prey Y is suitable for the hunter X.
*/
compatiblePreysKind(KindX, KindY) :-
	(KindX == carnivorous, KindY == herbivore);
	(KindX == herbivore, KindY == plant).

/*
compatiblePartnersKind(+KindX, +KindY)
Check if the kind of the entity Y is suitable for the entity X.
*/
compatiblePartnersKind(KindX, KindY) :-
	(KindX == carnivorous, KindY == carnivorous);
	(KindX == herbivore, KindY == herbivore).

/*
heightDiff(+HeightX, +HeightY)
Check if the height of the prey Y is suitable for the hunter X.
*/
heightDiff(HeightX, HeightY) :-
	Z is HeightX - HeightY,
	(Z > 0, Z < 5).

/*
discoverPreys(+X, -Y, -Lenght)
Giving as input x simulates, through a set of rules, the behavior of a binary tree, finding all the preys Y and calculating for each the lenght of the best route to get there.
*/
discoverPreys(X, Y, Lenght) :-
	entity(X, KindX, HeightX, StrongX, DefenseX, PosX),
	entity(Y, KindY, HeightY, StrongY, DefenseY, PosY),
	Y \== X,
	heightDiff(HeightX, HeightY),
	compatiblePreysKind(KindX, KindY),
	simulateAttack(StrongX, DefenseY),
	lenght(PosX, PosY, Lenght).

/*
discoverPartners(+X, -Y, -Lenght)
Giving as input x simulates, through a set of rules, the behavior of a binary tree, finding all the partners Y and calculating for each the lenght of the best route to get there.
Da completare e poi fattorizzare.
*/
discoverPartners(X, Y, Lenght) :-
	entity(X, KindX, HeightX, StrongX, DefenseX, PosX),
	entity(Y, KindY, HeightY, StrongY, DefenseY, PosY),
	Y \== X,
	heightDiff(HeightX, HeightY),
	compatiblePartnersKind(KindX, KindY),
	%simulateCoupling(SomethingOfX, SomethingOfY),
	lenght(PosX, PosY, Lenght).

/*
lenght(+PosX, +PosY, -Lenght)
return the lenght of path from PosX to PosY.
*/
lenght(PosX, PosY, Lenght) :-
	decision(PosX, PosY, NewPosX, Direction),
	lenght(NewPosX, PosY, CurrentLenght), Lenght is 1 + CurrentLenght.
lenght(PosX, PosY, Lenght) :-
	PosX == PosY,
	Lenght is 0.

/*
nextMove(+X, +Y, -Move) :-
return the next move of the path from PosX to PosY.
*/
nextMove(X, Y, NewX, NewY) :-
	entity(X, KindX, HeightX, StrongX, DefenseX, PosX),
	entity(Y, KindY, HeightY, StrongY, DefenseY, PosY),
	path(PosX, PosY, PathDirection, [[NewX, NewY] | OtherMoves]).

/*
path(+PosX, +PosY, -Path)
return the best path from PosX to PosY.
*/
path(PosX, PosY, PathDirection, PathPosition) :-
	 decision(PosX, PosY, NewPosX, Direction),
	 path(NewPosX, PosY, CurrentPathDirection, CurrentPathPosition), add(Direction, CurrentPathDirection, PathDirection), add(NewPosX, CurrentPathPosition, PathPosition).
path(PosX, PosY, PathDirection, PathPosition) :-
	PosX == PosY,
	PathDirection = [],
	PathPosition = [].

/*
decision(+PosX, +PosY, -NewPosX, -Direction)
compares PosX with PosY and return the Direction to take and the resulting PosX modified for the next iteration.
*/
decision([XX, YX], [XY, YY], [NewXX, NewYX], Direction):-
	XX > XY, !, NewXX is XX - 1, NewYX is YX, Direction = up.
decision([XX, YX], [XY, YY], [NewXX, NewYX], Direction):-
	XX < XY, !, NewXX is XX + 1, NewYX is YX, Direction = down.
decision([XX, YX], [XY, YY], [NewXX, NewYX], Direction):-
	(XX == XY, YX > YY), !, NewXX is XX, NewYX is YX - 1, Direction = left.
decision([XX, YX], [XY, YY], [NewXX, NewYX], Direction):-
	(XX == XY, YX < YY), !, NewXX is XX, NewYX is YX + 1, Direction = right.

/*
add(+H, +T, -[H|T])
Adds H at the top of the T list.
*/
add(H, T, [H|T]).