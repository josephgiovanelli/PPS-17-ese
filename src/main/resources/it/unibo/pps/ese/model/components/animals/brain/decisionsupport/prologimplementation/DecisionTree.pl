/*
Example of entity declaration.
*/
/*
entity(bocc, herbivore, 6, 6, 6, [6, 6], 1, male).
entity(joy, herbivore, 9, 10, 10, [2, 1], 3, male).
entity(john, herbivore, 7, 6, 6, [3, 1], 3, female).
entity(smith, carnivorous, 10, 10, 10, [3, 3], 1, male).
entity(jack, herbivore, 4, 4, 4, [5, 5], 1, female).
*/

/*
Example of dynamic contents.
*/
/*
threshold(attack, 3).
threshold(height, 5).
threshold(attractiveness, 2).

kind(prey, carnivorous, herbivore).
kind(prey, herbivore, plant).
kind(partner, carnivorous, carnivorous).
kind(partner, herbivore, herbivore).
*/

/*
API to add theory dynamically (thresholds and relationships between entities).
*/
setAttackThreshold(Threshold) :-
	assert(threshold(attack, Threshold)).

setAttractivenessThreshold(Threshold) :-
	assert(threshold(attractiveness, Threshold)).

setHeightThresholds(Threshold) :-
	assert(threshold(height, Threshold)).

addCompatibleHuntingKinds(HunterKind, PreyKind) :-
	assert(kind(prey, HunterKind, PreyKind)).

addCompatibleCouplingKinds(HunterKind, PartnerKind) :-
	assert(kind(partner, HunterKind, PartnerKind)).

deleteCompatibleHuntingKinds(HunterKind, PreyKind) :-
	retract(kind(prey, HunterKind, PreyKind)).

deleteCompatibleCouplingKinds(HunterKind, PartnerKind) :-
	retract(kind(partner, HunterKind, PartnerKind)).


/*
addEntity(+Name, +Kind, +Height, +strength, +Defense, +Pos, +Attractiveness, +Gender)
Add entity in the world.
*/
addEntity(Name, Kind, Height, Strength, Defense, Pos, Attractiveness, Gender) :-
	assert(entity(Name, Kind, Height, Strength, Defense, Pos, Attractiveness, Gender)).

/*
deleteEntity(+Name)
Delete entity from the world.
*/
deleteEntity(Name) :-
	retract(entity(Name, Kind, Height, Strength, Defense, Pos, Attractiveness, Gender)).

/*
simulateAttack(+strengthX, +DefenseY)
Check if the defense of the prey Y is acceptable for the hunter X.
*/
simulateAttack(strengthX, DefenseY) :-
	Z is strengthX - DefenseY,
	threshold(attack, T),
	Z > T.

/*
simulateCoupling(+AttractivenessY, +GenderX, +GenderY, +strengthX, +DefenseY)
Check if the partner Y is suitable for the hunter X.
*/
simulateCoupling(AttractivenessX, AttractivenessY, GenderX, GenderY, strengthX, DefenseY) :-
	threshold(attractiveness, T),
	AttractivenessY > T,
	GenderX == male,
	GenderY == female,
	(simulateAttack(strengthX, DefenseY); AttractivenessX > T).

/*
compatiblePreysKind(+KindX, +KindY)
Check if the kind of the prey Y is suitable for the hunter X.
*/
compatiblePreysKind(KindX, KindY) :-
	kind(prey, KindX, KindY).

/*
compatiblePartnersKind(+KindX, +KindY)
Check if the kind of the entity Y is suitable for the entity X.
*/
compatiblePartnersKind(KindX, KindY) :-
	kind(partner, KindX, KindY).

/*
heightDiff(+HeightX, +HeightY)
Check if the height of the prey/partner Y is suitable for the hunter/entity X.
*/
heightDiff(HeightX, HeightY) :-
	Z is HeightX - HeightY,
	threshold(height, T),
	(Z < T).

/*
discoverPreys(+X, -Y, -Lenght)
Giving as input x simulates, through a set of rules, the behavior of a binary tree, finding all the preys Y and calculating for each the lenght of the best route to get there.
*/
discoverPreys(X, Y, Lenght) :-
	entity(X, KindX, HeightX, strengthX, DefenseX, PosX, AttractivenessX, GenderX),
	entity(Y, KindY, HeightY, strengthY, DefenseY, PosY, AttractivenessY, GenderY),
	Y \== X,
	heightDiff(HeightX, HeightY),
	compatiblePreysKind(KindX, KindY),
	simulateAttack(strengthX, DefenseY),
	lenght(PosX, PosY, Lenght).

/*
discoverPartners(+X, -Y, -Lenght)
Giving as input x simulates, through a set of rules, the behavior of a binary tree, finding all the partners Y and calculating for each the lenght of the best route to get there.
*/
discoverPartners(X, Y, Lenght) :-
	entity(X, KindX, HeightX, StrengthX, DefenseX, PosX, AttractivenessX, GenderX),
	entity(Y, KindY, HeightY, StrengthY, DefenseY, PosY, AttractivenessY, GenderY),
	Y \== X,
	heightDiff(HeightX, HeightY),
	compatiblePartnersKind(KindX, KindY),
	simulateCoupling(AttractivenessX, AttractivenessY, GenderX, GenderY, StrengthX, DefenseY),
	lenght(PosX, PosY, Lenght).

/*
lenght(+PosX, +PosY, -Lenght)
Return the lenght of path from PosX to PosY.
*/
lenght(PosX, PosY, Lenght) :-
	decision(PosX, PosY, NewPosX, Direction),
	lenght(NewPosX, PosY, CurrentLenght), Lenght is 1 + CurrentLenght.
lenght(PosX, PosY, Lenght) :-
	PosX == PosY,
	Lenght is 0.

/*
nextMove(+X, +Y, -Move) :-
Return the next move of the path from PosX to PosY.
*/
nextMove(X, Y, NewX, NewY) :-
	entity(X, KindX, HeightX, strengthX, DefenseX, PosX, AttractivenessX, GenderX),
	entity(Y, KindY, HeightY, strengthY, DefenseY, PosY, AttractivenessY, GenderY),
	path(PosX, PosY, PathDirection, [[NewX, NewY] | OtherMoves]).

/*
path(+PosX, +PosY, -Path)
Return the best path from PosX to PosY.
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
Compares PosX with PosY and return the Direction to take and the resulting PosX modified for the next iteration.
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