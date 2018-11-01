
member(X, [X|_]).
member(X, [_|Ys]) :-
	member(X,Ys).

not_member(_, []) :- !.

not_member(X, [Head|Tail]) :-
     X \= Head,
    not_member(X, Tail).
	
append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).


auth_perm(X,Y) :- 
	rp(X,Y).

auth_perm(X,Y) :- 
	auth_role(X,Z,[]),
	auth_perm(Z,Y).
	
authorized_permissions(X,L) :-
	findall( Y, auth_perm(X, Y), S ),
	sort(S,L).

get_all_users(User) :-
	ur(User,_).

get_all_users_like_set(USERS) :-
	setof( Y, get_all_users(Y) , USERS ).
	
role_heirarchy(X,Y,S) :-
	rh(X,Y).
		
		
role_heirarchy(X,Y,S) :-
	rh(X,Z),
	not_member(Z,S),
	role_heirarchy(Z,Y,[Z|S]).
	

auth_role(X,Y,S) :-
	ur(X,Y).

auth_role(X,Y,S) :-
	ur(X,XR),
	not_member(XR,S),
	role_heirarchy(XR,Y,[XR|S]).

authorized_roles(X,L) :-	
	findall(Z,auth_role(X,Z,[]),S),
	sort(S,L).

	
get_permission_for_user([],[]).

get_permission_for_user([User|Rest_users],[H|T]) :-
	authorized_permissions(User,H),
	get_permission_for_user(Rest_users,T).
	
minRoles(S) :-
	get_all_users_like_set(USERS),
	get_permission_for_user(USERS,LIST_PERMISSIONS_PER_USER),
	sort(LIST_PERMISSIONS_PER_USER,L1), len(L1,S).
	
	
len(X,Y):-
	findlen(X,Y).
	
findlen([],X) :-
	X is 0.

findlen([X|Tail],Count) :-
	findlen(Tail,Prev),
	Count is Prev+1 .
	