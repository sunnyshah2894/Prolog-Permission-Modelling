
/*
Predicate: member
Checks if an element is a member of the list.

Arguments - member( %element, %list )
Returns true if element is part of the list
*/
member(X, [X|_]).
member(X, [_|Ys]) :-
	member(X,Ys).

/*
Predicate: not_member
Checks if an element is not a member of the list.

Arguments - not_member( %element, %list )
Returns true if element is not part of the list
*/
not_member(_, []) :- !.
not_member(X, [Head|Tail]) :-
     X \= Head,
    not_member(X, Tail).
	
/*
Predicate: append
Appends a head list to a list.

Arguments - append( %head_list, %list, %output_list )
*/
append([], Y, Y).
append([H|X], Y, [H|Z]) :- append(X, Y, Z).

/*
Predicate: flatten2
Flattens a list of list into a single dimensional list.


Credits - http:\/\/stackoverflow.com\/a\/9059827 
*/
flatten2([], []) :- !.
flatten2([L|Ls], FlatL) :-
    !,
    flatten2(L, NewL),
    flatten2(Ls, NewLs),
    append(NewL, NewLs, FlatL).
flatten2(L, [L]).


/*
Predicate: auth_perm
Returns list of permissions for a given role
*/
auth_perm(X,Y) :- 
	findall(Z,rp(X,Z),Y).

/*
Predicate: get_permission_for_role
Returns List of permissions for all the roles given.

Arguments: get_permission_for_role(%list_of_roles, %list_of_permissions)
Returns: list of list of permissions for each role.
*/	
get_permission_for_role([],[]).
get_permission_for_role([Role|Rest_role],[H|T]) :-
	auth_perm(Role,H),
	get_permission_for_role(Rest_role,T).
	
/*
Predicate: authorized_permissions
Find all the authorized permissions for a given role.

Logic: 
	
	-	Find all the roles assigned to the user using the authorized_roles predicate.
	- 	Find permissions for each of those roles. 
	- 	Flatten the list of list of permissions we recieved from get_permission_for_role
	-	Apply sort on the flattened list to get the unique permissions.
*/
authorized_permissions(X,L) :-
	authorized_roles(X,ROLES_X),
	get_permission_for_role(ROLES_X,L1),
	flatten2(L1,L2),sort(L2,L).	

/*
Predicate: get_all_users
Will return all the users in the graph, that are assigned a role
*/
get_all_users(User) :-
	ur(User,_).

/*
Predicate: get_all_users_like_set
Will return all unique users in the graph, that are assigned atleast 1 role
*/
get_all_users_like_set(USERS) :-
	setof( Y, get_all_users(Y) , USERS ).
	
/*
Predicate: role_heirarchy
Will return all the inherited roles for a given role.

Logic: 
	-	Initial base case is to return Y if rh(X,Y) is a known fact
	-	Else, find recursively all the ancestors of X, such that 
		the descendants are not in the list of Visited
	- 	On each selection, we also append the role to Visited list
*/
role_heirarchy(X,Y,Visited) :-
	rh(X,Y).
role_heirarchy(X,Y,Visited) :-
	rh(X,Z),
	not_member(Z,Visited),
	role_heirarchy(Z,Y,[Z|Visited]).
	
/*
Predicate: auth_role
Will return all roles assigned to a user. 
This may also include the roles that are inherited

Logic:
	-	Initial base case is to return Y if ur(X,Y) is a fact.
	-	Else, find recursively all the roles in hierarchy using the
		role_heirarchy predicate. Also append the current role in to a visited
		list.

*/
auth_role(X,Y,Visited) :-
	ur(X,Y).
auth_role(X,Y,Visited) :-
	ur(X,XR),
	not_member(XR,Visited),
	role_heirarchy(XR,Y,[XR|Visited]).

/*
Predicate: authorized_roles
Will return all unique roles assigned to a user in a list. 
This may also include the roles that are inherited 

Logic:
	
	-	Find all the roles returned by auth_role and add them to a list S.
	- 	Apply sort on S to remove the duplicates.

*/
authorized_roles(X,L) :-	
	findall(Z,auth_role(X,Z,[]),S),
	sort(S,L).

get_permission_for_user([],[]).

get_permission_for_user([User|Rest_users],[H|T]) :-
	authorized_permissions(User,H),
	get_permission_for_user(Rest_users,T).

/*
Predicate: minRoles
Will find the minimum roles required to such that the permission
requirement of each user can be satisfied.

Logic:

	-	Find all the users in the KB that are assigned atleast 1 role.
	- 	Find permissions for each of those users. 
	-	Now we have a list of list of permissions, where each internal list 
		denotes permissions assigned to a user.
	-	We apply sort on the list, which also internally removes the duplicates
		in the list.
	-	Now we have all unique set of permissions and thus the length of the list
		is the answer to minRoles

*/	
minRoles(S) :-
	get_all_users_like_set(USERS),
	get_permission_for_user(USERS,LIST_PERMISSIONS_PER_USER),
	sort(LIST_PERMISSIONS_PER_USER,L1), len(L1,S).
	
/*
Predicate: len
Will return length of a list
*/
len(X,Y):-
	findlen(X,Y).
findlen([],X) :-
	X is 0.
findlen([X|Tail],Count) :-
	findlen(Tail,Prev),
	Count is Prev+1 .
	