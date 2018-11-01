% Input format

/* Initializing Users, s, Permissions*/
users(5).
roles(6).
perms(10).


/* 	Creating the User- pairs 
	User-1 => -1
	User-2 => -2
	User-3 => -3
	User-4 => -4
	User-5 => -5
*/
ur(1,1).
ur(2,2).
ur(3,3).
ur(4,4).
ur(5,5).

/* 	Creating the -Permission pairs 
	-1 => Permissions-1,2
	-2 => Permissions-3,4
	-3 => Permissions-7,8
	-4 => Permissions-9,10
	-5 => Permissions-5,6
*/
rp(1,1).
rp(1,2).
rp(2,3).
rp(2,4).
rp(3,7).
rp(3,8).
rp(4,9).
rp(4,10).
rp(5,5).
rp(5,6).

/* 	Creating the -Hierarchy pairs 
	-1 ancestor of 2,3,4,6,5
	-2 ancestor of 2,3,4,6,5
	-3 ancestor of 2,3,4,6,5
	-4 ancestor of NONE
	-5 ancestor of NONE
*/
rh(1,2).
rh(1,5).
rh(1,6).
rh(2,3).
rh(2,4).
rh(3,6).
rh(6,1).


% Output format
% authorized_roles(1,List_s).
% List_s = [1]

% authorized_permissions(1,List_Permissions).
% List_Permissions = [1]

% mins(S).
% S = 2