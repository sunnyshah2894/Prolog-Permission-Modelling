% Input format
users(2).
roles(3).
perms(2).

ur(1,1).
rp(1,1).

rh(2,3).

ur(2,2).
rp(2,1).

rp(3,2).


% Output format
% authorized_roles(1,List_Roles).
% List_Roles = [1]

% authorized_permissions(1,List_Permissions).
% List_Permissions = [1]

% minRoles(S).
% S = 2