users(1).
users(2).
users(3).
users(4).
users(5).

roles(ceo).
roles(lead).
roles(ed).
roles(vp).
roles(manager).
roles(cto).

perms(r).
perms(w).
perms(x).
perms(rw).
perms(wx).


ur(1,ceo).
ur(2,lead).
ur(3,ed).
ur(4,vp).
ur(5,manager).

rp(ceo,ceoread).
rp(ceo,ceoexecute).
rp(lead,leadread).
rp(lead,leadexecute).
rp(ed,edread).
rp(ed,edexecute).
rp(vp,vpread).
rp(vp,vpexecute).
rp(manager,managerread).
rp(manager, managerexecute).

rh(ceo,lead).
rh(ceo,ed).
rh(ceo,cto).
rh(cto,ceo).
rh(cto,ed).
rh(ed,vp).
rh(vp,ceo).
rh(ed,cto).
rh(ed,manager).
rh(vp,manager).
rh(vp,ed).
