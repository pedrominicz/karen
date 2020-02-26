last(X,cons(X,nil)).
last(X,cons(_,Xs)) :- last(X,Xs).

% Prolog comments, baby.

but_last(X,cons(X,cons(_,nil))).
but_last(X,cons(_,Xs)) :- but_last(X,Xs).

element_at(X,cons(X,_),suc(zero)).
element_at(X,cons(_,Xs),suc(K)) :- element_at(X,Xs,K).

length_(nil,zero).
length_(cons(_,Xs),suc(K)) :- length_(Xs,K).

refl(X,X).

yes :- refl(x,x).

no :- refl(a,b).

test(cons(a,_)).

zeros(zero).
zeros(X) :- zeros(X).
zeros(impossible).

% ?- refl(a,X).
%
% query_X <- 1.
% refl1_X1 <- 2.
%
% unify (refl(a,1)) (refl(2,2)).
%
% 1 = a.
% false.
%
% ?- last(X,cons(a,cons(b,nil))).
%
% query_X <- 1.
% last1_X1 <- 2.
%
% unify (last(1,cons(a,cons(b,nil)))) (last(2,cons(2,nil))).
% false.
%
% last2_X1 <- 3.
% last2__1 <- 4.
% last2_Xs1 <- 5.
%
% unify (last(1,cons(a,cons(b,nil)))) (last(3,cons(4,5))).
%
% 1 = 3.
% 4 = a.
% 5 = cons(b,nil).
%
% last1_X2 <- 6.
%
% unify (last(3,5)) (last(6,cons(6,nil))).
% unify (last(3,cons(b,nil))) (last(6,cons(6,nil))).
%
% 1 = 3 = 6 = b.
% false.
