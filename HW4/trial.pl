tower(N, T, C) :-

        % preconditions for C
        counts(U, D, L, R) = C,
        length(U, N),
        length(D, N),
        length(L, N),
        length(R, N),

        % preconditions for T
        length(T, N),
        row_length(T, N), 

        maplist(fd_all_different, T), % rows have diff elements 
        row_range(T, N),  % rows in a range  

        transpose(T, Trans),      % converting rows into columns 
        maplist(fd_all_different, Trans), % columns have diff elements
        row_range(Trans, N),  % columns in a range 

        maplist(fd_labeling, T), % if instantiation needed 

        % getting values for C
        count_all(T, L),
        count_all(Trans, U),

        reverse_all(T, RevT),
        count_all(RevT, R),

        reverse_all(Trans, RevTrans),
        count_all(RevTrans, D).


% go thru the matrix 
count_all([], []).
count_all([ [First|Rest] |T], [C|List]) :- 
        % get_first(H, First),
        count_row([First|Rest], C, []),
        count_all(T, List).

all_less([], _).
all_less([H | Rest], X) :-
	%print(H), nl, print(X), nl,
	H #< X,
	all_less(Rest, X).

% go thru the row    
count_row([], Acc, _) :- Acc = 0.
count_row([H|T], Acc, Before) :-
    append(Before, [H], NewBefore),
    count_row(T, Acc1, NewBefore),
    (all_less(Before, H) -> Acc is Acc1 + 1; Acc1 = Acc).


% count_row([H|T], N, M, Acc) :-
%         H < M,
%         count_row(T, N, M, Acc).
% count_row([H|T], N, M, Acc) :-
%         H >= M, 
%         succ(Acc, Acc1),
%         count_row(T, N, H, Acc1).

% get_first([H|_], H).
  
transpose([],[]).
transpose([F|Fs], Ts) :-
        transpose_each(F, [F|Fs], Ts).

transpose_each([], _, []).
transpose_each([_|Rs], Ms, [Ts|Tss]) :-
        lists_change(Ms, Ts, Ms1),
        transpose_each(Rs, Ms1, Tss).

lists_change([], [], []).
lists_change([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_change(Rest, Fs, Oss).

row_range([], _).
row_range([H|T], N) :-
        fd_domain(H, 1, N),
        row_range(T, N).

row_length([], _).
row_length([H|T], N) :-
        length(H, N),
        row_length(T, N).


reverse_all([], []).
reverse_all([H|T], [Rev| RList]) :-
        reverse_row(H, Rev), 
        reverse_all(T, RList).

reverse_row([], []).
reverse_row([H|T], NList) :-
        reverse_row(T, List),
        append(List, [H], NList).
        

% my_all_different(L) :-
%         length(L, LLen), 
%         sort(L, S),
%         length(S, SLen), 
%         LLen == SLen .

% START 
elem_diff(_, []).
elem_diff(X, [F | R]) :-
	X #\= F,
	elem_diff(X, R).

my_all_unwrap(_, []).
my_all_unwrap(Before, [X | R]) :-
	elem_diff(X, R),
	append(Before, [X], NewBefore),
	my_all_unwrap(NewBefore, R).

my_all_different(L) :-
	my_all_unwrap([], L). 
% END 


my_labelling(0,[],_).
my_labelling(N, [H|T], S) :-
        % list_permutation(H, S),
        permutation(H, S),
        succ(N0, N),
        my_labelling(N0, T, S).

plain_tower(N, T, C) :-
        % preconditions for T
        length(T, N),
        row_length(T, N), 
        
        findall(X, between(1, N, X), S),
        my_labelling(N,T,S),    % if instantiation needed 

        maplist(my_all_different, T), % rows have diff elements 

        transpose(T, Trans),      % converting rows into columns 
        maplist(my_all_different, Trans), % columns have diff elements

        % preconditions for C
        C = counts(U, D, L, R),
        length(U, N),
        length(D, N),
        length(L, N),
        length(R, N),

        % getting values for C
        count_all(T, L),
        count_all(Trans, U),

        reverse_all(T, RevT),
        count_all(RevT, R),

        reverse_all(Trans, RevTrans),
        count_all(RevTrans, D).

% list_permutation(P,B) :-
%         length(P, PLen), 
%         length(B, BLen),
%         PLen == BLen,    % redundant goal helps termination
%         list_perm(P,B).
         
% list_perm([],[]).
% list_perm([A|As],Bs0) :-
%          select(A,Bs0,Bs),
%          list_perm(As,Bs).


speedup(X) :- 
        statistics(cpu_time, [_, _]),
        tower(5,
                [[2,3,4,5,1],
                [5,4,1,3,2],
                Row3,
                [RC41,5|Row4Tail],
                Row5],
                counts(Top, [4|BottomTail],
                        [Left1,Left2,Left3,Left4,5],
                        Right)),
        statistics(cpu_time, [_, T1]),
        plain_tower(5,
                [[2,3,4,5,1],
                [5,4,1,3,2],
                Row3_new,
                [RC41_new,5|Row4Tail_new],
                Row5_new],
                counts(Top_new, [4|BottomTail_new],
                        [Left1_new,Left2_new,Left3_new,Left4_new,5],
                        Right_new)),
        statistics(cpu_time, [_, T2]),
        FT1 is (T1+1.0),        % T1 is 0, added 1 to normalize 
        FT2 is (T2+1.0),
        X is FT2/FT1.
        

ambiguous(N, C, T1, T2) :- 
        tower(N, T1, C),
        tower(N, T2, C),
        T1 \= T2. 

        
