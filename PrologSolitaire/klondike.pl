% KLONDIKE SOLITARE for SWI-Prolog
% Copyright 2016 Sam MacKinnon and Regina Adshade Moore
%
% To start with a randomly shuffled deck:
% ?- start.
%
% To start with an ordered deck:
% ?- start_solvable.
%
% To make an automatically-generated move:
% ?- move.
%
% ////////////TEST CASES/////////////
%
% 1. Try playing a solvable game:
%   ?- start_solvable. --> Enter --> Enter -->
%   ?- move. --> Enter --> Enter --> (etc).
%
% 2. Try playing a random game:
%   ?- start. --> Enter --> Enter -->
%   ?- move. --> Enter --> Enter --> (etc).
%
% If a game ends in "LOSER", try these queries:
%
%   To see lists of face down cards on table:
%     ?- row(R,down,X). --> type ";" to see all hidden-card rows.
%
%   To see list of cards in extra-card deck:
%     ?- deck(D).
%
% 3. A few other interesting things to try:
%   ?- start_solvable.
%
%   To see all the possible moves for playing face-up cards on
%   eachother: (Repeat x4).
%   ?- row(R1,up,X),last(X,LC),row(R2,up,[C2|_]),plays_on(LC,C2).
%      --> type ";" to see all options
%   ?- play.
%
%   To remove all face-down cards, and re-render:
%     ?- retractall(row(R,down,X)),assert(row(R,down,[])).
%     ?- render.




:- dynamic
	row/3,
	deck/1,
	endpile/2.

% deck1 is a deck of 52 cards.
% card(N,S,C) represents a playing card where:
% N is the value of the card from 1 to 13 (13 is a King).
% S is the suit of the card
% C is the color of the card
deck1([
card(13,spds,black),card(13,dmds,red),
card(13,clbs,black),card(13,hrts,red),
card(12,spds,black),card(12,dmds,red),
card(12,hrts,red),card(12,clbs,black),
card(11,hrts,red),card(11,clbs,black),
card(11,dmds,red),card(11,spds,black),
card(10,hrts,red),card(10,spds,black),
card(10,clbs,black),card(10,dmds,red),
card(9,clbs,black),card(9,hrts,red),
card(9,dmds,red),card(9,spds,black),
card(8,clbs,black),card(8,hrts,red),
card(8,dmds,red),card(8,spds,black),
card(7,hrts,red),card(7,clbs,black),
card(7,dmds,red),card(7,spds,black),
card(6,hrts,red),card(6,clbs,black),
card(6,dmds,red),card(6,spds,black),
card(5,spds,black),card(5,hrts,red),
card(5,clbs,black),card(5,dmds,red),
card(4,spds,black),card(4,hrts,red),
card(4,clbs,black),card(4,dmds,red),
card(3,spds,black),card(3,hrts,red),
card(3,clbs,black),card(3,dmds,red),
card(2,dmds,red),card(2,clbs,black),
card(2,hrts,red),card(2,spds,black),
card(1,dmds,red),card(1,clbs,black),
card(1,spds,black),card(1,hrts,red)
]).

% row(R,State,Cards) is a row of cards on the table, where
% R is the row number
% State describes whether the row contains cards flipped up, or down
% Cards is the list of cards in the row
row(1,down,[]).
row(1,up,[]).
row(2,down,[]).
row(2,up,[]).
row(3,down,[]).
row(3,up,[]).
row(4,down,[]).
row(4,up,[]).
row(5,down,[]).
row(5,up,[]).
row(6,down,[]).
row(6,up,[]).
row(7,down,[]).
row(7,up,[]).
% Deck is the remaining pile of cards not dealt onto the table
deck([]).
% Endpiles are the final piles of hearts, spades, diamonds, and clubs
% that need to be completed to win the game.
endpile(hearts,[card(0,hrts,red)]).
endpile(spades,[card(0,spds,black)]).
endpile(diamonds,[card(0,dmds,red)]).
endpile(clubs,[card(0,clbs,black)]).

% Shuffle the deck, then deal
deal :-
    deck1(D),
    random_permutation(D,R),
    deal2(R,1).

% Deal the deck as-is (not shuffled).
deal_solvable :-
    deck1(DS),
    deal2(DS,1).

% Cards are dealt out in offical Klondike order. C is the first card in
% the deck. D is the rest of the deck. A is the row number that the
% current card is dealt into.
deal2([C|D],A) :-
	(   A = 1,
	    row(1,down,R1),
	    retractall(row(1,down,_)),
	    assert(row(1,down,[C|R1])),
	    deal2(D,2))
	 ;
	(   A = 2,
	    row(2,down,R2),
	    retractall(row(2,down,_)),
	    assert(row(2,down,[C|R2])),
	    deal2(D,3))
	;
	(   A = 3,
	    row(3,down,R3),
	    retractall(row(3,down,_)),
	    assert(row(3,down,[C|R3])),
	    deal2(D,4))
	;
	(   A = 4,
	    row(4,down,R4),
	    retractall(row(4,down,_)),
	    assert(row(4,_,[C|R4])),
	    deal2(D,5))
	;
	(   A = 5,
	    row(5,down,R5),
	    retractall(row(5,down,_)),
	    assert(row(5,_,[C|R5])),
	    deal2(D,6))
	;
       (    A = 6,
	    row(6,down,R6),
	    retractall(row(6,down,_)),
	    assert(row(6,_,[C|R6])),
	    deal2(D,7))
	;
	(   A = 7,
	    row(7,down,R7),
	    retractall(row(7,down,_)),
	    assert(row(7,_,[C|R7])),
	    length([C|R7],L7),
	    Goto is (L7 + 1),
	    deal2(D,Goto))
       ;
       (   A = 8,
            retractall(deck(_)),
            assert(deck([C|D]))).

% Sets up a game with a randomly shuffled deck of cards
start :-
	make_rows,nl,nl,
	write("   *[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*"),nl,nl,
	write("	      KLONDIKE SOLITAIRE"),nl,
	write("	       By Sam and Regina"),nl,nl,
	write("         type 'move.' to make a move"),nl,nl,
	write("   *[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*"),nl,nl,nl,
	deal,
	flip_top_cards(1),
	render.

% Sets up a game with an ordered set of cards
start_solvable :-
	make_rows,nl,nl,nl,
	write("*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*"),nl,nl,
	write("	   KLONDIKE SOLITAIRE"),nl,
	write("	    By Sam and Regina"),nl,nl,
	write("      type 'move.' to make a move"),nl,nl,
	write("*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*[<>]*"),nl,nl,nl,
	write("========================================="),nl,nl,
	deal_solvable,
	flip_top_cards(1),
	render.

% Resets all rows to starting states
make_rows :-
	retractall(row(_,_,_)),
	retractall(deck(_)),
	retractall(endpile(_,_)),
	assert(row(1,down,[])),	assert(row(1,up,[])),
	assert(row(2,down,[])),	assert(row(2,up,[])),
	assert(row(3,down,[])),	assert(row(3,up,[])),
	assert(row(4,down,[])),	assert(row(4,up,[])),
	assert(row(5,down,[])),	assert(row(5,up,[])),
	assert(row(6,down,[])),	assert(row(6,up,[])),
	assert(row(7,down,[])),	assert(row(7,up,[])),
        assert(deck([])),
	assert(endpile(hearts,[card(0,hrts,red)])),
	assert(endpile(spades,[card(0,spds,black)])),
	assert(endpile(diamonds,[card(0,dmds,red)])),
	assert(endpile(clubs,[card(0,clbs,black)])).


% Picks a move to do next, executes that move, and then re-renders the
% gamearea.
move :-
	% Try adding face up cards on table to endpiles
	add_to_endpiles
	;
	% Else, try moving cards around on table (play on eachother).
        play
	;
	% Else, see if there is a king on the table that can be moved to an empty space
	row(R,up,X),
	row(R,down,XD),
	king_move(X,XD)
	;
	% Else, try looking through the extra card pile, and playing from that.
	nl,write('Looking through deck... '),
	deck([Top|Rest]),
	endpile(_,[EP1|_]),
	look_through_deck([Top|Rest],[],EP1)
	% No more moves! Game is over. But did you win?
	;
	endgame.

% True if there is a card that can be added to a foundation pile
add_to_endpiles :-
	row(R,up,[X|T]),
        endpile(P,[EP1|_]),
	finishes_on(X,EP1),
	retractall(row(R,up,_)),
	retractall(endpile(P,_)),
	assert(row(R,up,T)),
	assert(endpile(P,[X|EP1])),
	row(R,down,RD),
	row(R,up,RU),
	flip_card(R,RD,RU),
	convert(X,Str),
	nl,write("Added "),write(Str),
	write(" to "),write(P),
	write(" foundation."),nl,nl,
	render.

% True if a row of face up cards on the table can be moved onto another
% row.
play :-
	row(R,up,X),
	last(X,LC),
	row(R2,up,[X2|T2]),
	plays_on(LC,X2),
	retractall(row(R,up,_)),
	assert(row(R,up,[])),
	retractall(row(R2,up,_)),
	append(X,[X2|T2],NewRow),
	assert(row(R2,up,NewRow)),
	row(R,down,RD),
	flip_card(R,RD,[]),
	convert(LC,S1),
	convert(X2,S2),
	nl,nl, write("Played "),write(S1),
	write(" from row "),write(R),
	write(" onto "),write(S2),
	write(" on row "),write(R2),
	write("."),nl,nl,
	render.

% If a king is already on an empty "down" row, don't move it.
king_move(_,[]) :-
	fail.

% If there is a row with a King, and an empty space, move the entire row
% to the empty space on the board
king_move(X,XD) :-
	\+(XD = []),
	row(R,up,X),
	row(R,down,XD),
	last(X,card(13,_,_)),
	last(X,C),
	row(R2,up,[]),
	retractall(row(R,up,_)),
	assert(row(R,up,[])),
	retractall(row(R2,up,_)),
	assert(row(R2,up,X)),
	row(R,down,RD),
	flip_card(R,RD,[]),
	convert(C,Str),
	nl,write("Moved KING "),write(Str),
	write(" onto empty row: "),write(R2),
	write("."),nl,
	render.

% Go through the extra-cards deck,and try moves. [C|RD] is the list of
% cards in the extra deck. Tried is a list of tried cards that have been
% tried. E is the endpile(foundation) currently being tried (eg does
% this card fit onto this foundation pile?).
look_through_deck([C|RD],Tried,E) :-
	%true if a card can be moved onto a foundation pile
	finishes_on(C,E),
	convert(C,Str),
	endpile(P,[E|EP]),
	append(RD,Tried,NewDeck),
	retractall(endpile(P,_)),
	retractall(deck(_)),
	assert(endpile(P,[C,E|EP])),
	assert(deck(NewDeck)),
	nl,nl,write("FOUND CARD "),write(Str),nl,
	write("Played onto "),write(P),
	write(" foundation."),nl,nl,
	render
	;
	%true if a card can be moved onto a face up card on the table
	row(R2,up,[C2|T2]),
	plays_on(C,C2),
	convert(C,Str),
	append(RD,Tried,NewDeck),
	retractall(row(R2,up,_)),
	retractall(deck(_)),
	assert(row(R2,up,[C,C2|T2])),
	assert(deck(NewDeck)),
	convert(C2,Str2),
	nl,nl,write("FOUND CARD "),write(Str),nl,
	write("Played onto "),write(Str2),
	write(" on row "),write(R2),write("."),nl,nl,
	render
	;
	%true if there is a king in the deck that can be moved to an empty space
	row(R2,up,[]),
	row(R2,up,X),
	is_a_king(C),
	convert(C,Str),
	append(RD,Tried,NewDeck),
	retractall(row(R2,up,_)),
	assert(row(R2,up,[C|X])),
	retractall(deck(_)),
	assert(deck(NewDeck)),
	nl,nl,write("FOUND KING "),write(Str),nl,
	write("Moved to empty row: "),write(R2),write("."),
	nl,nl,
	render
	;
	look_through_deck(RD,[C|Tried],E).

%Evaluates if game has been won or lost.
endgame :-
	% If endpiles all contain Kings as the top card, then print "WINNER" visual!
	endpile(hearts,[card(13,hrts,red)|_]),
	endpile(spades,[card(13,spds,black)|_]),
        endpile(diamonds,[card(13,dmds,red)|_]),
        endpile(clubs,[card(13,clbs,black)|_]),
	nl,nl,nl,nl,nl,nl,
	write(".------..------..------..------..------..------."),nl,
        write("|W.--. ||I.--. ||N.--. ||N.--. ||E.--. ||R.--. |"),nl,
        write("| :/\\: || (\\/) || :(): || :/\\: || (\\/) || :(): |"),nl,
        write("| :\\/: || :\\/: || ()() || (__) || :\\/: || ()() |"),nl,
        write("| '--'W|| '--'I|| '--'N|| '--'N|| '--'E|| '--'R|"),nl,
        write("`------'`------'`------'`------'`------'`------'"),nl,
	write("         type 'start.' to play again"),
	nl,nl,nl,nl,nl,nl
        ;
	%Else, print "LOSER" visual.
	nl,nl,nl,nl,nl,nl,nl,nl,nl,
	write("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),nl,nl,
	write("	 There are no more moves"),nl,nl,
	write(".------..------..------..------..------."),nl,
        write("|L.--. ||O.--. ||S.--. ||E.--. ||R.--. |"),nl,
        write("| :/\\: || :/\\: || :/\\: || (\\/) || :(): |"),nl,
	write("| (__) || :\\/: || :\\/: || :\\/: || ()() |"),nl,
        write("| '--'L|| '--'O|| '--'S|| '--'E|| '--'R|"),nl,
        write("`------'`------'`------'`------'`------'"),nl,nl,
	write("      type 'start.' to play again"),nl,nl,
	write("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"),
	nl,nl,nl,nl,
	render.

% True if card is a king
is_a_king(card(13,_,_)).

% called during setup --> flips over top card in each row (takes from
% "down" row, and adds to "up" row.
flip_top_cards(X) :-
	row(X,down,RD),
	flip_card(X,RD,[]),
	X2 is (X + 1),
	flip_top_cards(X2).

% Stop flip_top_cards recursion when accumulater reaches 8
flip_top_cards(8).

% Don't flip a card if the "up" row still contains cards after one is
% removed
flip_card(_,RD,RU) :-
	\+(RD = []),
	\+(RU = []).

% if a row or card is taken off of a "down" row, flip over the top card
% in that row
flip_card(R,RD1,[]) :-
	\+ (RD1 = []),
	row(R,down,RD1),
	row(R,down,[C|RD]),
	row(R,up,RU),
	retractall(row(R,down,[C|RD])),
	retractall(row(R,up,RU)),
	assert(row(R,down,RD)),
	assert(row(R,up,[C|RU])).

% If there is no "down" row, then do nothing.
flip_card(_,[],_).

% Plays_on(C1,C2) evals to true if C1 (a card) can be placed on top of a
% face-up card on the table (not including foundations).
plays_on(card(V1,_,C1),card(V2,_,C2)) :-
    V1 =:= (V2 - 1),
    \+ (C1 = C2).

% Finishes_on(C1,C2) evals to true if C1 can be added to the top card
% of an foundation of the same suit
finishes_on(card(V1,S,_),card(V2,S,_)) :-
	V2 =:= (V1 - 1).

% Renders a visualization of the gamearea
render :-
	row(7,down,R7),row(6,down,R6),
	row(5,down,R5),row(4,down,R4),
	row(3,down,R3),row(2,down,R2),
	row(1,down,R1),row(7,up,R7U),
	row(6,up,R6U),row(5,up,R5U),
	row(4,up,R4U),row(3,up,R3U),
	row(2,up,R2U),row(1,up,R1U),
	deck(RD),
	endpile(hearts,[Hrt|_]),endpile(spades,[Spd|_]),
	endpile(diamonds,[Dmd|_]),endpile(clubs,[Clb|_]),
        convert_down(R7,[],E7),convert_down(R6,[],E6),
	convert_down(R5,[],E5),convert_down(R4,[],E4),
	convert_down(R3,[],E3),convert_down(R2,[],E2),
	convert_down(R1,[],E1),convert(R1U,[],E1U),
	convert(R2U,[],E2U),convert(R3U,[],E3U),
	convert(R4U,[],E4U),convert(R5U,[],E5U),
	convert(R6U,[],E6U),convert(R7U,[],E7U),
	convert(Hrt,SH),convert(Spd,SS),
	convert(Dmd,SD),convert(Clb,SC),
        concat_list(E7,S7),concat_list(E6,S6),
	concat_list(E5,S5),concat_list(E4,S4),
	concat_list(E3,S3),concat_list(E2,S2),
	concat_list(E1,S1),concat_list(E7U,S7U),
	concat_list(E6U,S6U),concat_list(E5U,S5U),
	concat_list(E4U,S4U),concat_list(E3U,S3U),
	concat_list(E2U,S2U),concat_list(E1U,S1U),
	length(RD,DeckCount),
	nl,write("[<>] x "),write(DeckCount),nl,nl,
	write("--------"),nl,
	write(SH),nl,write(SS),nl,
	write(SD),nl,write(SC),nl,
	write("--------"),nl,nl,
	write("7  "),write(S7),write(S7U),nl,nl,
	write("6  "),write(S6),write(S6U),nl,nl,
	write("5  "),write(S5),write(S5U),nl,nl,
	write("4  "),write(S4),write(S4U),nl,nl,
	write("3  "),write(S3),write(S3U),nl,nl,
	write("2  "),write(S2),write(S2U),nl,nl,
	write("1  "),write(S1),write(S1U),nl,nl,
	write("=================================================").

% Makes a list of "[<>]" symbols given a list of face-down cards
convert_down([_|T],R,E) :-
	convert_down(T,["[<>]"|R],E).
convert_down([],R,R).

% Makes a list of strings representing cards given a list of face-up
% Cards eg card(1,up,hrts,red) converts to "[1hrts]"
convert([card(A,B,_)|T],R3,E) :-
	string_concat(A,B,C),
	string_concat("[",C,C2),
	string_concat(C2,"]",C3),
	convert(T,[C3|R3],E).
convert([],R3,R3).

% Converts a single card to a string representing that card
convert(card(A,B,_),C3) :-
	string_concat(A,B,C),
	string_concat("[",C,C2),
	string_concat(C2,"]",C3).

% Concat list of "card strings" into one "row string" to print to screen
concat_list(L,S) :-
	atomic_list_concat(L,'', A),
	atom_string(A, S).
