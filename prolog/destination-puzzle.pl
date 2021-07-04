% Given 7 cities,
% Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta,
% * one city received four votes,
% * two cities received two votes each,
% * two cities received one vote each,
% * two cities received no votes,
% * Beijing and Cairo received a different number of votes,
% * Moscow either got four votes or no votes,
% * Cairo got more votes than Jakarta,
% * In the list of cities above, each of the two cities with two votes succeeds a city that got no votes,
% * Jakarta got either one vote fewer than London or one vote fewer than Beijing.

num_pairs([],_,0).
num_pairs([X,Y|T],[X,Y],N) :- num_pairs(T,[X,Y],Nminus1), N is Nminus1+1.
num_pairs([Z|T],[X,Y],N) :- Z \= X, num_pairs(T,[X,Y],N).

puzzle([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta]) :-
    permutation([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta], [4, 2, 2, 1, 1, 0, 0]),
    (Cairo \== Beijing),
    (Moscow = 0 ; Moscow = 4),
    (Cairo > Jakarta),
    num_pairs([Cairo, London, Beijing, Moscow, Mumbai, Nairobi, Jakarta], [0, 2], 2),
    (Jakarta is (London - 1) ; Jakarta is (Beijing - 1)).
