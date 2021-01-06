NB. J seems particularly suited to solving the array-based task of 2020-12-01.

input=:".@:>cutopen toJ 1!:1<'advent-2020-1.txt'

NB. Part 1: find two numbers that sum to 2020.
NB. For each element in the list array[i], test whether 2020-array[i] is contained in array[i+1..]
within=: +/@:=
conjugate_exists=: ((2020 - {. @ [) within (}. @ [))
solve=: ((solve @: }.) ` ({.) @. (0 < conjugate_exists))
echo ([ * 2020 - [) solve input

NB. echo ([*2020-[)$:@:}.`{.@.(0<((2020-{.@[)+/@:=}.@[)) input
