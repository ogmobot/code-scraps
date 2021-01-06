NB. define 'count_fives' as 
NB. +/ (reduce with +) @: (compose) =&5 (values x such that x = 5)
count_fives=:+/@:=&5
count_n=:+/@:=~

NB. Count the number of fives in the list:
echo count_fives 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 6 6 6 7 7 7
NB. exit''
