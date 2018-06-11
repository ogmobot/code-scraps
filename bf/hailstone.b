[
  Array structure:
  c0 - counter
  c1 - stores n
  c2 - buffer
  c3 - stores whether n is odd
  c4 - buffer for finding n%2
]

>,                        get input
-[+                       while input is not 1
  [->>+<<] >> [-<+<+>>] <   copy c1 to c2 (using c3 as buffer)
  [->[>+<-]+>[<->-]<<] >    store c2 modulo 2 in c3 (using repeating NOTS)
  [                         while c3 is 1
    - <<                      set c3 to 0
    [->+++<]>+                set c2 to 3 times c1 plus 1
    [-<++>]>                  set c1 to 2 times c2
  ]                         end loop
  << [-->+<] >              set c2 to c1 over 2
  [-<+>] <                  set c1 to c2
  <+>                       increment counter
  .                         print n
-]                        end loop
