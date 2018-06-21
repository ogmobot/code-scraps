[
  Array structure:
  c0 = counter (not used)
  c1 = stores n
  c2 = buffer
  c3 = stores whether n is odd
  c4 = buffer for finding n%2
]

>
                          ==input routine==
                          with thanks to Urban Mueller
>+
[[-]                // Begin loop on first temp
>[-],               // Clear the inp buffer to detect leave on eof and input
  [
    +[                          // Check for minus one on eof
      -----------[            // Check for newline
        >[-]++++++[<------>-]       // Subtract 38 to get the char in zero to nine
        <--<<[->>++++++++++<<]      // Multiply the existing value by ten
        >>[-<<+>>]          // and add in the new char
      <+>]
    ]
  ]
<]
<
   // Current cell is the number input

                          ==end input routine==

-[+                       while input is not 1

                            ==output routine==
                            with thanks to esolang wiki

                            ;copy cell to workspace and back
  [>>+>+<<<-]>>>
  [<<<+>>>-]<<+>
  [<->[                     ;while value exists
     >++++++++++<           ;make a 10
     [->-[>+>>]>[+[-<+>]>+>>]<<<<<] ;divide value by 10
     >[-]                   ;dont need this cell
     ++++++++[<++++++>-]    ;add 48 to remainder
     >[<<+>>-]              ;store the remainder
     >[<<+>>-]              ;get next value
     <<
  ]>]
  <[-                       ;else need to make a zero
     >>++++++++[<++++++>-]
  ]
    ;print and clear each stored remainder in reverse
  <[.[-]<] 
  
  ++++++++++.[-]<          \n
                            ==end output routine==

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
-]                        end loop

+++++++[->+++++++<]>.     write a 1
<++++++++++.              \n
