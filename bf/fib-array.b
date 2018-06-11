>>>,             choose the number of Fibonacci numbers to compute (c1)
                 state eg: (0 0 6*)
[[>>]+[<<]>>-]   put a 1 in the next even empty cell and decrement c1
                 state eg: (0 0 5* 0 1)
                         : (0 0 4* 0 1 0 1)
                         : (0 0 3* 0 1 0 1 0 1)
                         : (0 0 2* 0 1 0 1 0 1 0 1)
                         : (0 0 1* 0 1 0 1 0 1 0 1 0 1)
                         : (0 0 0* 0 1 0 1 0 1 0 1 0 1 0 1)
+<+>>+<          initialise the Fibonacci array
                 state eg: (0 a 1* b 1 0 1 0 1 0 1 0 1 0 1)
[                loop
 -               state eg: (0 a 0* b 1 0 1 0 1 0 1 0 1 0 1)
 <[->+>>>+<<<<]  copy a cell to buffer and sum
                 state eg: (0 0* a b 1 a 1 0 1 0 1 0 1 0 1)
 >[-<+>]                    _ a 0* _
 >[-<+>>>+<<]    copy b cell to buffer and sum
                 state eg: (0 a b 0* 1 c 1 0 _
 <[->+<]                      _ 0* b _
 >>
]
<[-] <<[-] <<    delete the last two numbers
[<<]             go back to the start
>>[.>>]          and output the rest
                 (fancy output code goes here)
