#!/usr/bin/lua

run_bf = function(prog)
    tape = {}
    jumpcache = {} -- stores pointers from '[' to ']' for easy jumps
    loopstack = {} -- stores addrsses of '['s for easy jumps
    ptr = 1
    pc = 1
    for i = 1, #prog do
        local c = prog:sub(i,i)
        if c == "[" then
            table.insert(loopstack,i)
        elseif c == "]" then
            jumpcache[assert(table.remove(loopstack))]=i
        end
    end
    assert(#loopstack == 0) -- ensures brackets are balanced
    while pc <= #prog do
        local c = prog:sub(pc,pc)
        if c == ">" then
            ptr = ptr + 1
        elseif c == "<" then
            ptr = ptr - 1
        elseif c == "+" then
            tape[ptr] = (tape[ptr] or 0) + 1
        elseif c == "-" then
            tape[ptr] = (tape[ptr] or 0) - 1
        elseif c == "," then
            tape[ptr] = string.byte(io.read(1))
        elseif c == "." then
            io.write(string.char(tape[ptr]))
        elseif c == "[" then
            if tape[ptr] and tape[ptr] ~= 0 then
                table.insert(loopstack, pc)
            else
                pc = jumpcache[pc]
            end
        elseif c == "]" then
            pc = table.remove(loopstack)
            pc = pc - 1
        end
        pc = pc + 1
    end
end

filename = arg[1]
inputfile = assert(io.open(filename,"r"))
programstring = inputfile:read("*a")
run_bf(programstring)
