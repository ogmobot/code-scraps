#!/usr/bin/lua

--[[
    John Conway's Game of Life.
    Codes used to indicate cell status:
        1=dead->dead
        2=alive->dead
        3=dead->alive
        4=alive->alive
--]]

LIVECELL = " #"
DEADCELL = " ."
GRIDSIZE = 20

sleep = function (seconds)
    os.execute("sleep " .. tostring(seconds))
end

countneighbours = function (state, row, col)
    local count = 0
    for r=-1,1 do
        for c=-1,1 do
            if (r ~= 0) or (c ~= 0) then
                therow = state[row+r]
                if therow then
                    thecell = therow[col+c]
                    if thecell then
                        count = count + 1 - (thecell % 2)
                    end
                end
            end
        end
    end
    return count
end

printstate = function (state)
    for r, row in ipairs(state) do
        for c, cell in ipairs(row) do
            if cell and cell%2 == 0 then
                io.write(LIVECELL)
            else
                io.write(DEADCELL)
            end
        end
        io.write("\n")
    end
end

updatestate = function (state, rules)
    for r, row in ipairs(state) do
        for c, cell in ipairs(row) do
            if rules[cell][countneighbours(state, r, c)] then
                row[c] = cell + 2
            end
        end
    end
    for r, row in ipairs(state) do
        for c, cell in ipairs(row) do
            if cell > 2 then
                row[c] = 2
            else
                row[c] = 1
            end
        end
    end
end

Simulation = function (rules, cycles)
    local rules = rules or {{[3]=true}, {[2]=true, [3]=true}}
    local state = {}
    local cycles = cycles or 100
    math.randomseed(os.time())
    for i=1,GRIDSIZE do
        local newrow = {}
        for j=1,GRIDSIZE do
            table.insert(newrow, math.random(2))
        end
        table.insert(state, newrow)
    end
    ---[[
    for i=1,cycles do
        io.write("\n")
        printstate(state)
        updatestate(state, rules)
        sleep(0.2)
    end
    --]]
    --printstate(state)
end

dofile("gameoflife.rc")
