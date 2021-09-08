make_circle = function()
    local circle = {}
    circle.x = love.math.random(800)
    circle.y = love.math.random(600)
    circle.colour = {
        ["r"]=love.math.random()/4 + 0.75,
        ["g"]=love.math.random()/2 + 0.5,
        ["b"]=love.math.random()/4 + 0.75,
    }
    return circle
end

set_colour = function(tab)
    love.graphics.setColor(tab.r, tab.g, tab.b)
end

love.load = function()
    circles = {}
    for _=1,100 do
        table.insert(circles, make_circle())
    end
end

love.update = function()
    for _, c in pairs(circles) do
        c.x = c.x + love.math.random(3) - 2
        c.y = c.y + love.math.random(3) - 2
    end
end

love.draw = function()
    for _, c in pairs(circles) do
        set_colour(c.colour)
        love.graphics.circle("line", c.x, c.y, 5)
    end
end
