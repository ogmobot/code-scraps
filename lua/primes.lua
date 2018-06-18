local primecache = {}

isprime = function (n)
    if primecache[n] ~= nil then return primecache[n] end
    local result = true
    for i=2,math.sqrt(n) do
        if n % i == 0 then
            result = false
            break
        end
    end
    primecache[n] = result
    return result
end

primesuntil = function (maxnum)
    maxnum = maxnum or 100
    local n
    iterator = function ()
        n = n or 1
        while n < maxnum do
            n = n + 1
            if isprime(n) then return n end
        end
    end
    return iterator
end

makemersenne = function (p) -- assume p is prime
    local candidate = (2^p)-1
    if isprime(candidate) then return candidate end
end

perfects = function (mmc) -- maximum mersenne prime candidates
    mmc = mmc or 10
    local result = {}
    getprime = primesuntil(1000000)
    for i=1, mmc do
        local p = makemersenne(getprime())
        if p then table.insert(result, p) end
    end
    for i, p in ipairs(result) do
        result[i] = (p*(p+1))/2
    end
    return result
end
