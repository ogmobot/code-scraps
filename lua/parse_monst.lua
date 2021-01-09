concat_lists = function(l1, l2)
    ltotal = {}
    for index, value in ipairs(l1) do
        ltotal[#ltotal + 1] = value
    end
    for index, value in ipairs(l2) do
        ltotal[#ltotal + 1] = value
    end
    return ltotal
end

parse_level = function(text)
    -- text should be a string of the form "LVL(2, 18, 3, 0, 0)"
    -- (lvl, mov, ac, mr, aln)
    local index = 1
    local numbers = {}
    while index < #text do
        found, index, num = text:find("([%d%-]+)", index)
        if found then
            index = index + 1
            numbers[#numbers + 1] = tonumber(num)
        else
            break
        end
    end
    result = {
        ["mlevel"]              = numbers[1], -- hit dice (0 = 1/2 HD)
        ["mmove"]               = numbers[2], -- arbitrary speed units, where 12 = ordinary human
        ["armour class"]        = numbers[3], -- before armour
        ["magic resistance"]    = numbers[4], -- before armour
        ["alignment"]           = numbers[5],
    }
    if result["alignment"] then
        if result["alignment"] > 0 then
            result["alignment"] = "lawful (" .. result["alignment"] .. ")"
        elseif result["alignment"] < 0 then
            result["alignment"] = "chaotic (" .. result["alignment"] .. ")"
        else
            result["alignment"] = "neutral"
        end
    end
    return result
end

parse_attacks = function(text)
    -- "A(ATTK(at, ad, n, d) .. ATTK(at, ad, n, d))"
    -- from monattk.h
    local attack_types = {
        AT_NONE = "passive",        AT_CLAW = "claw",           AT_BITE = "bite",               AT_KICK = "kick", AT_BUTT = "head butt",
        AT_TUCH = "touch",          AT_STNG = "sting",          AT_HUGS = "crushing bearhug",   AT_SPIT = "spit", AT_ENGL = "engulf",
        AT_BREA = "breath attack",  AT_EXPL = "explosion",      AT_BOOM = "death explosion",    AT_GAZE = "gaze", AT_TENT = "tentacle",
        AT_WEAP = "weapon",         AT_MAGC = "cast spell",
    }
    local damage_types = {
        AD_PHYS = "physical",           AD_MAGM = "magic missile",  AD_FIRE = "fire",               AD_COLD = "cold",               AD_SLEE = "sleep",
        AD_DISN = "disintegration",     AD_ELEC = "electric",       AD_DRST = "poison",             AD_ACID = "acid",               AD_BLND = "blinding",
        AD_STUN = "stun",               AD_SLOW = "slow",           AD_PLYS = "paralyse",           AD_DRLI = "drain level",        AD_DREN = "drain magic",
        AD_LEGS = "injure legs",        AD_STON = "petrification",  AD_STCK = "sticky",             AD_SGLD = "steal gold",         AD_SITM = "steal item",
        AD_SEDU = "seduction",          AD_TLPT = "forced teleport",AD_RUST = "rust",               AD_CONF = "confusion",          AD_DGST = "digestion",
        AD_HEAL = "heal HP",            AD_WRAP = "grapple",        AD_WERE = "cause lycanthropy",  AD_DRDX = "drain dexterity",    AD_DRCO = "drain constitution",
        AD_DRIN = "drain intelligence", AD_DISE = "disease",        AD_DCAY = "rot",                AD_SSEX = "fade-to-black seduction",AD_HALU = "cause hallucination",
        AD_DETH = "Death's Touch",      AD_PEST = "Pestilence's Touch",AD_FAMN = "Famine's Touch",  AD_SLIM = "turn to slime",      AD_ENCH = "disenchant",
        AD_CORR = "corrode",            AD_POLY = "forced polymorph",AD_CLRC = "as cleric",         AD_SPEL = "as wizard",          AD_RBRE = "random breath type",
        AD_SAMU = "steal Amulet",       AD_CURS = "steal ability",
    }

    local index = 1
    local attacks = {}
    local ATTK = function (at, ad, n, d)
        return attack_types[at] .. " " .. n .. "d" .. d .. " (" .. damage_types[ad] .. ")"
    end
    local regex = "ATTK%(([^,]*),%s*([^,]*),%s*([^,]*),%s*([^%)]*)%)"
    while true do
        local start, finish, at, ad, n, d = text:find(regex, index)
        if start then
            attack = ATTK(at, ad, n, d)
            table.insert(attacks, attack)
            index = finish + 1
        else
            break
        end
    end
    return attacks
end

parse_size = function(text)
    return {}
end

merge_flags = function(text)
    flag_text = {
        MR_FIRE = "fire resistance",            MR_COLD = "cold resistance",            MR_SLEEP = "sleep resistance",
        MR_DISINT = "disintegration resistance",MR_ELEC = "shock resistance",           MR_POISON = "poison resistance",
        MR_ACID = "acid resistance",            MR_STONE = "petrification resistance",

        M1_FLY          = "can fly or float",               M1_SWIM         = "can traverse water",                 M1_AMORPHOUS    = "can flow under doors",
        M1_WALLWALK     = "can phase thru rock",            M1_CLING        = "can cling to ceiling",               M1_TUNNEL       = "can tunnel thru rock",
        M1_NEEDPICK     = "needs pick to tunnel",           M1_CONCEAL      = "hides under objects",                M1_HIDE         = "mimics, blends in with ceiling",
        M1_AMPHIBIOUS   = "can survive underwater",         M1_BREATHLESS   = "doesn't need to breathe",            M1_NOTAKE       = "cannot pick up objects",
        M1_NOEYES       = "no eyes to gaze into or blind",  M1_NOHANDS      = "no hands to handle things",          M1_NOLIMBS      = "no arms/legs to kick/wear on",
        M1_NOHEAD       = "no head to behead",              M1_MINDLESS     = "has no mind--golem, zombie, mold",   M1_HUMANOID     = "has humanoid head/arms/torso",
        M1_ANIMAL       = "has animal body",                M1_SLITHY       = "has serpent body",                   M1_UNSOLID      = "has no solid or liquid body",
        M1_THICK_HIDE   = "has thick hide or scales",       M1_OVIPAROUS    = "can lay eggs",                       M1_REGEN        = "regenerates hit points",
        M1_SEE_INVIS    = "can see invisible creatures",    M1_TPORT        = "can teleport",                       M1_TPORT_CNTRL  = "controls where it teleports to",
        M1_ACID         = "acidic to eat",                  M1_POIS         = "poisonous to eat",                   M1_CARNIVORE    = "carnivorous",
        M1_HERBIVORE    = "herbivorous",                    M1_OMNIVORE     = "omnivorous",                         M1_METALLIVORE  = "eats metal",

        M2_NOPOLY       = "players mayn't poly into one",   M2_UNDEAD       = "is walking dead",                    M2_WERE         = "is a lycanthrope",
        M2_HUMAN        = "is a human",                     M2_ELF          = "is an elf",                          M2_DWARF        = "is a dwarf",
        M2_GNOME        = "is a gnome",                     M2_ORC          = "is an orc",                          M2_DEMON        = "is a demon",
        M2_MERC         = "is a guard or soldier",          M2_LORD         = "is a lord to its kind",              M2_PRINCE       = "is an overlord to its kind",
        M2_MINION       = "is a minion of a deity",         M2_GIANT        = "is a giant",                         M2_SHAPESHIFTER = "is a shapeshifting species",
        M2_MALE         = "always male",                    M2_FEMALE       = "always female",                      M2_NEUTER       = "neither male nor female",
        M2_PNAME        = "monster name is a proper name",  M2_HOSTILE      = "always starts hostile",              M2_PEACEFUL     = "always starts peaceful",
        M2_DOMESTIC     = "can be tamed by feeding",        M2_WANDER       = "wanders randomly",                   M2_STALK        = "follows you to other levels",
        M2_NASTY        = "extra-nasty monster (more xp)",  M2_STRONG       = "strong (or big) monster",            M2_ROCKTHROW    = "throws boulders",
        M2_GREEDY       = "likes gold",                     M2_JEWELS       = "likes gems",                         M2_COLLECT      = "picks up weapons and food",
        M2_MAGIC        = "picks up magic items",

        M3_WANTSAMUL    = "would like to steal the amulet", M3_WANTSBELL    = "wants the bell",                     M3_WANTSBOOK    = "wants the book",
        M3_WANTSCAND    = "wants the candelabrum",          M3_WANTSARTI    = "wants the quest artifact",           M3_WANTSALL     = "wants any major artifact",
        M3_WAITFORU     = "waits to see you or get attacked",M3_CLOSE       = "lets you close unless attacked",     M3_COVETOUS     = "wants something",
        M3_WAITMASK     = "waiting...",                     M3_INFRAVISION  = "has infravision",                    M3_INFRAVISIBLE = "sees by infravision",
        M3_DISPLACES    = "moves monsters out of its way",
    }
    local index = 1
    local flags = {}
    while index < #text do
        found, index, flag = text:find("(M._%a*)", index)
        if found then
            index = index + 1
            flags[#flags + 1] = flag_text[flag]
        else
            break
        end
    end
    return flags
end

calculate_hit_dice = function(monster)
    -- no return value
    local hd = monster["mlevel"] or 0
    local hd_string = ""
    if monster["symbol"] == "S_GOLEM" then
        if monster["name"] == "paper golem" or monster_name == "straw golem" then
            hd_string = "20"
        elseif monster["name"] == "rope golem" then
            hd_string = "30"
        elseif monster["name"] == "flesh golem" or monster_name == "gold golem" or monster_name == "leather golem" then
            hd_string = "40"
        elseif monster["name"] == "clay golem" or monster_name == "wood golem" then
            hd_string = "50"
        elseif monster["name"] == "glass golem" or monster_name == "stone golem" then
            hd_string = "60"
        elseif monster["name"] == "iron golem" then
            hd_string = "80"
        else
            hd_string = "20?"
        end
    elseif monster["name"] == "Death" or monster["name"] == "Famine" or monster["name"] == "Pestilence" then
        hd_string = "10d8"
    elseif monster["symbol"] == "S_DRAGON" then
        hd_string = hd .. "d4 + " .. (hd * 4)
    elseif 50 <= hd and hd <= 127 then
        local exact_hp = (2 * (hd - 6))
        hd_string = exact_hp .. " HP (" .. (exact_hp // 4) .. " HD)"
    elseif hd == 0 then
        hd_string = "1d4"
    else
        hd_string = hd .. "d8"
    end
    monster["hit dice"] = hd_string
end

parse_monster = function(text)
    -- Parses an entry from monst.c, e.g. text = "MON(\"gargoyle\", S_GREMLIN, ... )"
    -- name, symbol, level macro, gen flags, atk macro, siz macro, mr1, mr2, flg1, flg2, flg3, d, colour
    -- mr1 is monster resistances, mr2 is resistances conveyed by eating the monster
    -- Returns the table.
    local index = 1
    local monster = {}
    local start, finish = 0, 0
    while text:sub(index, index) ~= "M" do
        index = index + 1
    end
    -- is this MON or MON3?
    if text:sub(index, index + 3) == "MON3" then
        -- MON3 macro
        start, finish, monster["name"] = text:find("MON3%([^,]*,%s*[^,]*,%s*\"([^,]*)\",%s*", index)
    else
        -- assume MON macro
        start, finish, monster["name"] = text:find("MON%(\"([^,]*)\",%s*", index)
    end
    if start then
        index = finish + 1
    end
    start, finish, monster["symbol"] = text:find("([^,]*),%s*", index)
    if start then
        index = finish + 1
    end
    local tmp_string = ""
    start, finish, tmp_string = text:find("(LVL%b()),%s*")
    if start then
        local level_data = parse_level(tmp_string)
        for k, v in pairs(level_data) do
            monster[k] = v
        end
        index = finish + 1
    end
    start, finish, monster["generation flags"] = text:find("([^,]*),%s*", index)
    if start then
        index = finish + 1
    end
    local attacks_data = {}
    start, finish, tmp_string = text:find("(A%b()),%s*") -- %b is OP
    if start then
        monster["attacks"] = parse_attacks(tmp_string)
        index = finish + 1
    end
    start, finish, tmp_string = text:find("(SIZ%b()),%s*")
    if start then
        local size_data = parse_size(tmp_string)
        for k, v in pairs(size_data) do
            monster[k] = v
        end
        index = finish + 1
    end
    start, finish, tmp_string = text:find("([^,]*),%s*", index)
    if start then
        monster["resistances"] = merge_flags(tmp_string)
        index = finish + 1
    end
    start, finish, tmp_string = text:find("([^,]*),%s*", index)
    if start then
        monster["resistances conveyed"] = merge_flags(tmp_string)
        index = finish + 1
    end

    monster["flags"] = {}
    monster["extra flags"] = {} -- probably don't need these for print version
    start, finish, tmp_string = text:find("([^,]*),%s*", index)
    if start then
        monster["flags"] = concat_lists(monster["flags"], merge_flags(tmp_string))
        index = finish + 1
    end
    start, finish, tmp_string = text:find("([^,]*),%s*", index)
    if start then
        monster["extra flags"] = concat_lists(monster["extra flags"], merge_flags(tmp_string))
        index = finish + 1
    end
    start, finish, tmp_string = text:find("([^,]*),%s*", index)
    if start then
        monster["extra flags"] = concat_lists(monster["extra flags"], merge_flags(tmp_string))
        index = finish + 1
    end

    calculate_hit_dice(monster)
 
---[[
    for k, v in pairs(monster) do
        if k == "attacks" then
            for ka, va in pairs(monster[k]) do
                print("attack: " .. va)
            end
        elseif type(v) == "table" then
            print(k .. ":")
            for ka, va in ipairs(monster[k]) do
                print("  " .. va)
            end
        else
            print(k .. ": " .. v)
        end
    end
--]]

    return monster
end

run_test = function()
    parse_monster([[MON("fire ant", S_ANT, LVL(3, 18, 3, 10, 0), (G_GENO | G_SGROUP | 1),
        A(ATTK(AT_BITE, AD_PHYS, 2, 4), ATTK(AT_BITE, AD_FIRE, 2, 4), NO_ATTK,
          NO_ATTK, NO_ATTK, NO_ATTK),
        SIZ(30, 10, MS_SILENT, MZ_TINY), MR_FIRE, MR_FIRE,
        M1_ANIMAL | M1_NOHANDS | M1_OVIPAROUS | M1_CARNIVORE, M2_HOSTILE,
        M3_INFRAVISIBLE, 6, CLR_RED)]])
    print("===")
    parse_monster([[MON("Demogorgon", S_DEMON, LVL(106, 15, -8, 95, -20),
        (G_HELL | G_NOCORPSE | G_NOGEN | G_UNIQ),
        A(ATTK(AT_MAGC, AD_SPEL, 8, 6), ATTK(AT_STNG, AD_DRLI, 1, 4),
          ATTK(AT_CLAW, AD_DISE, 1, 6), ATTK(AT_CLAW, AD_DISE, 1, 6), NO_ATTK,
          NO_ATTK),
        SIZ(1500, 500, MS_GROWL, MZ_HUGE), MR_FIRE | MR_POISON, 0,
        M1_FLY | M1_SEE_INVIS | M1_NOHANDS | M1_POIS,
        M2_NOPOLY | M2_DEMON | M2_STALK | M2_HOSTILE | M2_PNAME | M2_NASTY
            | M2_PRINCE | M2_MALE,
        M3_WANTSAMUL | M3_INFRAVISIBLE | M3_INFRAVISION, 57, HI_LORD)]])
    print("===")
    parse_monster([[MON("King Arthur", S_HUMAN, LVL(20, 12, 0, 40, 20), (G_NOGEN | G_UNIQ),
        A(ATTK(AT_WEAP, AD_PHYS, 1, 6), ATTK(AT_WEAP, AD_PHYS, 1, 6), NO_ATTK,
          NO_ATTK, NO_ATTK, NO_ATTK),
        SIZ(WT_HUMAN, 400, MS_LEADER, MZ_HUMAN), 0, 0,
        M1_HUMANOID | M1_OMNIVORE,
        M2_NOPOLY | M2_HUMAN | M2_PNAME | M2_PEACEFUL | M2_STRONG | M2_MALE
            | M2_COLLECT | M2_MAGIC,
        M3_CLOSE | M3_INFRAVISIBLE, 23, HI_LORD)]])
end
