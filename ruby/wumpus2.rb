NOTHING = 0; WUMPUS = 1; BAT = 2; PIT = 4; PLAYER = 8; FINISH = 1
PLAY_NEW = 2; PLAY_SAME = 4; S_PLAYER = 0;  MAX_ARROWS = 5; S_WUMPUS = 1
S_BAT1 = 2; S_BAT2 = 3; S_PIT1 = 4; S_PIT2 = 5; MAX_ROOMS = 20; SAVED = 6
MAX_EXITS = 3; A_PATH_LEN = 5
CAVETYPES = [
         # Dodecahedron
         [1, 4, 7, 0, 2, 9, 1, 3, 11, 2, 4, 13, 0, 3, 5, 4, 6, 14, 5, 7,
          16, 0, 6, 8, 7, 9, 17, 1, 8, 10, 9, 11, 18, 2, 10, 12, 11, 13, 
          19, 3, 12, 14, 5, 13, 15, 14, 16, 19, 6, 15, 17, 8, 16, 18, 
          10, 17, 19, 12, 15, 18],
         # Mobius Strip
         [20, 2, 3, 19, 1, 4, 1, 4, 5, 2, 3, 6, 3, 6, 7,
          4, 5, 8, 5, 8, 9, 6, 7, 10, 7, 10, 11, 8, 9, 12,
          9, 12, 13, 10, 11, 14, 11, 14, 15, 12, 13, 16, 13, 16, 17,
          14, 15, 18, 15, 18, 19, 16, 17, 20, 2, 17, 20, 1, 18, 19],
         # String of Beads
         [2, 3, 20, 1, 3, 4, 1, 2, 4, 2, 3, 5, 4, 6, 7,
          5, 7, 8, 5, 6, 8, 6, 7, 9, 8, 10, 11, 9, 11, 12,
          9, 10, 12, 10, 11, 13, 12, 14, 15, 13, 15, 16, 13, 14, 16,
          14, 15, 17, 16, 18, 19, 17, 19, 20, 17, 18, 20, 1, 18, 19],
         # Net on Torus omitted
         # Dendrite w/ Degeneracies
         [1, 1, 5, 2, 2, 5, 3, 3, 6, 4, 4, 6, 1, 2, 7,
          3, 4, 7, 5, 6, 10, 8, 9, 9, 8, 8, 10, 7, 9, 11,
          10, 13, 14, 12, 13, 13, 11, 12, 12, 11, 15, 16, 14, 17, 18,
          14, 19, 20, 15, 17, 17, 15, 18, 18, 16, 19, 19, 16, 20, 20],
         # One-way Lattice
         [5, 4, 8, 1, 5, 6, 2, 6, 7, 3, 7, 8, 8, 9, 12,
          5, 9, 10, 6, 10, 11, 7, 11, 12, 12, 13, 16, 9, 13, 14,
          10, 14, 15, 11, 15, 16, 16, 17, 20, 13, 17, 18, 14, 18, 19,
          15, 19, 20, 1, 4, 20, 1, 2, 17, 2, 3, 18, 3, 4, 19]
        ]

class Room
    attr_reader :obj
    @obj; @exit
    def initialize()
        @obj = NOTHING
        @exit = Array.new(MAX_EXITS) 
    end
    def get_exit(i)
        return @exit[i]
    end
    def clear(obj)
        @obj -= obj
        if @obj < NOTHING then @obj = NOTHING end
    end
    def empty()
        @obj = NOTHING
    end
    def set_exit(i, e)
        @exit[i] = e
    end
    def set_object(obj)
        @obj = @obj + obj
    end
end
class Cave
    @rooms
    def initialize()
        @rooms = Array.new(MAX_ROOMS)
        #exits = [1, 4, 7, 0, 2, 9, 1, 3, 11, 2, 4, 13, 0, 3, 5, 4, 6, 14, 5, 7,
                 #16, 0, 6, 8, 7, 9, 17, 1, 8, 10, 9, 11, 18, 2, 10, 12, 11, 13, 
                 #19, 3, 12, 14, 5, 13, 15, 14, 16, 19, 6, 15, 17, 8, 16, 18, 
                 #10, 17, 19, 12, 15, 18]
        exits = choose_cave()
        for i in 0..@rooms.length
            @rooms[i] = Room.new()
            r = i * MAX_EXITS; a = 0
            for e in r..(r + (MAX_ROOMS - 1))
                @rooms[i].set_exit(a, exits[e])
                a += 1
            end
        end
    end
    def clear_all_rooms()
        @rooms.each do |room|
            room.empty()
        end
    end
    def room(i)
        return @rooms[i]
    end
    def choose_cave()
        ex = -1
        while ex < 0 or ex >= CAVETYPES.length
          print "Cave #(0-%s) " % [CAVETYPES.length-1]; ex = gets.to_i
        end
        return CAVETYPES[ex]
    end
end
class Game
    @cave; @player; @wumpus; @arrows; @exits; @path; @saved; 
    @gameOver; @playerWins; @game_res
 
    def initialize()
        @player = MAX_ROOMS; @game_res = PLAY_NEW
        @exits = Array.new(MAX_EXITS); @path = Array.new(A_PATH_LEN);
        @saved = Array.new(SAVED); @cave = Cave.new()
        instructions()
    end
    def look()
        room = @cave.room(@player)
        puts "\n-----------------------------------"
        puts "You are in room ##{@player + 1}"
        print "Tunnels lead to rooms #: "
 
        @exits.each do |x|
            print "#{1 + x} " 
        end
        look_around()
    end
    def shoot(pathLen)
        room = @cave.room(@player)
        @path.each do |x|
            if exits(x)
                r = @cave.room(x)
            else 
                r = @cave.room(room.get_exit(rand(MAX_EXITS)))
             end
            obj = r.obj
            if ((WUMPUS & obj) == WUMPUS) 
                 @gameOver = true; @playerWins = true
                return
            end 
            if ((PLAYER & obj) == PLAYER) 
                @gameOver = true; @playerWins = false
                puts "  OUCH! Arrow got you!\n"
                return
            end
        end
        puts "  Missed!\n"; @arrows -= 1
        if @arrows == 0
            iputs "  You run out of arrows...\n"
            @gameOver = true; @playerWins = false
            return
        end
        wumpus(@player)
    end
    def result()
        if @playerWins
            puts "\n  AHA! You got the Wumpus!\n  HEE HEE HEE - " <<
            "The Wumpus'll getcha next time!!\n\n"
        else 
            puts "  HA HA HA - You lose!\n\n"
        end
        print "\nPlay again (Y/N)? "; i = gets[0].downcase
        if i == "y"
            print "Same setup (Y/N)? "; i = gets[0].downcase
            if i == "y" 
                return PLAY_SAME
            else
                return PLAY_NEW
            end
        end
        return FINISH
    end
    def look_around()
        msg = 0
        @exits.each do |x|
            obj = @cave.room(x).obj
            if ((WUMPUS & obj) == WUMPUS) then msg += WUMPUS end
            if ((BAT & obj) == BAT) then msg += BAT end
            if ((PIT & obj) == PIT) then msg += PIT end
        end
        puts "\n"
        if ((msg & WUMPUS) == WUMPUS) 
            puts "- You smell something terrible nearby." 
        end
        if ((msg & PIT) == PIT) 
            puts "- You feel a cold wind blowing from a nearby cavern." 
        end
        if ((msg & BAT) == BAT)
            puts "- You hear a rustling." 
        end
    end
    def exits(e)
        @exits.each do |x|
            return true if e == x
        end
        return false
    end
    def input()
        print "\nShoot or Move (S/M)? "; r = gets[0].downcase
        case r
            when "o"
                puts @cave.room(@player).obj
            when "m"
                print "Where to? "; ex = gets.to_i
                return false if ex == nil or ex < 0 or ex > MAX_ROOMS + 1
                ex -= 1
                if exits(ex)
                    set_player(ex)
                else 
                    puts "\nArrggh! --- You cannot go there!\n\n"
                end
                return true
            when "s"
                  pathLen = 0
                loop do
                    print "\nNumber of rooms (1-5)? "
                      pathLen = gets.to_i
                      break if pathLen > 0 and pathLen < (A_PATH_LEN + 1)
                end
                cnt = 0
                loop do
                    print "Room #"; n = gets.to_i
                    if n != nil and n > 0 and n < (MAX_ROOMS + 1)
                        @path[cnt] = n - 1
                        if cnt < 2 or @path[cnt] != @path[cnt - 2]
                            cnt += 1
                        else
                            puts "\nArrows aren't that crooked! - " <<
                            "Please, try another room.\n\n"
                        end
                    end
                    break if cnt == pathLen
                end
                shoot(pathLen)
                return true
        end
        return false
    end
    def set_player(pos)
        @cave.room(@player).clear(PLAYER) if @player < MAX_ROOMS 
        return if hazards(pos) 
        @player = pos
        room = @cave.room(@player)
        room.set_object(PLAYER)
        for i in 0..(MAX_EXITS - 1)
            @exits[i] = room.get_exit(i)
        end
    end
    def hazards(pos)
        room = @cave.room(pos); obj = room.obj
        if ((WUMPUS & obj) == WUMPUS)
            puts "\n  ...OOPS! Bumped a Wumpus!\n\n"
            if wumpus(pos)
                puts "\n  TSK TSK TSK - Wumpus got you!\n"
                @gameOver = true; @playerWins = false
                return true
            end
        end
        if ((PIT & obj) == PIT)
            puts "\n  YYYYIIIIEEEE!!!! Fell in pit!\n"
            @gameOver = true; @playerWins = false
            return true
        end
        if ((BAT & obj) == BAT)
            puts "\n  ZAP -- Super bat snatch! Elsewhereville for you!\n\n"
            set_player(rand(MAX_ROOMS))
            return true;
        end
        return false
    end
    def wumpus(pos)
        if rand(100) < 75 
            room = @cave.room(@wumpus)
            room.clear(WUMPUS)
            @wumpus = room.get_exit(rand(MAX_EXITS))
            @cave.room(@wumpus).set_object(WUMPUS)
        end
        return pos == @wumpus
    end
    def init()
        puts "\n\n\n\nHUNT THE WUMPUS\n---------------\n"
        @cave.clear_all_rooms(); @gameOver = false; @arrows = MAX_ARROWS
        if @game_res == PLAY_NEW
            @saved[S_PLAYER] = rand(MAX_ROOMS); set_player(@saved[S_PLAYER])
            @saved[S_BAT1] = fill_room(BAT); @saved[S_BAT2] = fill_room(BAT)
            @saved[S_PIT1] = fill_room(PIT); @saved[S_PIT2] = fill_room(PIT)
            @wumpus = @saved[S_WUMPUS] = fill_room(WUMPUS)
        else
            set_player(@saved[S_PLAYER]); @wumpus = @saved[S_WUMPUS]
            @cave.room(@wumpus).set_object(WUMPUS)
            @cave.room(@saved[S_BAT1]).set_object(BAT)
            @cave.room(@saved[S_BAT2]).set_object(BAT)
            @cave.room(@saved[S_PIT1]).set_object(PIT)
            @cave.room(@saved[S_PIT2]).set_object(PIT)
        end
    end
    def fill_room(obj)
        i = 0; room = nil
        loop do
            i = rand(MAX_ROOMS)
            room = @cave.room(i)
            break if room.obj == NOTHING
         end
        room.set_object(obj)
        return i
    end
    def instructions()
        print "\n\nWelcome to `WUMPUS II'\n\nInstructions (Y/N)? "; 
        return if gets.chomp.downcase == 'n'
        puts "\n\nThis version has the same rules as `Hunt the Wumpus'.\n" <<
             "However, you now have a choice of caves to play in.\nSome " <<
             "caves are easier than others. All caves have 20\nrooms and 3 " <<
             "tunnels leading from one room to other rooms.\nThe caves " <<
             " are:\n\n  0  -  Dodecahedron     The rooms of this cave" <<
             " are on a\n        12-sided object, each side forming a " <<
             "pentagon.\n        The rooms are at the corners of the " <<
             "pentagons,\n        each room having tunnels leading to 3" << 
             "other rooms.\n\n  1  -  Mobius Strip     This cave" <<
             "is two rooms\n        wide and 10 rooms around (like a belt).\n" <<
             "        You will notice there is a half-twist\n        somewhere." <<
             "\n\n  2  -  String of Beads    Five beads in a circle.\n        " <<
             "Each bead is a diamond with a vertical\n        cross-bar. " <<
             "The right & left corners lead \n        to neighbouring beads. " <<
             "(This one is difficult\n        to play.)\n\n  Caves 0-2 are " <<
             "regular in the sense that each room\n  goes to three other " <<
             "rooms & tunnels allow two-\n  way traffic. Here are some " <<
             "`irregular' caves!\n\n  3 - Dendrite with Degeneracies.    " <<
             "\n  4 - One way lattice" << 
             "" <<
             "" <<
             "" <<
             "" <<
             "" <<
             "" <<
             "" <<
             "" <<
             ""
        gets   
    end
    def main_loop()
        while @game_res != FINISH
            init()
            while not @gameOver 
                look()
                begin
                    i = input()
                end while not i
            end
            @game_res = result();
        end
    end
end
@game = Game.new().main_loop()
 
