const std = @import("std");
const print = std.debug.print;
// The goal here is to implement a memory-efficient tournament structure.

// Build three binary trees:
// - Winner's bracket [W]
// - Minor Loser's bracket (two contestants from loser's bracket) [Lmin]
// - Major Loser's bracket (one contestant from winner's and one from loser's) [Lmaj]
// Grow W bracket until it has enough leaves for all contestants.
// Populate remaining leaves with byes.
// Grow Lmin to one less level than W.
// Grow Lmaj to the same level as W.
// Populate Lmaj "player 2" leaves with byes.
// W(lev, match) populates W(lev-1, match<<1) and Lmaj(lev, match) [P1]
// Lmaj(lev, match) populates Lmin(lev-1, match<<1)
// Lmin(lev, match) populates Lmaj(lev, match) [P2]

// [0] is not used.
// L0       *       [1]   depth=1
// L1   *       *   [2-3] depth=2
// L2  * *     * *  [4-7] depth=3
// L3 ** **   ** ** [8-15] depth=4

const bracket_type = enum {
    WINNER, LOSER_MINOR, LOSER_MAJOR
};

const Tournament = struct {
    allocator: *std.mem.Allocator,
    depth: u6,
    b_W: []Node, // Winner's bracket
    b_Lmin: []Node, // Loser's bracket (minor)
    b_Lmaj: []Node, // Loser's bracket (major)
    const Self = @This();

    fn init(allocator: *std.mem.Allocator, depth: u6) Self {
        // Allocates memory for the tournament.
        // "depth: u6" limits this to a 64-level tournament; i.e. 2^64 contestants.
        // This is about 9 orders of magnitude higher than the current population
        // of the planet, so should be fine for the foreseeable future.
        var result = Self{
            .allocator = allocator,
            .depth = depth,
            .b_W = allocator.alloc(Node, @as(u64, 1) << depth) catch &[_]Node{},
            .b_Lmin = allocator.alloc(Node, @as(u64, 1) << (depth - 1)) catch &[_]Node{},
            .b_Lmaj = allocator.alloc(Node, @as(u64, 1) << depth) catch &[_]Node{},
        };
        return result;
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.b_W);
        self.allocator.free(self.b_Lmin);
        self.allocator.free(self.b_Lmaj);
        return;
    }
    fn populate(self: *Self, players: []*Player) void {
        // Populates the leaves of winner's bracket with the given players,
        // and populates the leaves of Lmaj.player_2 with byes.
        const layer_size = @as(u64, 1) << (self.depth - 1);
        // layer_size is also the starting index
        var index = layer_size;
        // Make Winner's bracket null
        while (index < layer_size * 2) {
            self.b_W[index].player_1 = null;
            self.b_W[index].player_2 = null;
            index += 1;
        }
        // Make Loser's bracket (major) null
        index = layer_size;
        while (index < layer_size * 2) {
            self.b_Lmaj[index].player_2 = null;
            index += 1;
        }
        // Add players
        // TODO Chris' algorithm
        for (players) |player, i| {
            if (i % 2 == 0) {
                self.b_W[layer_size + (i >> 1)].player_1 = player;
            } else {
                self.b_W[layer_size + (i >> 1)].player_2 = player;
            }
        }
        return;
    }
    fn run_match(self: *Self, which_bracket: bracket_type, level: u6, index: u64) void {
        // Runs the match of the given bracket
        // E.g. t.run_match(.WINNER, 2, 0)
        // runs the first match of the winner's semifinals
        print("Running match {}, {}, {}.\n", .{ which_bracket, level, index });
        const bracket = switch (which_bracket) {
            .WINNER => self.b_W,
            .LOSER_MINOR => self.b_Lmin,
            .LOSER_MAJOR => self.b_Lmaj,
        };
        var match = &bracket[(@as(u64, 1) << level) + index];
        var winning_player: ?*Player = null;
        var losing_player: ?*Player = null;
        if (first_player_wins(match.*.player_1, match.*.player_2)) {
            match.*.winner = .PLAYER_1;
            winning_player = match.*.player_1;
            losing_player = match.*.player_2;
            print("Player 1 wins.\n", .{});
        } else {
            match.*.winner = .PLAYER_2;
            winning_player = match.*.player_2;
            losing_player = match.*.player_1;
            print("Player 2 wins.\n", .{});
        }
        // Populate next brackets
        switch (which_bracket) {
            .WINNER => {
                // winner progresses to next level of winner's bracket
                // loser goes to same level of loser's bracket (major)
                var next_match = self.b_W[(@as(u64, 1) << (level - 1)) + (index >> 1)];
                if (index % 2 == 0) {
                    next_match.player_1 = winning_player;
                } else {
                    next_match.player_2 = winning_player;
                }
                self.b_Lmaj[(@as(u64, 1) << level) + index].player_1 = losing_player;
            },
            .LOSER_MINOR => {
                // winner progresses to same level of loser's bracket (major)
                // loser drops
                self.b_Lmaj[(@as(u64, 1) << level) + index].player_2 = winning_player;
            },
            .LOSER_MAJOR => {
                // winner progresses to next level of loser's bracket (minor)
                // loser drops
                var next_match = self.b_Lmaj[(@as(u64, 1) << level) + index];
                if (index % 2 == 0) {
                    next_match.player_1 = winning_player;
                } else {
                    next_match.player_2 = winning_player;
                }
            },
        }
    }
    fn print_tree(self: *Self) void {
        var level: u6 = undefined;
        print("Winner's bracket\n", .{});
        level = 0;
        while (level < self.depth) {
            for (self.b_W[(@as(u64, 1) << level)..(@as(u64, 1) << (level + 1))]) |match, i| {
                print_match(match);
            }
            print("\n", .{});
            level += 1;
        }
        print("Loser's bracket (major)\n", .{});
        level = 0;
        while (level < self.depth) {
            for (self.b_Lmaj[(@as(u64, 1) << level)..(@as(u64, 1) << (level + 1))]) |match, i| {
                print_match(match);
            }
            print("\n", .{});
            level += 1;
        }
        print("Loser's bracket (minor)\n", .{});
        level = 0;
        while (level < self.depth - 1) {
            for (self.b_Lmin[(@as(u64, 1) << level)..(@as(u64, 1) << (level + 1))]) |match, i| {
                print_match(match);
            }
            print("\n", .{});
            level += 1;
        }
    }
};

const Node = struct {
    // If player_x is null, this is a bye.
    winner: enum { PLAYER_1, PLAYER_2, UNKNOWN } = .UNKNOWN,
    player_1: ?*Player = null,
    player_2: ?*Player = null,
    const Self = @This();
};

const Player = struct {
    id: u32,
    elo: i32,
};

fn first_player_wins(p1: ?*Player, p2: ?*Player) bool {
    if (p2 == null)
        return true;
    if (p1 == null)
        return false;
    // TODO: the elo thing
    return p1.?.*.elo >= p2.?.*.elo;
}

fn print_match(match: Node) void {
    print("Node({} ", .{match.winner});
    //if (match.player_1) |p1| {
    //print("{}", .{p1});
    //} else {
    //print("null ", .{});
    //}
    //if (match.player_2) |p2| {
    //print("{}", .{p2});
    //} else {
    //print("null ", .{});
    //}
    print(")", .{});
}

pub fn main() void {
    const allocator = std.heap.page_allocator;
    var tmp = Tournament.init(allocator, 3);
    defer tmp.deinit();
    var alice = Player{ .id = 0, .elo = 0 };
    var bob = Player{ .id = 1, .elo = 1 };
    var pointers = [_]*Player{ &alice, &bob };
    tmp.populate(pointers[0..]);
    tmp.print_tree();
    tmp.run_match(.WINNER, 2, 0);
    tmp.print_tree();
    var grand_final = print("Hello, world!\n", .{});
}
