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

// Defining this as u64 means we can avoid @as(u64, 1) later
const ONE: u64 = 1;

const bracket_type = enum {
    WINNER, LOSER_MINOR, LOSER_MAJOR, GRAND_FINAL
};

const Tournament = struct {
    allocator: *std.mem.Allocator,
    depth: u6,
    b_W: []Node, // Winner's bracket; b_W[0] is grand final
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
            .b_W = allocator.alloc(Node, ONE << depth) catch &[_]Node{},
            .b_Lmin = allocator.alloc(Node, ONE << (depth - 1)) catch &[_]Node{},
            .b_Lmaj = allocator.alloc(Node, ONE << depth) catch &[_]Node{},
        };
        return result;
    }
    fn deinit(self: *Self) void {
        self.allocator.free(self.b_W);
        self.allocator.free(self.b_Lmin);
        self.allocator.free(self.b_Lmaj);
        return;
    }
    fn populate(self: *Self, players: []*const Player) void {
        // Populates the leaves of winner's bracket with the given players,
        // and populates all other nodes with byes.
        var index: usize = 0;
        while (index < (ONE << self.depth)) {
            self.b_W[index] = .{};
            index += 1;
        }
        index = 0;
        while (index < (ONE << self.depth)) {
            self.b_Lmaj[index] = .{};
            index += 1;
        }
        index = 0;
        while (index < (ONE << (self.depth - 1))) {
            self.b_Lmin[index] = .{};
            index += 1;
        }
        // Add players
        const layer_size = ONE << (self.depth - 1);
        // layer_size is also the starting index
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
            .GRAND_FINAL => undefined,
        };
        var match: *Node = undefined;
        if (which_bracket == .GRAND_FINAL) {
            match = &self.b_W[0];
        } else {
            match = &bracket[(ONE << level) + index];
        }
        var winning_player: ?*const Player = undefined;
        var losing_player: ?*const Player = undefined;
        if (first_player_wins(match.*.player_1, match.*.player_2)) {
            match.*.winner = .PLAYER_1;
            winning_player = match.*.player_1;
            losing_player = match.*.player_2;
            //print("Player 1 wins.\n", .{});
        } else {
            match.*.winner = .PLAYER_2;
            winning_player = match.*.player_2;
            losing_player = match.*.player_1;
            //print("Player 2 wins.\n", .{});
        }
        // Populate next brackets
        switch (which_bracket) {
            .WINNER => {
                // winner progresses to next level of winner's bracket
                // loser goes to same level of loser's bracket (major)
                var next_match: *Node = undefined;
                if (level == 0) {
                    next_match = &self.b_W[0];
                    next_match.*.player_1 = winning_player;
                } else {
                    next_match = &self.b_W[(ONE << (level - 1)) + (index >> 1)];
                    if (index % 2 == 0) {
                        next_match.*.player_1 = winning_player;
                    } else {
                        next_match.*.player_2 = winning_player;
                    }
                }
                self.b_Lmaj[(ONE << level) + index].player_1 = losing_player;
            },
            .LOSER_MINOR => {
                // winner progresses to same level of loser's bracket (major)
                // loser drops
                self.b_Lmaj[(ONE << level) + index].player_2 = winning_player;
            },
            .LOSER_MAJOR => {
                // winner progresses to next level of loser's bracket (minor)
                // loser drops
                var next_match: *Node = undefined;
                if (level == 0) {
                    next_match = &self.b_W[0];
                    next_match.*.player_2 = winning_player;
                } else {
                    next_match = &self.b_Lmin[(ONE << (level - 1)) + (index >> 1)];
                    if (index % 2 == 0) {
                        next_match.*.player_1 = winning_player;
                    } else {
                        next_match.*.player_2 = winning_player;
                    }
                }
            },
            .GRAND_FINAL => {},
        }
    }
    fn run_all_matches(self: *Self) void {
        var level = self.depth - 1;
        var index: usize = undefined;
        var layer_size: usize = undefined;
        while (true) {
            layer_size = ONE << level;
            if (level < self.depth - 1) {
                // Run Loser's (Minor)
                index = 0;
                while (index < layer_size) {
                    self.run_match(.LOSER_MINOR, level, index);
                    index += 1;
                }
            }
            // Run Winner's
            index = 0;
            while (index < layer_size) {
                self.run_match(.WINNER, level, index);
                index += 1;
            }
            // Run Loser's (Major)
            index = 0;
            while (index < layer_size) {
                self.run_match(.LOSER_MAJOR, level, index);
                index += 1;
            }
            if (level == 0) {
                break;
            } else {
                level -= 1;
            }
        }
        self.run_match(.GRAND_FINAL, 0, 0);
    }
    fn print_tree(self: *Self) void {
        var level: u6 = undefined;
        print("Winner's bracket\n", .{});
        level = 0;
        while (level < self.depth) {
            for (self.b_W[(ONE << level)..(ONE << (level + 1))]) |match, i| {
                print_match(match);
            }
            print("\n", .{});
            level += 1;
        }
        print("Loser's bracket\n", .{});
        level = 0;
        while (level < self.depth) {
            for (self.b_Lmaj[(ONE << level)..(ONE << (level + 1))]) |match, i| {
                print_match(match);
            }
            print("\n", .{});
            if (level < self.depth - 1) {
                for (self.b_Lmin[(ONE << level)..(ONE << (level + 1))]) |match, i| {
                    print_match(match);
                }
                print("\n", .{});
            }
            level += 1;
        }
        print("Grand finals\n", .{});
        print_match(self.b_W[0]);
        print("\n", .{});
    }
};

const Node = struct {
    // If player_x is null, this is a bye.
    winner: enum { PLAYER_1, PLAYER_2, UNKNOWN } = .UNKNOWN,
    player_1: ?*const Player = null,
    player_2: ?*const Player = null,
    const Self = @This();
};

const Player = struct {
    name: []const u8,
    id: u32,
    elo: i32,
};

fn first_player_wins(p1: ?*const Player, p2: ?*const Player) bool {
    if (p2 == null)
        return true;
    if (p1 == null)
        return false;
    // TODO: the elo thing
    return p1.?.*.elo >= p2.?.*.elo;
}

fn print_match(match: Node) void {
    // Don't call this on uninitialised node!
    print("(", .{});
    if (match.player_1) |p1| {
        print("{}", .{p1.name});
    } else {
        print("-bye-", .{});
    }
    switch (match.winner) {
        .PLAYER_1 => print("* v  ", .{}),
        .PLAYER_2 => print("  v *", .{}),
        else => print("  v  ", .{}),
    }
    if (match.player_2) |p2| {
        print("{}", .{p2.name});
    } else {
        print("-bye-", .{});
    }
    print(") ", .{});
}

pub fn main() void {
    const allocator = std.heap.page_allocator;
    var tmp = Tournament.init(allocator, 2);
    defer tmp.deinit();
    const alice = Player{ .id = 0, .elo = 0, .name = "Alice" };
    const bob = Player{ .id = 1, .elo = 1, .name = "Bob" };
    const charlie = Player{ .id = 2, .elo = 3, .name = "Charlie" };
    const dave = Player{ .id = 3, .elo = 2, .name = "Dave" };
    var pointers = [_]*const Player{ &alice, &bob, &charlie, &dave };
    tmp.populate(pointers[0..]);
    tmp.run_all_matches();
    tmp.print_tree();
}
