const std = @import("std");
const print = std.debug.print;
// The goal here is to implement a memory-efficient tournament structure.

const Node = struct {
    winner: enum { PLAYER_1, PLAYER_2, UNKNOWN },
    node_type: enum { MAIN_BRACKET, MAIN_LEAF, ELIM_BRACKET, ELIM_LEAF, ELIM_EXTRA },
    player_1: ?*Player,
    player_2: ?*Player,
    left_node: ?*Node,
    right_node: ?*Node,
    // Node pointers point to previous round (temporally).
    // For XTRA nodes, left_node should be main bracket
    // and right_node should be elim bracket
};

const Player = struct {
    id: u32,
    elo: i32,
};

const Return_Code = enum {
    SUCCESS,
    DEFER,
};

fn make_tree(depth: u32) *Node {
    // TODO: this whole function. Create a big stack or something to allocate memory with.
    // allocator should be passed as an argument.
    switch (depth) {
        1 => {
            // 2 contestants
        },
        else => {
            return null;
        },
    }
    var result = Node{
        .winner = .UNKNOWN,
        .node_type = .MAIN_BRACKET,
        .player_1 = null,
        .player_2 = null,
        .left_node = null,
        .right_node = null,
    };
    return &result;
}

fn first_player_wins(p1: *Player, p2: *Player) bool {
    // TODO: the elo thing
    return p1.*.elo >= p2.*.elo;
}

fn determine_winner(root: *Node) Return_Code {
    if (root.*.node_type != .LEAF) {
        // need to determine children's winners first
        const left_child = root.*.left_node.?;
        const right_child = root.*.left_node.?;
        var left_ret = determine_winner(left_child);
        var right_ret = determine_winner(right_child);
        if (left_ret == .DEFER) {
            // Try again, now that we've done the right child
            left_ret = determine_winner(left_child);
        }
        if (right_ret == .DEFER) {
            // Try again, now that we've done the left child
            right_ret = determine_winner(right_child);
        }
        if (left_ret == .DEFER or right_ret == .DEFER) {
            return .DEFER;
        }

        // Usually, player_1 is winner of left child.
        // For ELIM_EXTRA/ELIM_LEAF, player_1 is the player leaving main bracket
        // (left child points to main bracket.)
        root.*.player_1 = if ((left_child.*.winner == .PLAYER_1) and (root.*.node_type != .ELIM_BRACKET and root.*.node_type != .ELIM_LEAF)) {
            left_child.*.player_1;
        } else {
            left_child.*.player_2;
        };
        // Usually, player_2 is winner of right child.
        root.*.player_2 = if (right_child.*.winner == .PLAYER_1 and root.*.node_type != .ELIM_LEAF) {
            right_child.*.player_1;
        } else {
            right_child.*.player_2;
        };
    }
    // Now that players _1 and _2 have been populated, we can do *this* match.
    if (first_player_wins(root.*.player_1.?, root.*.player_2.?)) {
        root.*.winner = .PLAYER_1;
        return .SUCCESS;
    } else {
        root.*.winner = .PLAYER_2;
        return .SUCCESS;
    }
}

pub fn main() void {
    var grand_final = print("Hello, world!\n", .{});
}
