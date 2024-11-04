const std = @import("std");

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

pub const Regex = struct {
    const Self = @This();
    const States = enum { Qi, Qf, Qp };

    pattern: []const u8,
    state: States,
    pattern_index: i128,
    start_index: usize,

    fn transition(self: *Self, char: u8, index: usize) void {
        if (char == self.pattern[@intCast(self.pattern_index + 1)]) {
            if (self.pattern.len - 1 == self.pattern_index) {
                self.pattern_index = @as(i128, self.pattern.len) - 1;
                self.state = States.Qf;
            } else {
                if (self.pattern_index == -1) {
                    self.start_index = index;
                }
                self.pattern_index = self.pattern_index + 1;
                self.state = States.Qp;
            }
        } else {
            self.pattern_index = -1;
            self.state = States.Qi;
        }
    }

    pub fn init(pattern: []const u8) Regex {
        return Regex{ .pattern = pattern, .state = States.Qi, .pattern_index = -1, .start_index = 0 };
    }

    pub fn match(self: *Self, text: []u8) !MatchResult {
        var result = MatchResult{ .matches = &[_]Match{} };
        const allocator = std.heap.page_allocator;
        var matches = std.ArrayList(Match).init(allocator);
        for (text, 0..) |char, index| {
            self.transition(char, index);
            if (self.state == States.Qf) {
                try matches.append(Match{ .text = @constCast(text[self.start_index .. index + 1]), .start = self.start_index, .length = index - self.start_index + 1 });
            }
        }
        result.matches = matches.items;
        return result;
    }

    pub fn matchConst(self: *Self, text: []const u8) !MatchResult {
        var result = MatchResult{ .matches = &[_]Match{} };
        const allocator = std.heap.page_allocator;
        var matches = std.ArrayList(Match).init(allocator);
        for (text, 0..) |char, index| {
            self.transition(char, index);
            if (self.state == States.Qf) {
                try matches.append(Match{ .text = @constCast(text[self.start_index .. index + 1]), .start = self.start_index, .length = index - self.start_index + 1 });
            }
        }
        result.matches = matches.items;
        return result;
    }
};
