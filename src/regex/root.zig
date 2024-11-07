const std = @import("std");
const zstr = @import("../str/root.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

const PatternType = enum { Or, Literal };

const Pattern = struct { symbols: []u8, qualifier: u8, type: PatternType, startIndex: usize };

const States = enum { Qi, Qf, Qp };

const ParseState = struct { current: States, patterIndex: usize, startIndex: usize };

const RegexPatternParseError = error{ IncompleteOrPattern, OutOfMemory };

pub const Regex = struct {
    const Self = @This();

    pattern: []Pattern,

    state: States,
    pattern_index: usize,
    start_index: usize,

    fn transition(self: *Self, char: u8, index: usize) void {
        //debug("patter_index: {}, state: {}\n", .{ self.pattern_index, self.state });
        const indexPattern = self.pattern[@intCast(self.pattern_index)];
        //debug("indexPatternSymbol: {s}\n", .{indexPattern.symbols});
        switch (indexPattern.type) {
            PatternType.Literal => {
                //debug("char: {c}, indexPatternSymbol: {s}\n", .{ char, indexPattern.symbols });
                if (char == indexPattern.symbols[0]) {
                    if (self.pattern_index == 0) {
                        self.start_index = index;
                        self.state = States.Qp;
                        self.pattern_index += 1;
                    } else if (self.pattern_index == self.pattern.len - 1) {
                        self.state = States.Qf;
                        self.pattern_index = 0;
                    } else {
                        self.state = States.Qp;
                        self.pattern_index += 1;
                    }
                } else {
                    self.start_index = 0;
                    self.state = States.Qi;
                    self.pattern_index = 0;
                }
            },
            PatternType.Or => {
                //debug("char: {c}, indexPatternSymbol: {s}\n", .{ char, indexPattern.symbols });
                if (char == indexPattern.symbols[0] or char == indexPattern.symbols[1]) {
                    if (self.pattern_index == 0) {
                        self.start_index = index;
                        self.state = States.Qp;
                        self.pattern_index += 1;
                    } else if (self.pattern_index == self.pattern.len - 1) {
                        self.state = States.Qf;
                        self.pattern_index = 0;
                    } else {
                        self.state = States.Qp;
                        self.pattern_index += 1;
                    }
                } else {
                    self.state = States.Qi;
                    self.pattern_index = 0;
                    self.start_index = 0;
                }
            },
        }
    }

    pub fn init(allocator: Allocator, pattern: []const u8) RegexPatternParseError!Regex {
        const pattern_buf = try allocator.alloc(u8, pattern.len);
        @memcpy(pattern_buf, pattern);
        var parsedIndexes = std.ArrayList(usize).init(allocator);
        var patternArr = std.ArrayList(Pattern).init(allocator);
        if (zstr.has(pattern, "|")) {
            const or_index = zstr.find(@constCast(pattern), @constCast("|"));
            if (or_index == pattern.len - 1 or or_index == 0) {
                return RegexPatternParseError.IncompleteOrPattern;
            }
            // some wierd shit with how memory works, I don't know enough
            const left_or = pattern_buf[1];
            const right_or = pattern_buf[3];
            const or_pattern = Pattern{ .type = PatternType.Or, .symbols = @constCast(&[_]u8{ left_or, right_or }), .qualifier = '1', .startIndex = @intCast(or_index - 1) };
            try parsedIndexes.appendSlice(&[_]usize{ @intCast(or_index - 1), @intCast(or_index), @intCast(or_index + 1) });
            try patternArr.append(or_pattern);
        }
        for (pattern, 0..) |char, i| {
            var isInParsedIndexs = false;
            for (parsedIndexes.items) |j| {
                if (j == i) {
                    isInParsedIndexs = j == i;
                    break;
                }
            }
            if (!isInParsedIndexs) {
                const literal_pattern = Pattern{ .type = PatternType.Literal, .symbols = @constCast(&[_]u8{char}), .qualifier = '1', .startIndex = i };
                try patternArr.append(literal_pattern);
            }
        }

        // order patterns - Bubble sort
        var swapped = false;

        for (0..patternArr.items.len) |i| {
            swapped = false;
            for (0..patternArr.items.len - i - 1) |j| {
                if (patternArr.items[j].startIndex > patternArr.items[j + 1].startIndex) {
                    const toBeSwapped = patternArr.items[j];
                    patternArr.items[j] = patternArr.items[j + 1];
                    patternArr.items[j + 1] = toBeSwapped;
                    swapped = true;
                }
            }

            // If no two elements were swapped, then break
            if (!swapped)
                break;
        }

        debug("patterns:\n", .{});
        for (patternArr.items) |item| {
            debug("\ttype: {}, symbols: {s}, qualifier: {c}\n", .{ item.type, item.symbols, item.qualifier });
        }
        return Regex{ .pattern = patternArr.items, .state = States.Qi, .pattern_index = 0, .start_index = 0 };
    }

    pub fn match(self: *Self, text: []const u8) !MatchResult {
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

test "Regex" {
    const regex = Regex.init("pa");

    const text = "papa ou t'es";

    const match_result = try regex.match(text);

    try std.testing.expect(match_result.matches.len == 2);

    for (match_result.matches) |match| {
        try std.testing.expect(match.length == 2);
    }
}
