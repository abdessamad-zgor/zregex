const std = @import("std");
const zstr = @import("../str/root.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

const PatternType = enum { Or, Literal, Range };

const Pattern = struct { symbols: []const u8, qualifier: u8, type: PatternType, startIndex: usize };

const States = enum { Qi, Qf, Qp };

const ParseState = struct { current: States, patternIndex: usize, startIndex: usize };

const RegexPatternParseError = error{ IncompleteOrPattern, IncompleteRangePattern, InvalidPattern, WrongLowerAndUpperBound, OutOfMemory };

pub const Regex = struct {
    const Self = @This();

    pattern: []Pattern,

    state: States,
    pattern_index: usize,
    start_index: usize,

    fn step_on_success(self: *Self, index: usize) void {
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
    }

    fn step_on_failure(self: *Self) void {
        self.state = States.Qi;
        self.pattern_index = 0;
        self.start_index = 0;
    }

    fn transition(self: *Self, char: u8, index: usize) void {
        //debug("patter_index: {}, state: {}\n", .{ self.pattern_index, self.state });
        const indexPattern = self.pattern[@intCast(self.pattern_index)];
        //debug("indexPatternSymbol: {s}\n", .{indexPattern.symbols});
        switch (indexPattern.type) {
            PatternType.Literal => {
                //debug("char: {c}, indexPatternSymbol: {s}\n", .{ char, indexPattern.symbols });
                if (char == indexPattern.symbols[0]) {
                    self.step_on_success(index);
                } else {
                    self.step_on_failure();
                }
            },
            PatternType.Or => {
                //debug("char: {c}, indexPatternSymbol: {s}\n", .{ char, indexPattern.symbols });
                if (char == indexPattern.symbols[0] or char == indexPattern.symbols[1]) {
                    self.step_on_success(index);
                } else {
                    self.step_on_failure();
                }
            },
            PatternType.Range => {
                var isInRange = false;
                for (indexPattern.symbols) |range_char| {
                    isInRange = isInRange or range_char == char;
                }
                if (isInRange) {
                    self.step_on_success(index);
                } else {
                    self.step_on_failure();
                }
            },
        }
    }

    pub fn init(allocator: Allocator, pattern: []const u8) RegexPatternParseError!Regex {
        var parsedIndexes = std.ArrayList(usize).init(allocator);
        var patternArr = std.ArrayList(Pattern).init(allocator);
        // Parse or pattern
        if (zstr.has(pattern, "|")) {
            const or_index = zstr.find(pattern, "|");
            var or_symbols = try allocator.alloc(u8, 2);
            or_symbols[0] = pattern[@intCast(or_index - 1)];
            or_symbols[1] = pattern[@intCast(or_index + 1)];
            if (or_index == pattern.len - 1 or or_index == 0) {
                return RegexPatternParseError.IncompleteOrPattern;
            }
            // some wierd shit with how memory works, I don't know enough
            const or_pattern = Pattern{ .type = PatternType.Or, .symbols = or_symbols, .qualifier = '1', .startIndex = @intCast(or_index - 1) };
            try parsedIndexes.appendSlice(&[_]usize{ @intCast(or_index - 1), @intCast(or_index), @intCast(or_index + 1) });
            try patternArr.append(or_pattern);
        }
        // Parse range pattern
        if (zstr.has(pattern, "[")) {
            if (!zstr.has(pattern, "]")) {
                return RegexPatternParseError.IncompleteRangePattern;
            }
            const left_range = zstr.find(pattern, "[");
            const right_range = zstr.find(pattern, "]");
            if (left_range > right_range or right_range - left_range == 1) {
                return RegexPatternParseError.InvalidPattern;
            }
            try parsedIndexes.appendSlice(&[_]usize{ @intCast(left_range), @intCast(right_range) });
            const range_str = pattern[@intCast(left_range + 1)..@intCast(right_range)];
            debug("range string: {s}\n", .{range_str});
            const dashes = try zstr.findAll(allocator, range_str, "-");
            if (dashes.len == 0) {
                for (@intCast(left_range)..@intCast(right_range + 1)) |range_index| {
                    try parsedIndexes.append(range_index);
                }
                var symbols = try allocator.alloc(u8, range_str.len);
                for (range_str, 0..) |range_literal, range_index| {
                    symbols[range_index] = range_literal;
                }
                const range_pattern = Pattern{ .type = PatternType.Range, .symbols = symbols, .qualifier = '1', .startIndex = @intCast(left_range + 1) };
                try patternArr.append(range_pattern);
            } else {
                var symbols = std.ArrayList(u8).init(allocator);
                debug("dashes: ", .{});
                for (dashes, 0..) |dash, i| {
                    if (i == @as(usize, @intCast(dashes.len - 1))) {
                        debug(" {c}\n", .{range_str[@intCast(dash)]});
                        break;
                    }
                    debug(" {c}", .{range_str[@intCast(dash)]});
                }
                for (dashes, 0..) |dash, i| {
                    if (dash == 0 or dash == range_str.len - 1) {
                        return RegexPatternParseError.InvalidPattern;
                    } else if (dashes.len > 1) {
                        if (i != dashes.len - 1 and dashes[i] == dashes[i + 1] + 1) {
                            return RegexPatternParseError.InvalidPattern;
                        } else if (i != dashes.len - 1 and dashes[i] == dashes[i + 1] + 2) {
                            return RegexPatternParseError.InvalidPattern;
                        }
                    }
                    const lowerBound = range_str[@intCast(dash - 1)];
                    debug("lowerBound: {c}\n", .{lowerBound});
                    const upperBound = range_str[@intCast(dash + 1)];
                    debug("upperBound: {c}\n", .{upperBound});
                    if (lowerBound > upperBound) {
                        return RegexPatternParseError.WrongLowerAndUpperBound;
                    } else {
                        for (lowerBound..@as(u8, @intCast(upperBound + 1))) |char_in_range| {
                            try symbols.append(@intCast(char_in_range));
                        }
                        try parsedIndexes.appendSlice(&[_]usize{ @as(usize, @intCast(left_range)) + dash, @as(usize, @intCast(left_range)) + dash + 1, (@as(usize, @intCast(left_range)) + dash + 2) });
                        //debug("parsed range literals: {c} {c} {c}\n", .{ range_str[@intCast(dash - 1)], range_str[@intCast(dash)], range_str[@intCast(dash + 1)] });
                    }
                }
                debug("parsed indexes: ", .{});
                for (parsedIndexes.items, 0..) |parsedIndex, i| {
                    if (i == parsedIndexes.items.len - 1) {
                        debug(" {c}\n", .{pattern[parsedIndex]});
                    }
                    debug(" {c}", .{pattern[parsedIndex]});
                }

                for (range_str, 0..) |range_char, i| {
                    //if (@intCast(left_range + 1 + i))
                    var isInParsedIndexs = false;
                    for (parsedIndexes.items) |parsedIndex| {
                        if (parsedIndex == @as(usize, @intCast(left_range + 1)) + i) {
                            isInParsedIndexs = parsedIndex == @as(usize, @intCast(left_range)) + i + 1;
                            break;
                        }
                    }
                    if (!isInParsedIndexs) {
                        try symbols.append(range_char);
                        try parsedIndexes.append(i);
                    }
                }
                const range_pattern = Pattern{ .type = PatternType.Range, .symbols = symbols.items, .qualifier = '1', .startIndex = @intCast(left_range + 1) };
                try patternArr.append(range_pattern);
            }
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
                var literal_symbol = try allocator.alloc(u8, 1);
                literal_symbol[0] = char;
                const literal_pattern = Pattern{ .type = PatternType.Literal, .symbols = literal_symbol, .qualifier = '1', .startIndex = i };
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
