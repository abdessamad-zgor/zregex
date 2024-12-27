const std = @import("std");
const util = @import("./util.zig");
const zpattern = @import("./pattern.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;
const PatternState = zpattern.PatternState;
const SymbolValue = zpattern.SymbolValue;
const Symbols = zpattern.Symbols;
const PatternParseState = zpattern.PatternParseState;
const StackStateSymbol = zpattern.StackStateSymbol;
const RegexPatternParseError = zpattern.RegexPatternParseError;
const Pattern = zpattern.Pattern;

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

// describes the state of the FDA responsible for mathching the input against the regular expression
const InputState = enum { Qi, Qp, Qe, Qf };
// provides additional fields on top of th InsputState enum
const InputParseState = struct { current: InputState, patternIndex: usize, matchStartIndex: usize };

pub const Regex = struct {
    const Self = @This();

    allocator: Allocator,

    patterns: []Pattern,

    state: InputParseState,
    patternState: PatternParseState,

    fn stepOnSuccess(self: *Self, index: usize) void {
        if (self.state.patternIndex == 0) {
            self.state.matchStartIndex = index;
            self.state.current = InputState.Qp;
            self.state.patternIndex += 1;
        } else if (self.state.patternIndex == self.pattern.len - 1) {
            self.state.current = InputState.Qf;
            self.state.patternIndex = 0;
        } else {
            self.state.current = InputState.Qp;
            self.state.patternIndex += 1;
        }
    }

    fn stepOnFailure(self: *Self) void {
        if (self.state.current == InputState.Qp) {
            self.state.current = InputState.Qe;
        } else {
            self.state.current = InputState.Qi;
        }
        self.state.patternIndex = 0;
        self.state.matchStartIndex = 0;
    }

    pub fn init(allocator: Allocator) Regex {
        const inputState = InputParseState{ .current = InputState.Qi, .patternIndex = 0, .matchStartIndex = 0 };
        const patternState = PatternParseState.init(allocator);
        return Regex{ .allocator = allocator, .patterns = &[_]Pattern{}, .state = inputState, .patternState = patternState };
    }

    pub fn match(self: *Self, text: []const u8) !MatchResult {
        const result = MatchResult{ .matches = &[_]Match{} };
        _ = self;
        _ = text;
        return result;
    }

    ///Compile regular expression
    pub fn compile(self: *Self, pattern: []const u8) !void {
        var stack = std.ArrayList(StackStateSymbol).init(self.allocator);
        var patterns = std.ArrayList(Pattern).init(self.allocator);
        var states = std.ArrayList(PatternState).init(self.allocator);
        var lastValid: ?StackStateSymbol = null;
        try self.patternState.lex(pattern);
        try self.patternState.walk();
        try states.appendSlice(self.patternState.patternHistory);
        try states.append(self.patternState.current);

        if (self.patternState.current == .Valid or self.patternState.current == .ValidWithQualifier) {
            for (states.items, 0..) |state, i| {
                switch (state) {
                    .Valid => blk: {
                        lastValid = StackStateSymbol{ .symbol = self.patternState.symbols.?[i], .state = state };
                        break :blk;
                    },
                    .ValidWithQualifier => blk: {
                        const stateSymbol = self.patternState.symbols.?[i];
                        lastValid = null;
                        try stack.append(StackStateSymbol{ .symbol = stateSymbol, .state = state });
                        const builtPattern = try self.buildPattern(stack.items);
                        try patterns.append(builtPattern);
                        try stack.resize(0);
                        break :blk;
                    },
                    .QualifierStart => blk: {
                        const stateSymbol = self.patternState.symbols.?[i];
                        try stack.append(StackStateSymbol{ .symbol = stateSymbol, .state = state });
                        break :blk;
                    },
                    .ValidQualifier => blk: {
                        const stateSymbol = self.patternState.symbols.?[i];
                        try stack.append(StackStateSymbol{ .symbol = stateSymbol, .state = state });
                        break :blk;
                    },
                    else => {
                        const stateSymbol = self.patternState.symbols.?[i];
                        if (lastValid != null) {
                            try stack.append(StackStateSymbol{ .symbol = lastValid.?.symbol, .state = state });
                            const builtPattern = try self.buildPattern(stack.items);
                            try patterns.append(builtPattern);
                            try stack.resize(0);
                            lastValid = null;
                        } else {
                            try stack.append(StackStateSymbol{ .symbol = stateSymbol, .state = state });
                        }
                    },
                }
            }
        } else {
            return RegexPatternParseError.InvalidPattern;
        }

        self.patterns = patterns.items;
    }

    fn buildPattern(self: *Self, stack: []StackStateSymbol) !Pattern {
        _ = self;
        var current = Pattern{};
        // decide on the type of sub-expression
        current.type = switch (stack[0].state) {
            .GroupStart => .Group,
            // it's a literal because it's the only possible case
            .Valid => .Literal,
            .NeedsEscapeChar => .Literal,
            .RangeStart => .Range,
            .ValidOr => .Or,
            .StartsWith => .StartsWith,
            .EndsWith => .EndsWith,
            else => unreachable,
        };
        if (stack.len > 1) {
            for (stack[1 .. stack.len - 1], 0..) |entry, i| {
                _ = i;
                _ = entry;
                switch (current.type) {
                    .Group => blk: {
                        break :blk;
                    },
                    .Range => blk: {
                        break :blk;
                    },
                    .Or => blk: {
                        break :blk;
                    },
                    .StartsWith => blk: {
                        break :blk;
                    },
                    .EndsWith => blk: {
                        break :blk;
                    },
                    else => unreachable,
                }
            }
        }
        return current;
    }

    fn buildPatternTree(self: *Self, patterns: []Pattern) []Pattern {
        _ = self;
        _ = patterns;
        return &[_]Pattern{};
    }
};

test "Regex" {
    var regex = Regex.init(std.heap.page_allocator);
    try regex.compile(
        \\pa(mss\s)
    );
    try std.testing.expect(regex.patternState.current == PatternState.Valid);
}
