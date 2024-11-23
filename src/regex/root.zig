const std = @import("std");
const zstr = @import("../str/root.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;
// We're gonna use NFA's to parse the Regex expression
// then register the transition which will tell use how to Parse the regex expression
// it will give a better way to validate the regex expression and a more predictable way to generate a pattern structure

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

const PatternType = enum { Or, Literal, ExclusiveRange, Range, Group };

const Pattern = struct { symbols: ?[][]const u8, patterns: ?[]Pattern, qualifier: u8, type: PatternType, startIndex: usize };

const PatternParseResult = struct { type: PatternType, parsed: []usize };

const PatternState = enum { RangeStart, Or, Valid, ValidGroup, QualifierStart, ValidWithQualifier, ValidQualifier, ValidDashedRange, Lit, Dash, ValidRange, GroupStart, NeedsEscapeChar, StartsWith, EndsWith, Error, Init };
const Symbols = enum { lit, char, open_p, close_p, open_b, close_b, ror, cap, ends_w, open_c, close_c, point, star, escape, plus, dash, comma, num };
const SymbolValue = struct { type: Symbols, value: ?u8, index: usize };

const InputState = enum { Qi, Qp, Qe, Qf };

const InputParseState = struct { current: InputState, patternIndex: usize, matchStartIndex: usize };
const PatternParseState = struct {
    const Self = @This();
    /// used to keep track of pattern states that influence subsequent pattern states
    patternStateStack: ?[]PatternState,
    /// meaningful characters inside a regex pattern
    symbols: ?[]SymbolValue,
    parsedSymbols: ?[]SymbolValue,
    current: PatternState,

    pub fn patternStateOnSymbol(self: *Self, symbol: Symbols) void {
        self.current = switch (self.current) {
            PatternState.Init => {
                switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.open_p => PatternState.GroupStart,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    Symbols.cap => PatternState.StartsWith,
                    Symbols.point => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    else => PatternState.Error,
                }
            },
            PatternState.StartsWith => {
                switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.open_p => PatternState.GroupStart,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    Symbols.point => PatternState.Valid,
                    else => PatternState.Error,
                }
            },
            PatternState.Valid => {
                switch (symbol) {
                    Symbols.star => PatternState.ValidWithQualifier,
                    Symbols.plus => PatternState.ValidWithQualifier,
                    Symbols.open_c => PatternState.QualifierStart,
                    Symbols.open_b => blk: {
                        break :blk PatternState.RangeStart;
                    },
                    Symbols.open_p => blk: {
                        break :blk PatternState.GroupStart;
                    },
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.point => PatternState.Valid,
                    else => PatternState.Error,
                }
            },
            PatternState.ValidWithQualifier => {
                switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.open_p => PatternState.GroupStart,
                    else => PatternState.Error,
                }
            },
            PatternState.QualifierStart => {
                switch (symbol) {
                    Symbols.num => PatternState.ValidQualifier,
                    Symbols.comma => PatternState.ValidQualifier,
                    Symbols.close_c => PatternState.ValidWithQualifier,
                    else => PatternState.Error,
                }
            },
            PatternState.GroupStart => {
                switch (symbol) {
                    Symbols.num => blk: {
                        self.appendToStack(self.current);
                        break :blk PatternState.ValidGroup;
                    },
                    Symbols.char => blk: {
                        self.appendToStack(self.current);
                        break :blk PatternState.ValidGroup;
                    },
                    Symbols.lit => blk: {
                        self.appendToStack(self.current);
                        break :blk PatternState.ValidGroup;
                    },
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    else => PatternState.Error,
                }
            },
            PatternState.RangeStart => {
                switch (symbol) {
                    Symbols.num => PatternState.ValidRange,
                    Symbols.char => PatternState.ValidRange,
                    Symbols.lit => PatternState.ValidRange,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    else => PatternState.Error,
                }
            },
            PatternState.ValidRange => {
                switch (symbol) {
                    Symbols.num => PatternState.ValidRange,
                    Symbols.char => PatternState.ValidRange,
                    Symbols.lit => PatternState.ValidRange,
                    Symbols.dash => PatternState.Dash,
                    Symbols.close_b => PatternState.Valid,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    else => PatternState.Error,
                }
            },
            PatternState.Dash => {
                switch (symbol) {
                    Symbols.num => PatternState.ValidDashedRange,
                    Symbols.char => PatternState.ValidDashedRange,
                    Symbols.lit => PatternState.ValidDashedRange,
                    else => PatternState.Error,
                }
            },
            PatternState.ValidDashedRange => {
                switch (symbol) {
                    Symbols.num => PatternState.ValidRange,
                    Symbols.char => PatternState.ValidRange,
                    Symbols.lit => PatternState.ValidRange,
                    Symbols.close_b => PatternState.Valid,
                    else => PatternState.Error,
                }
            },
            PatternState.NeedsEscapeChar => blk: {
                const reducedStackState = self.reduceStack();
                if (reducedStackState == null) {
                    break :blk PatternState.Valid;
                } else {
                    break :blk switch (reducedStackState.?) {
                        PatternState.GroupStart => PatternState.ValidGroup,
                        PatternState.RangeStart => PatternState.ValidRange,
                        else => PatternState.Valid,
                    };
                }
            },
            PatternState.ValidGroup => {
                switch (symbol) {
                    Symbols.close_p => blk: {
                        self.emptyStack();
                        break :blk PatternState.Valid;
                    },
                }
            },
        };
    }

    fn appendToStack(self: *Self, state: PatternState) void {
        const stack = self.nestedSymbolStack.? orelse return &[_]PatternState{};
        const stackLen = stack.len;
        const patternStateStack: [stackLen + 1]PatternState = undefined;
        for (stack, 0..) |value, i| {
            patternStateStack[i] = value;
        }
        patternStateStack[stackLen - 1] = state;
        self.patternStateStack = patternStateStack;
    }

    fn emptyStack(self: *Self) void {
        self.patternStateStack = null;
    }

    fn popStack(self: *Self) ?PatternState {
        const stack = self.nestedSymbolStack.? orelse return &[_]PatternState{};
        const stackLen = stack.len;
        if (stackLen == 0) {
            return null;
        } else if (stackLen == 1) {
            const lastElement = stack[0];
            self.patternStateStack = null;
            return lastElement;
        } else {
            const lastElement = stack[stackLen - 1];
            const poppedStack = stack[0 .. stackLen - 1];
            self.patternStateStack = poppedStack;
            return lastElement;
        }
    }

    fn getLast(self: *Self) ?PatternState {
        const stack = self.nestedSymbolStack.? orelse return &[_]PatternState{};
        const stackLen = stack.len;
        if (stackLen == 0) {
            return null;
        } else {
            const lastElement = stack[stackLen - 1];
            return lastElement;
        }
    }

    fn reduceStack(self: *Self) ?PatternState {
        const stack = self.nestedSymbolStack.? orelse return &[_]PatternState{};
        const stackLen = stack.len;
        if (stackLen == 0) {
            return null;
        } else {
            // only the latest RangeStart and GroupRange are considered in this case
            var acc: ?PatternState = stack[stackLen - 1];
            for (0..stackLen - 1) |i| {
                acc = switch (stack[@intCast(stackLen - 2 - i)]) {
                    PatternState.RangeStart => blk: {
                        break :blk PatternState.RangeStart;
                    },
                    PatternState.GroupStart => blk: {
                        break :blk PatternState.GroupStart;
                    },
                    else => acc,
                };
            }
            return switch (acc) {
                PatternState.RangeStart => PatternState.RangeStart,
                PatternState.GroupStart => PatternState.GroupStart,
                else => null,
            };
        }
    }

    fn printPatternParseState(self: *Self) void {
        debug("pattern parse state: \n\tcurrent: {}\n", .{self.current});
        if (self.patternStateStack != null) {
            debug("\tstack: [", .{});
            for (self.patternStateStack.?, 0..) |patternState, i| {
                if (i == self.patternStateStack.?.len) {
                    debug("{}]\n", .{patternState});
                    break;
                }
                debug("{}, ", .{patternState});
            }
        } else {
            debug("\tstack: []\n", .{});
        }
    }
};

const RegexPatternParseError = error{ IncompleteGroupPattern, IncompleteOrPattern, IncompleteRangePattern, InvalidPattern, InvalidGroupPattern, WrongLowerAndUpperBound, OutOfMemory };

pub const Regex = struct {
    const Self = @This();

    allocator: Allocator,

    pattern: []Pattern,

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
        const inputState = InputParseState{ .current = InputState.Qi, .patternIndex = 0, .matchStartIndex = 0, .parsedStates = []InputState{} };
        const patternState = PatternParseState{ .stack = []PatternParseState{} };
        return Regex{ .allocator = allocator, .pattern = []Pattern{}, .state = inputState, .patternState = patternState };
    }

    pub fn match(self: *Self, text: []const u8) !MatchResult {
        const result = MatchResult{ .matches = &[_]Match{} };
        _ = self;
        _ = text;
        return result;
    }

    fn lex(self: *Self, pattern: []const u8) void {
        var patternSymbols = std.ArrayList(SymbolValue).init(self.allocator);
        for (pattern, 0..) |char, i| {
            switch (char) {
                '(' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_p, .index = i });
                },
                ')' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.close_p, .index = i });
                },
                ',' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.comma, .index = i });
                },
                '+' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.plus, .index = i });
                },
                '.' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.point, .index = i });
                },
                '\\' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.escape, .index = i });
                },
                '^' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.cap, .index = i });
                },
                '[' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_b, .index = i });
                },
                ']' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_b, .index = i });
                },
                '{' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_c, .index = i });
                },
                '}' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.close_c, .index = i });
                },
                '-' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.dash, .index = i });
                },
                '|' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.ror, .index = i });
                },
                '$' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.ends_w, .index = i });
                },
                '*' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.star, .index = i });
                },
                else => {
                    if (std.ascii.isDigit(char)) {
                        try patternSymbols.append(SymbolValue{ .type = Symbols.num, .value = char, .index = i });
                    } else if (std.ascii.isAlphabetic(char)) {
                        try patternSymbols.append(SymbolValue{ .type = Symbols.char, .value = char, .index = i });
                    } else {
                        try patternSymbols.append(SymbolValue{ .type = Symbols.lit, .value = char, .index = i });
                    }
                },
            }
        }
        self.patternState.symbols = patternSymbols.items;
    }

    pub fn parse(self: *Self, pattern: []const u8) !void {
        var stack = std.ArrayList(SymbolValue).init(self.allocator);
        const patterns = std.ArrayList(Pattern).init(self.allocator);
        var lastValid: ?SymbolValue = null;
        self.lex(pattern);
        for (self.patternState.symbols.?) |symbolValue| {
            self.patternState.patternStateOnSymbol(symbolValue.type);
            switch (self.patternState.current) {
                PatternState.Valid => {
                    lastValid = symbolValue;
                    try stack.append(symbolValue);
                },
                PatternState.ValidWithQualifier => {
                    lastValid = undefined;
                },
                PatternState.Error => {},
                else => {
                    // check if pattern was valid before current symbol
                    if (lastValid != null and lastValid.?.index == symbolValue.index - 1) {} else {
                        try stack.append(symbolValue);
                    }
                },
            }
        }
        self.patterns = patterns.items;
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
