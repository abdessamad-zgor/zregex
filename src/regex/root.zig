const std = @import("std");
const zstr = @import("../str/root.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;

pub const Match = struct { text: []u8, start: usize, length: usize };

pub const MatchResult = struct {
    matches: []Match,
};

const PatternType = enum { Or, Literal, ExclusiveRange, Range, Group };

const Pattern = struct { symbols: ?[][]const u8, patterns: ?[]Pattern, qualifier: u8, type: PatternType, startIndex: usize };

const PatternParseResult = struct { type: PatternType, parsed: []usize };
const PatternState = enum { RangeStart, Or, QualifierStart, Valid, ValidGroup, ValidWithQualifier, ValidQualifier, ValidDashedRange, Lit, Dash, ValidRange, GroupStart, NeedsEscapeChar, StartsWith, EndsWith, Error, Init };
const Symbols = enum { lit, char, open_p, close_p, open_b, close_b, ror, cap, ends_w, open_c, close_c, point, star, escape, plus, dash, comma, num };
const SymbolValue = struct { type: Symbols, value: ?u8, index: usize };
const InputState = enum { Qi, Qp, Qe, Qf };
const InputParseState = struct { current: InputState, patternIndex: usize, matchStartIndex: usize };
const PatternParseState = struct {
    const Self = @This();
    /// used to keep track of pattern states that influence subsequent pattern states
    patternStateStack: ?[]PatternState,
    // accumulates the steps in pattern parsing
    patternHistory: []PatternState,
    /// meaningful characters inside a regex pattern
    symbols: ?[]SymbolValue,
    current: PatternState,
    allocator: Allocator,

    pub fn init(allocator: Allocator) PatternParseState {
        return PatternParseState{ .patternStateStack = &[_]PatternState{}, .symbols = null, .current = .Init, .allocator = allocator, .patternHistory = &[_]PatternState{} };
    }

    // TODO: exhaustive list of cases ( Or, EndsWith)
    pub fn patternStateOnSymbol(self: *Self, symbol: Symbols) !void {
        if (self.current != .Init) {
            try self.appendToHistory(self.current);
        }
        self.current = switch (self.current) {
            PatternState.Init => switch (symbol) {
                .lit => PatternState.Valid,
                .open_p => blk: {
                    try self.appendToStack(PatternState.GroupStart);
                    break :blk PatternState.GroupStart;
                },
                .open_b => blk: {
                    try self.appendToStack(PatternState.RangeStart);
                    break :blk PatternState.RangeStart;
                },
                .cap => blk: {
                    try self.appendToStack(PatternState.StartsWith);
                    break :blk PatternState.StartsWith;
                },
                .escape => PatternState.NeedsEscapeChar,
                .point => PatternState.Valid,
                .num => PatternState.Valid,
                .char => PatternState.Valid,
                else => PatternState.Error,
            },
            PatternState.StartsWith => switch (symbol) {
                .lit => PatternState.Valid,
                .num => PatternState.Valid,
                .char => PatternState.Valid,
                .open_b => blk: {
                    try self.appendToStack(PatternState.RangeStart);
                    break :blk PatternState.RangeStart;
                },
                .open_p => blk: {
                    try self.appendToStack(PatternState.GroupStart);
                    break :blk PatternState.GroupStart;
                },
                .escape => PatternState.NeedsEscapeChar,
                .point => PatternState.Valid,
                else => PatternState.Error,
            },
            PatternState.Valid => switch (symbol) {
                .star => PatternState.ValidWithQualifier,
                .plus => PatternState.ValidWithQualifier,
                .open_c => blk: {
                    try self.appendToStack(PatternState.QualifierStart);
                    break :blk PatternState.QualifierStart;
                },
                .open_b => blk: {
                    try self.appendToStack(PatternState.RangeStart);
                    break :blk PatternState.RangeStart;
                },
                .open_p => blk: {
                    try self.appendToStack(PatternState.GroupStart);
                    break :blk PatternState.GroupStart;
                },
                .lit => PatternState.Valid,
                .num => PatternState.Valid,
                .char => PatternState.Valid,
                .point => PatternState.Valid,
                else => PatternState.Error,
            },
            PatternState.ValidWithQualifier => switch (symbol) {
                .lit => PatternState.Valid,
                .num => PatternState.Valid,
                .char => PatternState.Valid,
                .open_b => PatternState.RangeStart,
                .open_p => PatternState.GroupStart,
                else => PatternState.Error,
            },
            PatternState.QualifierStart => switch (symbol) {
                .num => PatternState.ValidQualifier,
                .comma => PatternState.ValidQualifier,
                .close_c => PatternState.ValidWithQualifier,
                else => PatternState.Error,
            },
            PatternState.GroupStart => switch (symbol) {
                .num => PatternState.ValidGroup,
                .char => PatternState.ValidGroup,
                .lit => PatternState.ValidGroup,
                .escape => PatternState.NeedsEscapeChar,
                else => PatternState.Error,
            },
            PatternState.RangeStart => switch (symbol) {
                .num => PatternState.ValidRange,
                .char => PatternState.ValidRange,
                .lit => PatternState.ValidRange,
                .escape => PatternState.NeedsEscapeChar,
                else => PatternState.Error,
            },
            PatternState.ValidRange => switch (symbol) {
                .num => PatternState.ValidRange,
                .char => PatternState.ValidRange,
                .lit => PatternState.ValidRange,
                .dash => PatternState.Dash,
                .escape => PatternState.NeedsEscapeChar,
                .close_b => blk: {
                    const openState = self.findStackState(PatternState.RangeStart);
                    if (openState == 0) {
                        self.emptyStack(openState);
                        break :blk PatternState.Valid;
                    } else {
                        self.emptyStack(openState);
                        const nestedConstructState = self.reduceStack();
                        break :blk switch (nestedConstructState.?) {
                            PatternState.GroupStart => PatternState.ValidGroup,
                            PatternState.RangeStart => PatternState.ValidRange,
                            else => nestedConstructState.?,
                        };
                    }
                },
                else => PatternState.Error,
            },
            PatternState.Dash => switch (symbol) {
                .num => PatternState.ValidDashedRange,
                .char => PatternState.ValidDashedRange,
                .lit => PatternState.ValidDashedRange,
                else => PatternState.Error,
            },
            PatternState.ValidDashedRange => switch (symbol) {
                .num => PatternState.ValidRange,
                .char => PatternState.ValidRange,
                .lit => PatternState.ValidRange,
                .close_b => PatternState.Valid,
                else => PatternState.Error,
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
            PatternState.ValidGroup => switch (symbol) {
                .close_p => blk: {
                    const openState = self.findStackState(PatternState.GroupStart);
                    std.debug.assert(openState != null);
                    if (openState.? == 0) {
                        self.emptyStack(openState);
                        break :blk PatternState.Valid;
                    } else {
                        self.emptyStack(openState);
                        const nestedConstructState = self.reduceStack();
                        break :blk switch (nestedConstructState.?) {
                            PatternState.GroupStart => PatternState.ValidGroup,
                            PatternState.RangeStart => PatternState.ValidRange,
                            else => {
                                unreachable;
                            },
                        };
                    }
                },
                .ror => PatternState.Or,
                .escape => PatternState.NeedsEscapeChar,
                .num => PatternState.ValidGroup,
                .char => PatternState.ValidGroup,
                .lit => PatternState.ValidGroup,
                else => PatternState.Error,
            },
            else => PatternState.Error,
        };
    }

    fn appendToHistory(self: *Self, state: PatternState) !void {
        const stack = self.patternHistory;
        var patternHistoryBuffer = std.ArrayList(PatternState).init(self.allocator);
        const stackLen = stack.len;
        try patternHistoryBuffer.resize(stackLen + 1);
        const patternHistory: []PatternState = patternHistoryBuffer.items;
        for (stack, 0..) |value, i| {
            patternHistory[i] = value;
        }
        patternHistory[stackLen] = state;
        self.patternHistory = patternHistory;
    }

    fn appendToStack(self: *Self, state: PatternState) !void {
        const stack = self.patternStateStack orelse &[_]PatternState{};
        var patternStateBuffer = std.ArrayList(PatternState).init(self.allocator);
        const stackLen = stack.len;
        try patternStateBuffer.resize(stackLen + 1);
        const patternStateStack: []PatternState = patternStateBuffer.items;
        for (stack, 0..) |value, i| {
            patternStateStack[i] = value;
        }
        patternStateStack[stackLen] = state;
        self.patternStateStack = patternStateStack;
    }

    fn emptyStack(self: *Self, index: ?usize) void {
        if (index == null or index.? == 0) {
            self.patternStateStack = null;
        } else {
            self.patternStateStack = self.patternStateStack.?[0..index.?];
        }
    }

    fn popStack(self: *Self) ?PatternState {
        const stack = self.patternStateStack orelse &[_]PatternState{};
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
        const stack = self.patternStateStack orelse &[_]PatternState{};
        const stackLen = stack.len;
        if (stackLen == 0) {
            return null;
        } else {
            const lastElement = stack[stackLen - 1];
            return lastElement;
        }
    }

    fn reduceStack(self: *Self) ?PatternState {
        const stack = self.patternStateStack orelse &[_]PatternState{};
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
            return switch (acc.?) {
                PatternState.RangeStart => PatternState.RangeStart,
                PatternState.GroupStart => PatternState.GroupStart,
                else => null,
            };
        }
    }

    fn findStackState(self: *Self, state: PatternState) ?usize {
        var index: ?usize = null;
        for (self.patternStateStack.?, 0..) |stackState, i| {
            if (stackState == state) {
                index = i;
            }
        }
        return index;
    }

    pub fn printPatternParseState(self: *Self) void {
        debug("pattern parse state: \n\tcurrent: {}\n", .{self.current});
        // print patternStateStack
        if (self.patternStateStack != null and self.patternStateStack.?.len != 0) {
            for (self.patternStateStack.?, 0..) |patternState, i| {
                if (i == 0) {
                    debug("\tstack: [\n", .{});
                }
                if (i == self.patternStateStack.?.len - 1) {
                    debug("\t\\t\tt{}\n\t]\n", .{patternState});
                    break;
                }
                debug("{}, ", .{patternState});
            }
        } else {
            debug("\tstack: []\n", .{});
        }
        // print patternHistory
        if (self.patternHistory.len != 0) {
            for (self.patternHistory, 0..) |patternState, i| {
                if (i == 0) {
                    debug("\thistory: [\n", .{});
                }
                if (i == self.patternHistory.len - 1) {
                    debug("\t\t{}\n\t]\n", .{patternState});
                    break;
                }
                debug("\t\t{},\n", .{patternState});
            }
        } else {
            debug("\thistory: []\n", .{});
        }
        // print symbols
        if (self.symbols != null or self.symbols.?.len != 0) {
            for (self.symbols.?, 0..) |symbolVal, i| {
                if (i == 0) {
                    debug("\tsymbols: [", .{});
                }
                if (i == self.symbols.?.len - 1) {
                    debug("\n\t\t type: {}, value: {?}, index: {} \n\t]\n", .{ symbolVal.type, symbolVal.value, symbolVal.index });
                    break;
                }
                debug("\n\t\t type: {}, value: {?}, index: {} , ", .{ symbolVal.type, symbolVal.value, symbolVal.index });
            }
        } else {
            debug("\tsymbols: []\n", .{});
        }
    }
};

const RegexPatternParseError = error{ IncompleteGroupPattern, IncompleteOrPattern, IncompleteRangePattern, InvalidPattern, InvalidGroupPattern, WrongLowerAndUpperBound, OutOfMemory };

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

    fn lex(self: *Self, pattern: []const u8) !void {
        var patternSymbols = std.ArrayList(SymbolValue).init(self.allocator);
        for (pattern, 0..) |char, i| {
            switch (char) {
                '(' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_p, .index = i, .value = null });
                },
                ')' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.close_p, .index = i, .value = null });
                },
                ',' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.comma, .index = i, .value = null });
                },
                '+' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.plus, .index = i, .value = null });
                },
                '.' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.point, .index = i, .value = null });
                },
                92 => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.escape, .index = i, .value = null });
                },
                '^' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.cap, .index = i, .value = null });
                },
                '[' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_b, .index = i, .value = null });
                },
                ']' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_b, .index = i, .value = null });
                },
                '{' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.open_c, .index = i, .value = null });
                },
                '}' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.close_c, .index = i, .value = null });
                },
                '-' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.dash, .index = i, .value = null });
                },
                '|' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.ror, .index = i, .value = null });
                },
                '$' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.ends_w, .index = i, .value = null });
                },
                '*' => {
                    try patternSymbols.append(SymbolValue{ .type = Symbols.star, .index = i, .value = null });
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

    ///Compile regular expression
    pub fn compile(self: *Self, pattern: []const u8) !void {
        var stack = std.ArrayList(SymbolValue).init(self.allocator);
        var patterns = std.ArrayList(Pattern).init(self.allocator);
        var lastValid: ?SymbolValue = null;
        try self.lex(pattern);
        for (self.patternState.symbols.?) |symbolValue| {
            try self.patternState.patternStateOnSymbol(symbolValue.type);
            switch (self.patternState.current) {
                PatternState.Valid => {
                    lastValid = symbolValue;
                    try stack.append(symbolValue);
                },
                PatternState.ValidWithQualifier => {
                    lastValid = null;
                    try stack.append(symbolValue);
                    const parsedPattern = self.buildPattern(stack.items);
                    try patterns.append(@as(Pattern, parsedPattern));
                    try stack.resize(0);
                },
                PatternState.Error => {},
                else => {
                    // check if pattern was valid before current symbol
                    if (lastValid != null and lastValid.?.index == symbolValue.index - 1) {
                        const parsedPattern = self.buildPattern(stack.items);
                        try patterns.append(parsedPattern);
                        try stack.resize(0);
                        lastValid = null;
                    } else {
                        try stack.append(symbolValue);
                    }
                },
            }
        }
        self.patternState.printPatternParseState();
        self.patterns = patterns.items;
    }

    fn buildPattern(self: *Self, symbols: []SymbolValue) Pattern {
        _ = self;
        _ = symbols;
        return Pattern{ .type = PatternType.Or, .qualifier = '1', .startIndex = 0, .symbols = null, .patterns = null };
    }
};

test "Regex" {
    var regex = Regex.init(std.heap.page_allocator);
    try regex.compile(
        \\pa(mss\s)
    );
    try std.testing.expect(regex.patternState.current == PatternState.Valid);
}
