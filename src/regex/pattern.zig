const std = @import("std");
const uarr = @import("../util/arr.zig");
const debug = std.debug.print;
const Allocator = std.mem.Allocator;

pub const PatternType = enum { Or, Literal, ExclusiveRange, Range, Group, StartsWith, EndsWith, Escape, Any, Empty };

pub const StackStateSymbol = struct { symbol: SymbolValue, state: PatternState };
pub const RegexPatternParseError = error{ IncompleteGroupPattern, IncompleteOrPattern, IncompleteRangePattern, InvalidPattern, InvalidGroupPattern, WrongLowerAndUpperBound, OutOfMemory };
pub const PatternSubExpressionsTag = enum { children, branches, symbols };
pub const PatternSubExpressions = union(PatternSubExpressionsTag) { children: []Pattern, branches: [][]Pattern, symbols: []u8 };
// describes a valid sub expression of a regular expression
pub const Pattern = struct { subexps: PatternSubExpressions, quantifier: Quantifier = .One, type: PatternType = .Empty, startIndex: usize = 0 };
pub const QuantifierTag = enum { One, Any, OneOrMore, MinOrMore, MaxOrLess, BetweenMinMax, Fixed };

pub const Quantifier = union(QuantifierTag) { One: void, Any: void, OneOrMore: void, MinOrMore: u32, MaxOrLess: u32, BetweenMinMax: struct { u32, u32 }, Fixed: u32 };

pub const PatternStateEnum = enum { RangeStart, Or, QuantifierStart, Valid, ValidGroup, ValidWithQuantifier, ValidQuantifier, ValidDashedRange, Dash, ValidRange, GroupStart, NeedsEscapeChar, StartsWith, EndsWith, Error, Init };
pub const PatternState = union(PatternStateEnum) {
    Init: void,
    RangeStart: void,
    GroupStart: void,
    Or: void,
    Valid: void,
    ValidGroup: void,
    ValidWithQuantifier: void,
    ValidQuantifier: void,
    ValidDashedRange: void,
    ValidRange: void,
    Dash: void,
    NeedsEscapeChar: void,
    StartsWith: void,
    EndsWith: void,
    Error: RegexPatternParseError,
};
pub const Symbols = enum { lit, char, open_p, close_p, open_b, close_b, ror, cap, ends_w, open_c, close_c, point, star, escape, plus, dash, comma, num };
pub const SymbolValue = struct { type: Symbols, value: ?u8, index: usize };

pub const PatternParseState = struct {
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
    pub fn parse(self: *Self) !void {
        var patterns = std.ArrayList(Pattern).init(self.allocator);
        var currentPattern: Pattern = Pattern{};
        if (self.current != .Init) {
            try self.appendToHistory(self.current);
        }

        for (self.symbols.?) |symbol| {
            self.current = switch (self.current) {
                .Init => switch (symbol) {
                    .lit => .Valid,
                    .open_p => blk: {
                        try self.appendToStack(.GroupStart);
                        currentPattern.type = .Group;
                        break :blk .GroupStart;
                    },
                    .open_b => blk: {
                        try self.appendToStack(.RangeStart);
                        currentPattern.type = .Range;
                        break :blk .RangeStart;
                    },
                    .cap => blk: {
                        try self.appendToStack(.StartsWith);
                        currentPattern.type = .StartsWith;
                        currentPattern.subexps = PatternSubExpressions{ .symbols = &[_]u8{} };
                        break :blk .StartsWith;
                    },
                    .escape => blk: {
                        currentPattern.type = .Escape;
                        break :blk .NeedsEscapeChar;
                    },
                    .point => blk: {
                        currentPattern.type = .Any;
                        try patterns.append(currentPattern);
                        currentPattern = Pattern{};
                        break :blk .Valid;
                    },
                    .num => blk: {
                        currentPattern.type = .Literal;
                        currentPattern.subexps = .{ .symbols = &[_]u8{symbol.value.?} };
                        try patterns.append(currentPattern);
                        break :blk .Valid;
                    },
                    .char => blk: {
                        currentPattern.type = .Literal;
                        currentPattern.subexps = .{ .symbols = &[_]u8{symbol.value.?} };
                        try patterns.append(currentPattern);
                        break :blk .Valid;
                    },
                    else => .Error,
                },
                .StartsWith => switch (symbol) {
                    .lit => blk: {
                        try patterns.append(currentPattern);
                        switch (currentPattern.subexps) {
                            .symbols => iblk: {
                                try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                break :iblk;
                            },
                            .children => iblk: {
                                const lastChild = currentPattern.subexps.children[@intCast(currentPattern.subexps.children.len - 1)];
                                if (lastChild.type == .Literal) {
                                    try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                } else {
                                    const newLiteralPattern = Pattern{ .type = .Literal, .subexps = .{ .symbols = &[_]u8{symbol.value.?} }, .startIndex = symbol.index };
                                    try uarr.appendToArray(Pattern, self.allocator, currentPattern.subexps.children, newLiteralPattern);
                                }
                                break :iblk;
                            },
                            else => unreachable,
                        }
                        break :blk .Valid;
                    },
                    .num => blk: {
                        switch (currentPattern.subexps) {
                            .symbols => iblk: {
                                try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                break :iblk;
                            },
                            .children => iblk: {
                                const lastChild = currentPattern.subexps.children[@intCast(currentPattern.subexps.children.len - 1)];
                                if (lastChild.type == .Literal) {
                                    try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                } else {
                                    const newLiteralPattern = Pattern{ .type = .Literal, .subexps = .{ .symbols = &[_]u8{symbol.value.?} }, .startIndex = symbol.index };
                                    try uarr.appendToArray(Pattern, self.allocator, currentPattern.subexps.children, newLiteralPattern);
                                }
                                break :iblk;
                            },
                            else => unreachable,
                        }
                        break :blk .Valid;
                    },
                    .char => blk: {
                        switch (currentPattern.subexps) {
                            .symbols => iblk: {
                                try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                break :iblk;
                            },
                            .children => iblk: {
                                const lastChild = currentPattern.subexps.children[@intCast(currentPattern.subexps.children.len - 1)];
                                if (lastChild.type == .Literal) {
                                    try uarr.appendToArray(u8, self.allocator, currentPattern.subexps.symbols, symbol.value.?);
                                } else {
                                    const newLiteralPattern = Pattern{ .type = .Literal, .subexps = .{ .symbols = &[_]u8{symbol.value.?} }, .startIndex = symbol.index };
                                    try uarr.appendToArray(Pattern, self.allocator, currentPattern.subexps.children, newLiteralPattern);
                                }
                                break :iblk;
                            },
                            else => unreachable,
                        }
                        break :blk .Valid;
                    },
                    .open_b => blk: {
                        try self.appendToStack(.RangeStart);
                        try patterns.append(currentPattern);
                        currentPattern = Pattern{};
                        currentPattern.type = .Range;
                        currentPattern.subexps = .{ .symbols = &[_]u8{} };
                        break :blk .RangeStart;
                    },
                    .open_p => blk: {
                        try self.appendToStack(.GroupStart);
                        currentPattern = Pattern{};
                        currentPattern.type = .Group;
                        currentPattern.subexps = .{ .children = &[_]Pattern{Pattern{}} };
                        break :blk .GroupStart;
                    },
                    .escape => blk: {
                        currentPattern = Pattern{};
                        currentPattern.type = .Escape;
                        currentPattern.subexps = .{ .symbols = &[_]u8{} };
                        break :blk .NeedsEscapeChar;
                    },
                    .point => blk: {
                        break :blk .Any;
                    },
                    else => .Error,
                },
                .Valid => switch (symbol) {
                    .star => .ValidWithQuantifier,
                    .plus => .ValidWithQuantifier,
                    .open_c => blk: {
                        try self.appendToStack(.QuantifierStart);
                        break :blk .QuantifierStart;
                    },
                    .open_b => blk: {
                        try self.appendToStack(.RangeStart);
                        break :blk .RangeStart;
                    },
                    .open_p => blk: {
                        try self.appendToStack(.GroupStart);
                        break :blk .GroupStart;
                    },
                    .lit => .Valid,
                    .num => .Valid,
                    .char => .Valid,
                    .point => .Valid,
                    .ror => blk: {
                        try self.appendToStack(self.patternHistory[0 .. self.patternHistory.len - 1]);
                        break :blk .Or;
                    },
                    else => .Error,
                },
                .ValidWithQuantifier => switch (symbol) {
                    .lit => .Valid,
                    .num => .Valid,
                    .char => .Valid,
                    .open_b => .RangeStart,
                    .open_p => .GroupStart,
                    else => .Error,
                },
                .QuantifierStart => switch (symbol) {
                    .num => .ValidQuantifier,
                    .comma => .ValidQuantifier,
                    .close_c => .ValidWithQuantifier,
                    else => .Error,
                },
                .GroupStart => switch (symbol) {
                    .num => .ValidGroup,
                    .char => .ValidGroup,
                    .lit => .ValidGroup,
                    .escape => .NeedsEscapeChar,
                    else => .Error,
                },
                .RangeStart => switch (symbol) {
                    .num => .ValidRange,
                    .char => .ValidRange,
                    .lit => .ValidRange,
                    .escape => .NeedsEscapeChar,
                    else => .Error,
                },
                .ValidRange => switch (symbol) {
                    .num => .ValidRange,
                    .char => .ValidRange,
                    .lit => .ValidRange,
                    .dash => .Dash,
                    .escape => .NeedsEscapeChar,
                    .close_b => blk: {
                        const openState = self.findStackState(.RangeStart);
                        if (openState == 0) {
                            self.emptyStack(openState);
                            break :blk .Valid;
                        } else {
                            self.emptyStack(openState);
                            const nestedConstructState = self.getParentState();
                            break :blk nestedConstructState.?;
                        }
                    },
                    else => .Error,
                },
                .Dash => switch (symbol) {
                    .num => .ValidDashedRange,
                    .char => .ValidDashedRange,
                    .lit => .ValidDashedRange,
                    else => .Error,
                },
                .ValidDashedRange => switch (symbol) {
                    .num => .ValidRange,
                    .char => .ValidRange,
                    .lit => .ValidRange,
                    .close_b => .Valid,
                    else => .Error,
                },
                .NeedsEscapeChar => blk: {
                    const reducedStackState = self.getParentState();
                    debug("reducedState: {?}\n", .{reducedStackState});
                    if (reducedStackState == null) {
                        break :blk .Valid;
                    } else {
                        break :blk reducedStackState.?;
                    }
                },
                .ValidGroup => switch (symbol) {
                    .close_p => blk: {
                        const openState = self.findStackState(.GroupStart);
                        std.debug.assert(openState != null);
                        if (openState.? == 0) {
                            self.emptyStack(openState);
                            break :blk .Valid;
                        } else {
                            self.emptyStack(openState);
                            const reducedStackState = self.getParentState();
                            if (reducedStackState == null) {
                                break :blk .Valid;
                            } else {
                                break :blk reducedStackState.?;
                            }
                        }
                    },
                    .ror => .Or,
                    .escape => .NeedsEscapeChar,
                    .num => .ValidGroup,
                    .char => .ValidGroup,
                    .lit => .ValidGroup,
                    else => .Error,
                },
                .Or => switch (symbol) {
                    .close_p => blk: {
                        const nestedPatternState = self.getParentState();
                        if (nestedPatternState == null) {
                            break :blk .Valid;
                        } else {
                            break :blk nestedPatternState.?;
                        }
                    },
                    .num => blk: {
                        const nestedPatternState = self.getParentState();
                        if (nestedPatternState == null) {
                            break :blk .Valid;
                        } else {
                            break :blk nestedPatternState.?;
                        }
                    },
                    .char => blk: {
                        const nestedPatternState = self.getParentState();
                        if (nestedPatternState == null) {
                            break :blk .Valid;
                        } else {
                            break :blk nestedPatternState.?;
                        }
                    },
                    .lit => blk: {
                        const nestedPatternState = self.getParentState();
                        if (nestedPatternState == null) {
                            break :blk .Valid;
                        } else {
                            break :blk nestedPatternState.?;
                        }
                    },
                    .escape => .NeedsEscapeChar,
                    else => .Error,
                },
                .ValidQuantifier => switch (symbol) {
                    .close_c => blk: {
                        const nestedPatternState = self.getParentState();
                        if (nestedPatternState == null) {
                            break :blk .Valid;
                        } else {
                            break :blk nestedPatternState.?;
                        }
                    },
                    .num => .ValidQuantifier,
                    else => .Error,
                },
                .EndsWith => .Error,
                .Error => .Error,
            };
        }
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
        self.symbols = patternSymbols.items;
    }

    fn appendToHistory(self: *Self, state: PatternState) !void {
        const stack = self.patternHistory;
        self.patternHistory = try uarr.appendToArray(PatternState, self.allocator, stack, state);
    }

    fn appendToStack(self: *Self, state: PatternState) !void {
        const stack = self.patternStateStack orelse &[_]PatternState{};
        self.patternStateStack = try uarr.appendToArray(PatternState, self.allocator, stack, state);
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

    fn getParentState(self: *Self) ?PatternState {
        const stack = self.patternStateStack orelse &[_]PatternState{};
        const stackLen = stack.len;
        if (stackLen == 0) {
            return null;
        } else {
            // only the latest RangeStart and GroupRange are considered in this case
            var acc: ?PatternState = null;
            reduce_loop: for (0..stackLen - 1) |i| {
                acc = switch (stack[@intCast(stackLen - 1 - i)]) {
                    PatternState.RangeStart => .ValidRange,
                    PatternState.GroupStart => .ValidGroup,
                    else => null,
                };
                if (acc != null) {
                    break :reduce_loop;
                }
            }
            return acc;
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
                    debug("\t\t\tt{}\n\t]\n", .{patternState});
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
