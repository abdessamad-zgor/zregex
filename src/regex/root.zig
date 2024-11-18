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

const PatternState = enum { RangeStart, Valid, QualifierStart, ValidWithQualifier, ValidQualifier, Lit, Dash, ValidRange, GroupStart, NeedsEscapeChar, StartsWith, EndsWith, Error, Init };
const Symbols = enum { lit, char, open_p, close_p, open_b, close_b, ror, cap, ends_w, open_c, close_c, point, star, escape, plus, dash, comma, num };
const SymbolValue = struct { type: Symbols, value: ?u8, index: usize };

const InputState = enum { Qi, Qp, Qe, Qf };

const InputParseState = struct { current: InputState, patternIndex: usize, matchStartIndex: usize };
const PatternParseState = struct {
    const Self = @This();
    /// the stack represents the symbols the belong to a multi charachter construct (i.e groups, altenatives, qualifiers, ..etc)
    stack: []SymbolValue,
    /// meaningful characters inside a regex pattern
    symbols: ?[]SymbolValue,
    parsedSymbols: ?[]SymbolValue,
    current: PatternState,
    fn step(self: *Self, symbol: SymbolValue) void {
        self.current = self.patternStateOnSymbol(symbol.type);
    }

    fn patternStateOnSymbol(self: *Self, symbol: Symbols) PatternState {
        switch (self.current) {
            PatternState.Init => {
                return switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.open_p => PatternState.GroupStart,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    Symbols.cap => PatternState.StartsWith,
                    Symbols.point => PatternState.AnyChar,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    else => PatternState.Error,
                };
            },
            PatternState.StartsWith => {
                return switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.open_p => PatternState.GroupStart,
                    Symbols.escape => PatternState.NeedsEscapeChar,
                    Symbols.point => PatternState.Valid,
                    else => PatternState.Error,
                };
            },
            PatternState.Valid => {
                return switch (symbol) {
                    Symbols.star => PatternState.ValidWithQualifier,
                    Symbols.plus => PatternState.ValidWithQualifier,
                    Symbols.open_c => PatternState.QualifierStart,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.open_p => PatternState.GroupStart,
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.point => PatternState.Valid,
                    else => PatternState.Error,
                };
            },
            PatternState.ValidWithQualifier => {
                return switch (symbol) {
                    Symbols.lit => PatternState.Valid,
                    Symbols.num => PatternState.Valid,
                    Symbols.char => PatternState.Valid,
                    Symbols.open_b => PatternState.RangeStart,
                    Symbols.open_p => PatternState.GroupStart,
                    else => PatternState.Error,
                };
            },
            PatternState.QualifierStart => {
                return switch (symbol) {
                    Symbols.num => PatternState.ValidQualifier,
                    else => PatternState.Error,
                };
            },
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
        var patterns = std.ArrayList(Pattern).init(self.allocator);
        var lastValid: SymbolValue = undefined;
        self.lex(pattern);
        for (self.patternState.symbols.?) |symbolValue| {
            self.patternState.step(symbolValue);
            switch (self.patternState.current) {
                PatternState.Valid => {
                    lastValid = symbolValue;
                    try stack.append(symbolValue);
                },
                PatternState.QualifierStart => {},
                PatternState.NeedsEscapeChar => {},
                PatternState.StartsWith => {},
                PatternState.ValidWithQualifier => {},
                PatternState.RangeStart => {},
                PatternState.Lit => {},
                PatternState.Dash => {},
                PatternState.GroupStart => {},
                PatternState.StartsWith => {},
                PatternState.EndsWith => {},
                PatternState.Error => {},
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
