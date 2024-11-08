const std = @import("std");
const regex = @import("regex/root.zig");
const debug = std.debug.print;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Hello ,Zig!\n", .{});

    try bw.flush();

    const text = "pa ei tirno pap pipo papapa sla3n bi k k moha ra papi, po";
    var rego = try regex.Regex.init(std.heap.page_allocator, "[a-z]a|i");
    //defer std.heap.page_allocator.free(rego);

    debug("patterns:\n", .{});
    for (rego.pattern) |item| {
        debug("\ttype: {}, symbols: {s}, qualifier: {c}\n", .{ item.type, item.symbols, item.qualifier });
    }
    //debug("issymbolsavailable: {} {}\n", .{ rego.pattern[1].symbols[0], rego.pattern[1].symbols[1] });

    const result = try rego.match(text);
    debug("matches: {}\n", .{result.matches.len});
    for (result.matches) |match| {
        debug("match: {s}, start: {}, length: {}\n", .{ match.text, match.start, match.length });
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit();
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const global = struct {
        fn testOne(input: []const u8) anyerror!void {
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(global.testOne, .{});
}
