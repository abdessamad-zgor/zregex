const std = @import("std");
const regex = @import("regex/regex.zig");

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Hello ,Zig!\n", .{});

    try bw.flush();

    const text = "paterno sla3nbi moha";
    var rego = regex.Regex.init("pa");
    const result = try rego.matchConst(text);
    for (result.matches) |match| {
        std.debug.print("match: {s}, start: {}, length: {}", .{ match.text, match.start, match.length });
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
