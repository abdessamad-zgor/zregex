const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn Stack(comptime T: type) type {
    return struct {
        const Self = @This();
        stack: std.ArrayList(T),

        pub fn init(allocator: Allocator) Stack(T) {
            return Stack(T){ .stack = std.ArrayList(T).init(allocator) };
        }

        pub fn push(self: *Self, item: T) !void {
            try self.stack.append(item);
        }

        pub fn pop(self: *Self) T {
            return self.stack.pop();
        }

        pub fn isEmpty(self: *Self) bool {
            return self.stack.items.len == 0;
        }
    };
}

test "stack test" {
    var stack = Stack(u8).init(std.heap.page_allocator);

    try stack.push(1);
    const value = stack.pop();
    try std.testing.expect(value == 1);
}
