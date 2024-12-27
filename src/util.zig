const std = @import("std");
const Allocator = std.mem.Allocator;

pub fn appendToArray(comptime T: type, allocator: Allocator, arr: []T, elem: T) ![]T {
    var tBuffer = std.ArrayList(T).init(allocator);
    defer tBuffer.deinit();
    const arrLen = arr.len;
    try tBuffer.resize(arrLen + 1);
    const tArr: []T = tBuffer.items;
    for (arr, 0..) |value, i| {
        tArr[i] = value;
    }
    tArr[arrLen] = elem;
    return tArr;
}
