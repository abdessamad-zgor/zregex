const std = @import("std");
const testing = std.testing;
const Allocator = std.mem.Allocator;

pub fn concat(allocator: Allocator, src: []const u8, dst: []const u8) ![]u8 {
    const result = try std.mem.concat(allocator, u8, &[_][]u8{ @constCast(src), @constCast(dst) });
    return result;
}

pub fn has(haystack: []const u8, needle: []const u8) bool {
    var result = false;
    var needle_index: usize = 0;
    for (haystack, 0..) |c, i| {
        if (needle_index == needle.len) {
            break;
        }
        for (i..haystack.len) |j| {
            _ = j;
            if (needle[needle_index] == c) {
                result = true;
                needle_index += 1;
            } else {
                result = false;
                needle_index = 0;
            }
            break;
        }
    }
    return result;
}

pub fn toLowerCaseAscii(allocator: Allocator, str: []const u8) ![]u8 {
    var result = try allocator.alloc(u8, str.len);
    for (str, 0..) |char, i| {
        result[i] = std.ascii.toLower(char);
    }
    return result;
}

pub fn find(haystack: []u8, needle: []u8) i8 {
    var result = false;
    var needle_index: usize = 0;
    var position: i8 = -1;

    for (haystack, 0..) |c, i| {
        if (needle_index == needle.len) {
            break;
        }
        for (i..haystack.len) |j| {
            _ = j;
            if (needle[needle_index] == c) {
                if (needle_index == 0) {
                    position = @intCast(i);
                }
                result = true;
                needle_index += 1;
            } else {
                position = -1;
                result = false;
                needle_index = 0;
            }
            break;
        }
    }
    return position;
}

pub fn equal(src: []const u8, dst: []const u8) bool {
    var result = true;
    for (src, 0..) |char, i| {
        if (char != dst[i]) {
            result = false;
            break;
        }
    }
    return result;
}

pub fn iequal(src: []const u8, dst: []const u8) bool {
    var result = true;
    for (src, 0..) |char, i| {
        if (std.ascii.toLower(char) != std.ascii.toLower(dst[i])) {
            result = false;
            break;
        }
    }
    return result;
}

test "concat" {
    const allocator = std.heap.page_allocator;
    const src = "Hello, ";
    const dst = "World!\n";

    const concat_result = try concat(allocator, src, dst);

    try testing.expect(std.mem.eql(u8, "Hello, World!\n", concat_result));
}

test "has" {
    const haystack = "how to do string manipulation in Zig?\n";
    const needle = "Zig";

    const has_result = has(@constCast(haystack), @constCast(needle));

    try testing.expect(has_result);
}

test "find" {
    const haystack = "how to do string manipulation in Zig?\n";
    const needle = "Zig";

    const find_result = find(@constCast(haystack), @constCast(needle));

    try testing.expect(find_result == 33);
}

test "equal" {
    const text = "Zig";

    const equal_result = equal(text, "Zig");

    try testing.expect(equal_result);
}

test "iequal" {
    const text = "ZiG";

    const equal_result = iequal(text, "ZIg");

    try testing.expect(equal_result);
}

test "lower case" {
    const allocator = std.heap.page_allocator;
    const str = "Zig";

    const lower_result = try toLowerCaseAscii(allocator, str);

    try testing.expect(equal(lower_result, "zig"));
}
