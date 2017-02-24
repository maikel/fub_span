This is a span implementation as defined in the proposal P0122R1 [1]
with enchancements from P0454R0 [2] in form of user defined policies.

`span<T, N>` is a non-decaying view for one dimensional contiguous data.
It has zero overhead compared to native arrays. It is type-safe and comes with
optional bound-checking.

Opposed to Neil Macintosh's proposal[1] I believe that span should not come
with forceful bound checking. The iterators from Microsofts GSL library have
considerably more overhead in algorithms than simple pointers.
 
[1] - http://open-std.org/JTC1/SC22/WG21/docs/papers/2016/p0122r1.pdf <br/>
[2] - http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2016/p0454r0.html <br/>