iv
==

A terminal-based vector algebra calculator written in OCaml. It is inspired by a subset of Iversonian notation but with traditional mathematical operator precedence.

Examples
--------

Scalar Arithemtic

iv> 1 + 2 - 1.5
1.5

iv> 4 * 2 / 3 
2.66666666667

iv> 1 + 2 ^ 3
9.

Scalar-Vector Arithemtic

iv> 1 + 2 3 4
3. 4. 5.

iv> 2 ^ 1 2 3
2. 4. 8.

iv> 1 2 3 * 2
2. 4. 6.

iv> 2 ^ 2 * 1 2 3 / 2 + 1
3. 5. 7.

Vector-Vector Arithmetic

iv> 1 2 3 + 1 2 3
2. 4. 6.

iv> 1 2 3 + 1 2 3 * 1 2 3 
2. 6. 12.

iv> 1 2.5 3 * 1 2 3 / 2 2 2
2. 0.4 0.222222222222
