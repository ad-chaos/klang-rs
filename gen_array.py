from itertools import product

kwds = [
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Bool",
    "_Complex",
    "_Imaginary",
]


def get_sum(kw, a, b):
    return (a * ord(kw[0]) + b * ord(kw[-1]) + len(kw) - 3 * ord("a")) & 0x7F


hash_vals = set()
array = ["Identifier" for _ in range(128)]


def find_parameters() -> tuple[int, int, int]:
    for a, b in product(range(1, 23), repeat=2):
        hash_vals.clear()
        for kw in kwds:
            hash_vals.add(get_sum(kw, a, b))

        if len(hash_vals) == len(kwds):
            return a, b, max(hash_vals)


a, b, length = find_parameters()

for kw in kwds:
    array[get_sum(kw, a, b)] = kw.title()

print(end="[")
for enum in array:
    print(enum, end=", ")
print(end="]\n")
