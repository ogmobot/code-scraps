import ctypes
import os
import random
import time

hailstone = lambda x: (x // 2) if x % 2 == 0 else ((3 * x) + 1)

# to compile maths_functions.so:
# gcc -fpic -shared maths_functions.c -o maths_functions.so
working_directory = os.path.dirname(os.path.realpath(__file__))
c_lib = ctypes.CDLL(f"{working_directory}/maths_functions.so")

print("Testing addition...")
assert c_lib.add(1, 1) == 2

print("Testing hailstones...")
for i in range(10):
    test_num = random.randint(1, 30)
    assert c_lib.hailstone(test_num) == hailstone(test_num)

def spin_wheels():
    steps = 0
    num = 670617279
    while True:
        steps += 1
        num = hailstone(num)
        if num == 1:
            break
        if num == 0:
            raise MathError
    return steps

print("Timing 'spin_wheels'x1000...")
start = time.time()
for i in range(1000):
    result = c_lib.spin_wheels()
end = time.time()
print(result)
print(f"{end-start} seconds.")
print("My turn...")
start = time.time()
for i in range(1000):
    result = spin_wheels()
end = time.time()
print(result)
print(f"{end-start} seconds.")

print("ROT-13ing a string.")
#c_lib.inplace_rot_13.restype  = ctypes.c_char_p

# strings are hard...
orig = "JEDI"
string_pointer = ctypes.c_char_p(orig.encode("UTF-8"))
c_lib.inplace_rot_13(string_pointer)
newstring = (string_pointer.value).decode("UTF-8")
print(f"{orig} => {newstring}")
