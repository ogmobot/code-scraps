import random
class Lisped_list:
    def __init__(self, iterable=[]):
        self._head = []
        for val in iterable:
            self.append(val)
    def car(self):
        if self._head:
            return self._head[0]
        else:
            return None
    def cdr(self):
        result = Lisped_list()
        if self._head:
            result._head = self._head[1]
        return result
    def append(self, val):
        ptr = self._head
        while ptr:
            ptr = ptr[1]
        ptr.extend([val, []])
    def prepend(self, val):
        result = Lisped_list()
        result._head = [val, self._head]
        return result
    def copy(self):
        result = Lisped_list()
        ptr = self._head
        while ptr:
            if hasattr(ptr[0], 'copy'):
                result.append(ptr[0].copy())
            else:
                result.append(ptr[0])
            ptr = ptr[1]
        return result
    def __len__(self):
        result = 0
        ptr = self._head
        while ptr:
            result += 1
            ptr = ptr[1]
        return result
    def __getitem__(self, val):
        if isinstance(val, slice):
            start = val.start if val.start else 0
            stop = val.stop if val.stop else len(self)
            step = val.step if val.step else 1
            ptr = self._head
            index = 0
            for _ in range(start):
                if ptr:
                    ptr = ptr[1]
                index += 1
            result = Lisped_list()
            while index < stop:
                if ptr:
                    result.append(ptr[0])
                for _ in range(step):
                    if ptr:
                        ptr = ptr[1]
                        index += 1
            return result
        else:
            ptr = self._head
            if not ptr:
                raise IndexError
            for _ in range(val):
                if ptr[1]:
                    ptr = ptr[1]
                else:
                    raise IndexError
            return ptr[0]
    def __iter__(self):
        ptr = self._head
        while ptr:
            yield ptr[0]
            ptr = ptr[1]
    def __str__(self):
        return "(" + " ".join([str(val) for val in self]) + ")"
    def __repr__(self):
        return str(self)

def tokenize(raw_data):
    token_acc = ""
    in_string = False
    in_comment = False
    tokens = []
    for character in raw_data:
        if character == ";" and (not in_string):
            in_comment = True
        elif character == "\n" and in_comment:
            in_comment = False
        elif in_comment:
            continue
        elif character == "(" and (not in_string):
            tokens.append("(")
        elif character == ")" and (not in_string):
            if token_acc:
                tokens.append(token_acc)
                token_acc = ""
            tokens.append(")")
        elif character == "\"":
            if in_string and token_acc == "":
                tokens.append("")
            in_string = not in_string
        elif character.isspace():
            if token_acc and (not in_string):
                tokens.append(token_acc)
                token_acc = ""
            elif in_string:
                token_acc += character
            else:
                pass
        else:
            token_acc += character
    return tokens

def ast_from_tokens(token_list):
    token = token_list.pop(0)
    if token == "(":
        ast = Lisped_list()
        try:
            while token_list[0] != ")":
                ast.append(ast_from_tokens(token_list))
        except IndexError:
            raise SyntaxError("Unmatched '('")
        token_list.pop(0)
        return ast
    elif token == ")":
        raise SyntaxError("Unmatched ')'")
    else:
        return atom(token)

def atom(x):
    if x.isdigit():
        return int(x)
    else:
        return x
    return x

def make_proc(arg_names, body):
    def result(*args):
        global g_current_context
        sub_env = {"parent_env": g_current_context}
        g_current_context = sub_env
        for index, name in enumerate(arg_names):
            sub_env[name] = args[index]
        retval = [evaluate(b, sub_env) for b in body][-1]
        g_current_context = g_current_context.get("parent_env", global_env)
        return retval
    return result

def read_from_file(filename):
    with open(filename, "r") as f:
        return f.read()

def find_in_env(symbol, env):
    # FIXME parameters with the same name in different functions
    # mess this up
    if symbol in env:
        #print(f"{symbol}={env[symbol]}")
        return env[symbol]
    elif "parent_env" in env:
        return find_in_env(symbol, env["parent_env"])
    else:
        return symbol

global_env = {
    "+": (lambda a, b: a+b),
    "-": (lambda a, b: a-b),
    "*": (lambda a, b: a*b),
    "%": (lambda a, b: a%b),
    "/": (lambda a, b: a//b),
    "pow": (lambda a, b: a**b),
    "floatdiv": (lambda a, b: a/b),
    "<": (lambda a, b: a < b),
    ">": (lambda a, b: a > b),
    "string->int-list": (lambda s: [int(c) for c in s]),
    "print": (lambda *args: print(*args)),
    "car": (lambda arg: arg.car()),
    "cdr": (lambda arg: arg.cdr()),
    "=": (lambda a, b: a == b),
    "cons": (lambda a, b: b.prepend(a)),
    "and": (lambda *args: all(args)),
    "or": (lambda *args: any(args)),
    "not": (lambda *args: not any(args)),
    "contains": (lambda a, b: (b in a)),
    "prompt": (lambda *p: input(" ".join(p))),
    "lower": (lambda s: s.lower()),
    "upper": (lambda s: s.upper()),
    "index": (lambda s, i: s[i]),
    "begin": (lambda *args: None),
    "null?": (lambda a: (a==None) or (len(a) == 0)),
    "->str": (lambda a: str(a)),
    "->int": (lambda a: int(a)),
    "->list": (lambda a: Lisped_list(a)),
    "tuple": (lambda *args: tuple(args)),
    "length": (lambda a: len(a)),
    "slice": (lambda s, i, j: s[i:j]),
    "True": True,
    "False": False,
    "None": None,
    "file-contents": (lambda f: read_from_file(f)),
    # dict methods
    "dict-new": (lambda: dict()),
    "dict-set": (lambda a, key, val: a.update({key: val})),
    "dict-get": (lambda a, key, default=None: a.get(key, default)),
    "dict-keys": (lambda d: d.keys()),
    "dict-update": (lambda a, b: {
        k:v for subdict in [a,b] for k,v in subdict.items()
    }),
    # set methods
    "set-new": (lambda: set()),
    "set-add": (lambda a, val: a.add(val)),
    # random methods
    "random-choice": (lambda s: random.choice(s)),
}
g_current_context = global_env

def evaluate(x, env=g_current_context):
    #print(f"evaluating {x}")
    #print(f"{[tuple(pair) for pair in env.items() if pair[0]!='parent_env']}")
    if type(x) == Lisped_list:
        if x[0] == "if":
            if evaluate(x[1], env):
                return evaluate(x[2], env)
            else:
                if len(x) > 3:
                    return evaluate(x[3], env)
        elif x.car() == "set!":
            result = evaluate(x[2], env)
            if hasattr(result, "copy"):
                result = result.copy()
            env.update({x[1]: result})
            return result
        elif x.car() == "lambda":
            return make_proc(x[1], x[2:])
        elif x.car() == "quote":
            return x[1]
        elif x.car() == "eval":
            return evaluate(evaluate(x[1],env),env)
        else:
            fn = evaluate(x.car(), env)
            if fn:
                return fn(*list(evaluate(s, env) for s in x.cdr()))
            else:
                print(f"No such function: {x[0]}")
    else:
        return find_in_env(x, env)

def eval_string(s):
    return evaluate(ast_from_tokens(tokenize(s)))

def repl():
    while True:
        try:
            print(eval_string(input("> ")))
        except EOFError:
            break

# Standard library
eval_string("""
(begin
(set! append (lambda (a b)
    (if (null? a)
        b
        (cons (car a) (append (cdr a) b)))))

(set! reverse (lambda (revseq)
    (foldl cons revseq (quote ()))))

(set! map (lambda (mapfn mapseq)
    (if (null? mapseq)
        (quote ())
        (cons (mapfn (car mapseq)) (map mapfn (cdr mapseq))))))

(set! foldl (lambda (foldfn foldseq foldacc)
    (if (null? foldseq)
        foldacc
        (foldl foldfn (cdr foldseq) (foldfn (car foldseq) foldacc)))))

(set! filter (lambda (filterfn fseq)
    (if (null? fseq)
        (quote ())
        (if (filterfn (car fseq))
            (cons (car fseq) (filter filterfn (cdr fseq)))
            (filter filterfn (cdr fseq))))))

(set! countif (lambda (countiffn countifseq)
    (length (filter countiffn countifseq))))

(set! apply (lambda (apply-fn apply-args)
    (eval (cons apply-fn apply-args))))

(set! qsort (lambda (qcmp qseq)
    (if (null? qseq)
        (quote ())
        (append
            (qsort qcmp (filter (lambda (x) (qcmp x (car qseq))) (cdr qseq)))
            (cons
                (car qseq)
                (qsort qcmp (filter (lambda (x) (not (qcmp x (car qseq)))) (cdr qseq))))))))

(set! delimit (lambda (seq delimiter)
    (if (= (length seq) 1)
        (car seq)
        (+ (+ (car seq) delimiter) (delimit (cdr seq) delimiter)))))

(set! split-line (lambda (line delimiter-list test-index)
    (if (= test-index (length line))
        (cons line (quote ()))
        (if (apply or (map (lambda (x) (= x (index line test-index))) delimiter-list))
            (cons
                (slice line 0 test-index)
                (split-line
                    (slice line (+ test-index 1) (length line))
                    delimiter-list
                    0))
            (split-line line delimiter-list (+ test-index 1))))))
)""")
