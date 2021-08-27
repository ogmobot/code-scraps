import random
class Symbol:
    def __init__(self, name):
        self.name = name
    def __eq__(self, other):
        if hasattr(other, "name"):
            return self.name == other.name
        else:
            return False
    def __hash__(self):
        return hash(self.name)
    def __repr__(self):
        return f"'{self.name}"

class Procedure:
    def __init__(self, arg_names, body):
        self.arg_names = arg_names
        self.body = body
    def __call__(self, *args):
        global g_current_context
        sub_env = {"parent_env": g_current_context}
        g_current_context = sub_env
        try:
            for index, name in enumerate(self.arg_names):
                sub_env[name] = args[index]
        except IndexError:
            raise TypeError(f"Wrong number of args: setting {arg_names} to {args}")
        retval = [evaluate(b, sub_env) for b in self.body].pop()
        g_current_context = g_current_context.get("parent_env", global_env)
        return retval

class Lisped_list:
    def __init__(self, iterable=[]):
        self._car = None
        self._cdr = None
        if iterable:
            self._car = iterable[0]
            self._cdr = Lisped_list(iterable[1:])
    def null(self):
        return (self._car == None) and (self._cdr == None)
    def car(self):
        return self._car
    def cdr(self):
        return self._cdr
    def append(self, val):
        if self.null():
            self._car = val
            self._cdr = Lisped_list()
        else:
            self._cdr.append(val)
    def prepend(self, val):
        result = Lisped_list()
        result._car = val
        result._cdr = self
        return result
    def copy(self):
        if self.null():
            return Lisped_list()
        else:
            result = Lisped_list()
            if hasattr(self._car, 'copy'):
                result._car = self._car.copy()
            else:
                result._car = self._car
            result._cdr = self._cdr.copy()
        return result
    def __len__(self):
        if self.null():
            return 0
        else:
            return 1 + len(self._cdr)
    def __getitem__(self, val):
        if isinstance(val, slice):
            start = val.start if val.start else 0
            stop = val.stop if val.stop else len(self)
            step = val.step if val.step else 1
            target = self
            index = 0
            for _ in range(start):
                if not target.null():
                    target = target.cdr()
                index += 1
            result = Lisped_list()
            while index < stop:
                if not target.null():
                    result.append(target.car())
                for _ in range(step):
                    if not target.null():
                        target = target.cdr()
                        index += 1
            return result
        else:
            if self.null():
                raise IndexError(f"index out of range.")
            elif val < 0:
                raise IndexError(f"index must be non-negative.")
            elif val == 0:
                return self.car()
            else:
                return self.cdr()[val - 1]
    def __iter__(self):
        tmp = self
        while not tmp.null():
            yield tmp.car()
            tmp = tmp.cdr()
    def __repr__(self):
        return "(" + " ".join([repr(val) for val in self]) + ")"

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
            in_string = not in_string
            token_acc += character
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
    if token_acc:
        tokens.append(token_acc)
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
    elif x.startswith("\"") and x.endswith("\"") and len(x) >= 2:
        return str(x[1:-1])
    else:
        return Symbol(x)

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
        print(f"Can't find {symbol} in environment!")
        return None

global_env = {
    Symbol("+"): (lambda a, b: a+b),
    Symbol("-"): (lambda a, b: a-b),
    Symbol("*"): (lambda a, b: a*b),
    Symbol("%"): (lambda a, b: a%b),
    Symbol("/"): (lambda a, b: a//b),
    Symbol("pow"): (lambda a, b: a**b),
    Symbol("floatdiv"): (lambda a, b: a/b),
    Symbol("<"): (lambda a, b: a < b),
    Symbol(">"): (lambda a, b: a > b),
    Symbol("string->int-list"): (lambda s: [int(c) for c in s]),
    Symbol("print"): (lambda *args: print(*args)),
    Symbol("car"): (lambda arg: arg.car()),
    Symbol("cdr"): (lambda arg: arg.cdr()),
    Symbol("="): (lambda a, b: a == b),
    Symbol("cons"): (lambda a, b: b.prepend(a)),
    Symbol("and"): (lambda *args: all(args)),
    Symbol("or"): (lambda *args: any(args)),
    Symbol("not"): (lambda *args: not any(args)),
    Symbol("contains"): (lambda a, b: (b in a)),
    Symbol("prompt"): (lambda *p: input(" ".join(p))),
    Symbol("lower"): (lambda s: s.lower()),
    Symbol("upper"): (lambda s: s.upper()),
    Symbol("index"): (lambda s, i: s[i]),
    Symbol("begin"): (lambda *args: None),
    Symbol("null?"): (lambda a: (a==None) or (len(a) == 0)),
    Symbol("->str"): (lambda a: str(a)),
    Symbol("->int"): (lambda a: int(a)),
    Symbol("->list"): (lambda a: Lisped_list(a)),
    Symbol("tuple"): (lambda *args: tuple(args)),
    Symbol("length"): (lambda a: len(a)),
    Symbol("slice"): (lambda s, i, j: s[i:j]),
    Symbol("True"): True,
    Symbol("False"): False,
    Symbol("None"): None,
    Symbol("file-contents"): (lambda f: read_from_file(f)),
    Symbol("str-replace"): (lambda s, a, b: s.replace(a, b)),
    # dict methods
    Symbol("dict-new"): (lambda: dict()),
    Symbol("dict-set"): (lambda a, key, val: a.update({key: val})),
    Symbol("dict-get"): (lambda a, key, default=None: a.get(key, default)),
    Symbol("dict-keys"): (lambda d: d.keys()),
    Symbol("dict-update"): (lambda a, b: {
        k:v for subdict in [a,b] for k,v in subdict.items()
    }),
    # set methods
    Symbol("set-new"): (lambda: set()),
    Symbol("set-add"): (lambda a, val: a.add(val)),
    # random methods
    Symbol("random-choice"): (lambda s: random.choice(s)),
}
g_current_context = global_env

def evaluate(x, env=g_current_context):
    #print(f"evaluating {x}")
    #print(f"{[tuple(pair) for pair in env.items() if pair[0]!='parent_env']}")
    if type(x) == Lisped_list:
        if x[0] == Symbol("if"):
            if evaluate(x[1], env):
                return evaluate(x[2], env)
            else:
                if len(x) > 3:
                    return evaluate(x[3], env)
        elif x[0] == Symbol("set!"):
            result = evaluate(x[2], env)
            if hasattr(result, "copy"):
                result = result.copy()
            env.update({x[1]: result})
            return result
        elif x[0] == Symbol("lambda"):
            return Procedure(x[1], x[2:])
        elif x[0] == Symbol("quote"):
            return x[1]
        elif x[0] == Symbol("eval"):
            return evaluate(evaluate(x[1],env),env)
        else:
            fn = evaluate(x[0], env)
            if hasattr(fn, "__call__"):
                return fn(*list(evaluate(s, env) for s in x.cdr()))
            else:
                print(f"No such function: {x[0]}")
    elif type(x) == Symbol:
        return find_in_env(x, env)
    else:
        return x

def eval_string(s):
    return evaluate(ast_from_tokens(tokenize(s)))

def repl():
    while True:
        try:
            print(repr(eval_string(input("> "))))
        except EOFError:
            break
        except KeyboardInterrupt:
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
(set! range (lambda (rangestart rangeend)
    (if (< rangestart rangeend)
        (cons rangestart (range (+ rangestart 1) rangeend))
        (quote ()))))
)""")
