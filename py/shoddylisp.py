import random
DEBUG = False
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
        return self.name

class Env(dict):
    def __init__(self, outer=None):
        self.outer = outer

class Procedure:
    def __init__(self, arg_names, body, env):
        self.arg_names = arg_names
        self.body = body
        self.env = env
    def __call__(self, args, parent_env):
        env = Env(self.env)
        env.update(self.args_env(args))
        #env.outer = parent_env
        retval = [evaluate(b, env) for b in self.body].pop()
        # TODO tail recursion
        return retval
    def args_env(self, args):
        arg_ptr = Lisped_list(args)
        new_env = {}
        for index, symbol in enumerate(self.arg_names):
            if symbol.name.startswith("&"):
                new_env[Symbol(symbol.name[1:])] = arg_ptr
                break
            if arg_ptr.null():
                raise TypeError(f"Not enough args: needed {self.arg_names}, got {args}")
            new_env[symbol] = arg_ptr.car()
            arg_ptr = arg_ptr.cdr()
        else:
            if not arg_ptr.null():
                raise TypeError(f"Too many args: needed {self.arg_names}, got {args}")
        return new_env

class Macro(Procedure):
    pass

class Lisped_list:
    @staticmethod
    def cons(car, cdr):
        result = Lisped_list()
        result._car = car
        result._cdr = cdr
        return result
    def __init__(self, iterable=[]):
        self._car = None
        self._cdr = None
        if iterable:
            iterable = iter(iterable)
            try:
                self._car = next(iterable)
                self._cdr = Lisped_list(iterable)
            except StopIteration:
                self._car = None
                self._cdr = None
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
            if hasattr(self._cdr, 'copy'):
                result._cdr = self._cdr.copy()
            else:
                result._cdr = self._cdr
        return result
    def __len__(self):
        if self.null():
            return 0
        elif hasattr(self._cdr, "__len__"):
            return 1 + len(self._cdr)
        else:
            return 1
    def __getitem__(self, val):
        if isinstance(val, slice):
            start = val.start if val.start else 0
            stop = val.stop if val.stop else len(self)
            step = val.step if val.step else 1
            target = self
            index = 0
            for _ in range(start):
                if (not target.null()) and hasattr(target, "cdr"):
                    target = target.cdr()
                index += 1
            result = Lisped_list()
            while index < stop:
                if (not target.null()) and hasattr(target, "car"):
                    result.append(target.car())
                for _ in range(step):
                    if (not target.null()) and hasattr(target, "cdr"):
                        target = target.cdr()
                        index += 1
            return result
        else:
            if self.null() or (not hasattr(self._cdr, "__getitem__")):
                raise IndexError(f"index out of range.")
            elif val < 0:
                raise IndexError(f"index must be non-negative.")
            elif val == 0:
                return self.car()
            else:
                return self.cdr()[val - 1]
    def __iter__(self):
        tmp = self
        while (not tmp.null()) and hasattr(tmp.cdr(), "car"):
            yield tmp.car()
            tmp = tmp.cdr()
    def iter_cycle_safe(self):
        seen = set()
        tmp = self
        while all([
            (not tmp.null()),
            (tmp not in seen),
            hasattr(tmp.cdr(), "car")]):
            seen.add(tmp)
            yield tmp.car()
            tmp = tmp.cdr()
        if tmp in seen:
            yield Symbol("...")
    def __repr__(self):
        if type(self._cdr) == Lisped_list:
            return "(" + " ".join([
                (repr(val) if val != self else "(...)")
                for val in self.iter_cycle_safe()
            ]) + ")"
        elif self.null():
            return "nil"
        else:
            # dotted pair
            return f"({repr(self._car)} . {repr(self._cdr)})"
    def __bool__(self):
        return not self.null()

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
    elif env.outer:
        return find_in_env(symbol, env.outer)
    else:
        print(f"Can't find {symbol} in environment!")
        return None

global_env = Env()
global_env.update({
    Symbol("+"):        (lambda a, b: a+b),
    Symbol("-"):        (lambda a, b: a-b),
    Symbol("*"):        (lambda a, b: a*b),
    Symbol("%"):        (lambda a, b: a%b),
    Symbol("/"):        (lambda a, b: a//b),
    Symbol("pow"):      (lambda a, b: a**b),
    Symbol("floatdiv"): (lambda a, b: a/b),
    Symbol("<"):        (lambda a, b: a < b),
    Symbol(">"):        (lambda a, b: a > b),
    Symbol("print"):    (lambda *args: print(*args)),
    Symbol("car"):      (lambda arg: arg.car()),
    Symbol("cdr"):      (lambda arg: arg.cdr()),
    Symbol("="):        (lambda a, b: a == b),
    Symbol("cons"):     (lambda a, b: Lisped_list.cons(a, b)),
    Symbol("and"):      (lambda *args: all(args)),
    Symbol("or"):       (lambda *args: any(args)),
    Symbol("not"):      (lambda *args: not any(args)),
    Symbol("contains"): (lambda a, b: (b in a)),
    Symbol("prompt"):   (lambda *p: input(" ".join(p))),
    Symbol("lower"):    (lambda s: s.lower()),
    Symbol("upper"):    (lambda s: s.upper()),
    Symbol("index"):    (lambda s, i: s[i]),
    Symbol("begin"):    (lambda *args: args[-1] if args else None),
    Symbol("null?"):    (lambda a: bool(a) == False),
    Symbol("->str"):    (lambda a: str(a)),
    Symbol("->int"):    (lambda a: int(a)),
    Symbol("->list"):   (lambda a: Lisped_list(a)),
    Symbol("tuple"):    (lambda *args: tuple(args)),
    Symbol("length"):   (lambda a: len(a)),
    Symbol("slice"):    (lambda s, i, j: s[i:j]),
    Symbol("True"):     True,
    Symbol("False"):    False,
    Symbol("None"):     None,
    Symbol("nil"):      Lisped_list(),
    Symbol("file-contents"):
                        read_from_file, # takes filename as arg
    Symbol("str-replace"):
                        (lambda s, a, b: s.replace(a, b)),
    # dict methods
    Symbol("dict-new"): (lambda: dict()),
    Symbol("dict-set"): (lambda a, key, val: a.update({key: val})),
    Symbol("dict-get"): (lambda a, key, default=None: a.get(key, default)),
    Symbol("dict-keys"):
                        (lambda d: d.keys()),
    Symbol("dict-update"):
                        (lambda a, b: {
        k:v for subdict in [a,b] for k,v in subdict.items()
    }),
    # set methods
    Symbol("set-new"):  (lambda: set()),
    Symbol("set-add"):  (lambda a, val: a.add(val)),
    # random methods
    Symbol("random-choice"):
                        (lambda s: random.choice(s)),
})

def evaluate(x, env=global_env):
    if DEBUG:
        print(f"evaluating {x}")
        if env!=global_env:
            print("\n  ".join(str(tuple(pair))
                for pair in env.items()))
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
            return Procedure(x[1], x[2:], env)
        elif x[0] == Symbol("macro"):
            return Macro(x[1], x[2:], env)
        elif x[0] == Symbol("quote"):
            return x[1]
        elif x[0] == Symbol("eval"):
            return evaluate(evaluate(x[1],env),env)
        elif x[0] == Symbol("expand-macro"):
            expr = x[1]
            return (evaluate(expr[0], env))(expr.cdr(), env)
        else:
            fn = evaluate(x[0], env)
            if hasattr(fn, "__call__"):
                if type(fn) == Macro:
                    # expand macro
                    args = x.cdr()
                    macro_ast = fn(args, env)
                    #print(f"Expanded macro to: {repr(macro_ast)}")
                    return evaluate(macro_ast, env)
                else:
                    args = Lisped_list((evaluate(val, env) for val in x.cdr()))
                    if type(fn) == Procedure:
                        retval = fn(args, env)
                        return retval
                    else:
                        # This must be a Python function
                        return fn(*args)
            else:
                print(f"No such function: {x[0]}")
    elif type(x) == Symbol:
        return find_in_env(x, env)
    else:
        return x

def eval_string(s):
    try:
        return evaluate(ast_from_tokens(tokenize("(begin " + s + ")")))
    except SyntaxError as s:
        print(f"Syntax Error: {s}")

def repl():
    while True:
        try:
            read = input("> ")
            if read:
                print(repr(eval_string(read)))
        except EOFError:
            break
        except KeyboardInterrupt:
            break

# Standard library
eval_string("""
(set! defmacro (macro (mac-name mac-args mac-body)
    (cons (quote set!)
        (cons mac-name
            (cons
                (cons (quote macro)
                    (cons mac-args
                        (cons mac-body nil)))
                nil)))))

(defmacro defun (defun-name defun-args &defun-body)
    (cons (quote set!)
        (cons defun-name
            (cons
                (cons (quote lambda)
                    (cons defun-args defun-body))
                nil))))

(defmacro cond (&params)
    (if (null? params)
        (quote nil)
        (cons (quote if)
            (cons (car (car params))
                (cons (car (cdr (car params)))
                    (cons
                        (cons
                            (quote cond)
                            (cdr params))
                        nil))))))

(defmacro let (params &body)
    (cons
        (cons
            (quote lambda)
            (cons
                (map1 car params)
                body))
        (map1 cadr params)))

(defun cadr (a)
    (car (cdr a)))

(defun caddr (a)
    (car (cdr (cdr a))))

(defun append-two (a b)
    (if (null? a)
        b
        (cons (car a) (append-two (cdr a) b))))

(defun append (&lists)
    (foldr append-two lists nil))

(defun reverse (revseq)
    (foldl cons revseq nil))

(defun map1 (mapfn mapseq)
    (if (null? mapseq)
        nil
        (cons (mapfn (car mapseq)) (map1 mapfn (cdr mapseq)))))

(defun map (mapfn &mapseqs)
    ;; really should use apply here instead of eval...
    (if (foldl or (map1 null? mapseqs) False)
        nil
        (cons
            (eval (cons mapfn (map1 car mapseqs)))
            (eval
                (cons (quote map)
                    (cons (quote mapfn)
                        (map1
                            (lambda (x)
                                (cons
                                    (quote quote)
                                    (cons (cdr x) nil)))
                            mapseqs)))))))

(defun foldl (foldfn foldseq foldacc)
    (if (null? foldseq)
        foldacc
        (foldl foldfn (cdr foldseq) (foldfn (car foldseq) foldacc))))

(defun foldr (foldfn foldseq foldacc)
    (if (null? foldseq)
        foldacc
        (foldr foldfn (cdr foldseq) (foldfn foldacc (car foldseq)))))

(defun filter (filterfn fseq)
    (if (null? fseq)
        nil
        (if (filterfn (car fseq))
            (cons (car fseq) (filter filterfn (cdr fseq)))
            (filter filterfn (cdr fseq)))))

(defun countif (countiffn countifseq)
    (length (filter countiffn countifseq)))

(defun apply (apply-fn apply-args)
    (eval (cons apply-fn apply-args)))

(defun iterate (iter-fn iter-arg iter-count)
    (if (= iter-count 0)
        iter-arg
        (iterate iter-fn (iter-fn iter-arg) (- iter-count 1))))

(defun qsort (qcmp qseq)
    (if (null? qseq)
        nil
        (append
            (qsort
                qcmp
                (filter
                    (lambda (q) (qcmp q (car qseq)))
                    (cdr qseq)))
            (cons (car qseq) nil)
            (qsort
                qcmp
                (filter
                    (lambda (q) (not (qcmp q (car qseq))))
                    (cdr qseq))))))

(defun delimit (seq delimiter)
    (if (= (length seq) 1)
        (car seq)
        (+ (+ (car seq) delimiter) (delimit (cdr seq) delimiter))))

(defun split-line (line delimiter-list test-index)
    (if (= test-index (length line))
        (cons line nil)
        (if
            (foldl
                or
                (map1
                    (lambda (x) (= x (index line test-index)))
                    delimiter-list)
                False)
            (cons
                (slice line 0 test-index)
                (split-line
                    (slice line (+ test-index 1) (length line))
                    delimiter-list
                    0))
            (split-line line delimiter-list (+ test-index 1)))))

(defun range (rangestart rangeend)
    (if (< rangestart rangeend)
        (cons rangestart (range (+ rangestart 1) rangeend))
        nil))

(defun acons (a-key a-value a-list)
    (cons (cons a-key a-value) a-list))

(defun assoc (a-key a-list)
    (if (null? a-list)
        nil
        (if (= a-key (car (car a-list)))
            (car a-list)
            (assoc a-key (cdr a-list)))))

(defun rassoc (a-key a-list)
    (if (null? a-list)
        nil
        (if (= a-key (cdr (car a-list)))
            (car a-list)
            (rassoc a-key (cdr a-list)))))
""")
