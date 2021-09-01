import shoddylisp
quine = """
((lambda (x)
    (cons
        x
        (cons
            (cons
                (quote quote)
                (cons x (quote ())))
            (quote ()))))
(quote
(lambda (x)
    (cons
        x
        (cons
            (cons
                (quote quote)
                (cons x (quote ())))
            (quote ()))))))
"""

quine = " ".join(quine.strip().split())
print("Source:")
print(quine)
print("Ouptut:")
print(shoddylisp.eval_string(quine))
#print("Output of evaluating output:")
#print(shoddylisp.eval_string(str(shoddylisp.eval_string(quine))))
