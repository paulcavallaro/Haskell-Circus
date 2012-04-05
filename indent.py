def foo(x):
    if x > 5:
        return x
    else:
        y = 10
    return y

def bar(x):
    def baz(y):
        if y > x:
            return y
    return baz(x+1)
