def bar(x):
    def baz(y):
        if y > x:
            y = y + y
            return y
    return baz(x+1)
