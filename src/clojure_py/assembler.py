import json as json
import ast
import sys

def err(s):
    sys.stderr.write(s)

def _compile(form):
    err(str(form) + "\n")
    if type(form) == int:
        return form
    elif "args*" in form:
        obj = getattr(ast, form["op"])
        args = map(_compile, form["args*"])
        o = obj(args)
        o.body = args
        return o
    elif "args" in form:
        obj = getattr(ast, form["op"])
        args = map(_compile, form["args"])
        return obj(*args)
    else:
        obj = getattr(ast, form["op"])
        return obj()

def assemble(j):
    a = _compile(json.load(j))
    err(ast.dump(a) + "\n")
    return json.dumps("baz!")

def main():
    print(assemble(sys.stdin))


if __name__ == "__main__":
    main()

    
