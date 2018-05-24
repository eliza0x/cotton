# Cotton

LLVMで動く試作言語

# Syntax

```
def fact(n: Int): Int {
    if n == 0 {
        1
    } else {
        n * fact(n-1)
    }
}

def main(): Unit {
    print(fact(5))
}
```

