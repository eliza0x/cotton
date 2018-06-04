# Cotton

[![CircleCI](https://circleci.com/gh/eliza0x/cotton/tree/master.svg?style=svg)](https://circleci.com/gh/eliza0x/cotton/tree/master)

LLVMで動く試作言語, 関数型言語を目指しています

# todo

- 型推論
- ユーザー定義の演算子
- グローバル変数
- 高階関数
- 構造体
- 直和型

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

