define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 10, i32* %1, align 4
  %3 = load i32, i32* %1, align 4
  store i32 %3, i32* %2, align 4
  ret i32 0
}
