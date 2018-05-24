define void @init()  {	
  ret void
}

define i32 @main()  {	
	%main_a = alloca i32, align 4
	%main_a_b = alloca i32, align 4
	store i32 10, i32* %1, align 4
	store i32 20, i32* %2, align 4
	store i32* %main_a, i32* %ks9MCzdo, align 4
	store i32* %main_a_b, i32* %BZjlRdmB, align 4
	%_return = add nsw i32 %ks9MCzdo, %BZjlRdmB
	ret i32 %_return
}

