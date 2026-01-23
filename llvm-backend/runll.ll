target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-w64-windows-gnu"

@helloworld = constant [3 x i8] c"hi\00"

@comb = constant [7 x i8] c"+++SKKx"
@heap = global ptr null

; 2-bit character {00 = +, 01 = S, 10 = K, 11 = I}
; left, a "pointer" (i64 offset from @heap ptr)
; right, a "pointer" (i64 offset from @heap ptr)
; references, an i32 count of references pointing to this node
%struct.HeapComb = type { i2, i64, i64, i32 }

define i32 @main() {
  call ptr @puts(ptr @helloworld)
  ret i32 0
}

declare i32 @puts();