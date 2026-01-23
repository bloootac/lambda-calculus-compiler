target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-w64-windows-gnu"

; 2-bit character {00 = +, 01 = S, 10 = K, 11 = I}
; left,  a "pointer" (i64 offset from @heap ptr)
; right, a "pointer" (i64 offset from @heap ptr)
; references, an i32 count of references pointing to this node
%struct.HeapComb = type { i2, i64, i64, i32 }

@startprompt = constant [26 x i8] c"Interpreting comb term...\00"
@looping     = constant [11 x i8]  c"Looping...\00"

@comb = constant [7 x i8] c"+++SKKx"

@heap       = global ptr null
@heapLength = global i64 1
@heapSize   = global i64 0

define i32 @main() {
  call ptr @puts(ptr @startprompt)
  %2 = call ptr @malloc(i64 1)
  store ptr %2, ptr @heap
  br label %5

5:
  %6 = phi i64 [ 0, %0 ], [ %9, %5 ]
  %7 = getelementptr [7 x i8], ptr @comb, i64 0, i64 %6
  %deref = load i8, ptr %7
  call ptr @putchar(i8 %deref)
  %9 = add i64 %6, 1
  %10 = icmp ult i64 %9, 7               ; second arg here is size of @comb
  br i1 %10, label %5, label %11

11:
  ret i32 0
}

declare i32 @puts();
declare i32 @putchar();
declare ptr @malloc();