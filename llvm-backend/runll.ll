target datalayout = "e-m:w-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-w64-windows-gnu"

; value, a string (ptr) containing S, K, I, or a variable name
; left,  a "pointer" (i64 offset from @heap ptr)
; right, a "pointer" (i64 offset from @heap ptr)
; references, an i32 count of references pointing to this node
%HeapComb = type { ptr, i32, i32, i32 }

@startprompt = constant [26 x i8] c"Interpreting comb term...\00"
@looping     = constant [11 x i8]  c"Looping...\00"

@comb = constant [7 x i8] c"+++SKKx"

@heap       = global ptr null
@heapLength = global i64 1
@heapSize   = global i64 0

define i32 @main() {
Start:
  call ptr @puts(ptr @startprompt)
  %dynamicHeap = call ptr @malloc(i64 162)
  store ptr %dynamicHeap, ptr @heap
  call ptr @parseComb()
  ret i32 0
}

; Parse the comb string, creating a node in @heap for each symbol
define void @parseComb() {
Start:
  %parseStack = call ptr @malloc(i64 64)
  %parseStackSize = alloca i64
  store i64 1, ptr %parseStackSize
  %parseStackPtr  = alloca i64
  store i64 0, ptr %parseStackPtr
  br label %Loop

Loop:
  %i = phi i64 [ 0, %Start ], [ %newi, %Loop ]
  %symbol = getelementptr [7 x i8], ptr @comb, i64 0, i64 %i
  %symbolVal = load i8, ptr %symbol
  call ptr @putchar(i8 %symbolVal)
  %newi = add i64 %i, 1
  %cmp = icmp ult i64 %newi, 7               ; second arg here is size of @comb
  br i1 %cmp, label %Loop, label %Exit

Exit:
  ret void
}

declare i32 @puts();
declare i32 @putchar();
declare ptr @malloc();
declare ptr @realloc();