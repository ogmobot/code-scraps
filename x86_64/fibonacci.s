	.text
	.globl fibonacci
	.type fibonacci, @function
fibonacci:
prologue:
	push %rbp
	mov %rsp, %rbp
	push %rbx /*x*/
	   /*%rax   y*/
	push %rdx /*z*/
	push %rcx /*counter*/
main:
	mov %rdi, %rcx
	mov $0, %rbx
	mov $1, %rax
	cmp $1, %rcx
	jbe epilogue
	dec %rcx
loop:
	mov %rbx, %rdx
	add %rax, %rdx
	mov %rax, %rbx
	mov %rdx, %rax
	dec %rcx
	cmp $0, %rcx
	jne loop
epilogue:
	pop %rcx
	pop %rdx
	pop %rbx
	mov %rbp, %rsp
	pop %rbp
	ret
