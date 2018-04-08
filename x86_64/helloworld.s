/* Write to file 1 (STDOUT)
   Write from string at the given location
   Write 14 bytes ("hello, world!\n") */

.text
.globl main

main:
  movq $message, %rsi  /* Pointer to char */
  movq $1, %rax         /* Interrupt code (SYS_WRITE) */
  movq $1, %rdi         /* File descriptor (1=STDOUT) */
  movq $14, %rdx        /* Length of message (14 bytes) */
  syscall

  movq $60, %rax        /* Interrupt code (EXIT) */
  movq $0, %rdi         /* Exit code (0) */
  syscall

.data
message:    .ascii "Hello, world!\n"
