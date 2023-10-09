        .data
VALUE = 3
var1:   .word 12 # test
var2:   .word 10
        .text
main:
        lw $t0, var1
        lw $t1, var2
        # test 2
        sw $t1, var2 #test 3 sw $t1, var3
        or $t2, $t0, $t1  
        oi $t2, $t0, 3 
        beq $t1, $t1, main
        jr $t1
        sll $t4, $t5, VALUE
