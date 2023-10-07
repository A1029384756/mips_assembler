        .data
VALUE = 3
var1:   .word 12
var2:   .word 10
        .text
main:
        lw $t0, var1
        lw $t1, var2
        or $t2, $t0, $t1  
        or $t2, $t0, VALUE
	
