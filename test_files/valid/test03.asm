	# Example program to compute the sum of squares from Jorgensen [2016]

	#---------------------------------------------------------------
	# data declarations
	
	.data
n:		.word 10
sumOfSquares:	.word 0

	#---------------------------------------------------------------
	# the program
	.text
main:
	lw $t0, n
	addi $t1, $0, 1
	addi $t2, $0, 0

  addi $t5, $0, 1

sumLoop:
  addi $t4, $0, 0
squareLoop:
  add $t3, $t3, $t1
  addi $t4, $t4, 1
  bne $t4, $t1, squareLoop

  add $t2, $t2, $t3
  addi $t1, $t1, 1

  slt $t4, $t1, $t0
  beq $t4, $t5, sumLoop
  beq $t1, $t0, sumLoop

	sw  $t2, sumOfSquares

end:
	j end
	
