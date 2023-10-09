	# A test file of data declarations only
	.data
var1:	.word 1024             # int var1 = 1024

var2:	.half 12               # short var2 = 12
	
var3:	.byte 0                # char var3 = 0

var4:	.byte 1

var5:	.space 512             # reserve 512 bytes

var6:	.ascii "hello"

var7:	.asciiz "goodbye"

	.text
main:
  add $t0, $t0, $t0	

