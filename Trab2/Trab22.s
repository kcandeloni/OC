# Autor: Kevin Willian Candeloni
# e-Mail: kcandeloni@gmail.com
# Descrição: Implementação em assembly do processador MIPS, do segundo algoritmo da divisão do livro do Patterson. 
.text
main:

iteracao0:
lw $s7, mascXor
lw $s1, tDividendo # Carrega o valor do Dividendo para o Resto 
# $s1 e $s2 serao os registradores do resto sendo #s2 o registrador com os bits mais significativos do Resto 
# $s3 e $s4 serao os registradores do Divisor sendo #s4 o registrador com os bits mais significativos do Divisor 
#  $s2$s1
# +$s4$s3
add $t8, $zero, $zero # Zera o contador de iteraçoes
###################################################################################################

Deslocamento1esc:
add $t7, $zero, $zero
srl $t1, $s1, 31 # Desloca o Dividendo 31 bits para direita (verifica se vai haver um bit que deve passar para a parte mais significativa do Resto)

sll $s1, $s1, 1 # Desloca os bits menos significativos do Dividendo em 1 bit para esquerda (Deslocamento da parte menos significativa)
sll $s2, $s2, 1 # Desloca os bits mais significativos do Dividendo em 1 bit para esquerda (Deslocamento da parte mais significativa)

beq $t1, $zero, VerificaFlag # Verifica se o bit mais significativo de $s1 era == 0 antes do deslocamento
# Se era zero, salta para o SomaResto
# Caso contrario soma 1 aos bist mais significativos do Resto

soma1:
addi $s2, $s2, 1 # Soma 1 ao registrador com os bits mais significativos do Resto,
# caso tenha ocorrido perde de um bit no deslocamento lógico

VerificaFlag:
beq $t9, $zero, SomaResto # No caso do bit mais significativo apos a soma do Resto + o Divisor negado seja = 0
addi $s1, $s1, 1
add $t9, $zero, $zero # zera o valor da flag $t9, quando for 1, quer dizer que o bit mais significativo da soma dos bits
 # mais significativos do Resto e do divisor negado é == 1, e então o valor do Resto deve ser restaurado
##################################################################################################

SomaResto:
beq $t8, 32, DeslocaResto # Finaliza
addi $t8, $t8, 1 # Incremenda o contador de Iteraçoes

lw $s4, tDivisor # Carrega para os bits mais significativos o divisor
# Coloca em complemento de 2 o divisor 
divNegado:
xor $s4, $s4, $s7 # Armazena em $t4 os bits mais significativos do divisor negado
addi $s4, $s4, 1 # Soma 1 ao Divisor negado


add $s2, $s2, $s4 # Soma os bits mais significativos do Resto com o Divisor negado
bne $t7, $zero, Deslocamento1esc

srl $t1, $s2, 31 # Desloca 31 bits para direita os bits mais significativos do Resto


beq $t1, $zero, bitResto0 # Verifica se o bit mais significativo do Resto é == zero
# Se for == 0, salta para o passo 2
# Caso contrário, restara o valor do resto
# Para isso, tira o divisor do complemento de 2 e soma ele ao resto

addi $t7, $zero, 1 # $t7 vai ser uma flag, que controlará quando vai ser restaurado o valor do Resto 
j divNegado # salta para onde será restaurado o valor do Resto

bitResto0:
addi $t9, $zero, 1
j Deslocamento1esc

####################################################################################################
# Finaliza
DeslocaResto:
srl $s2, $s2, 1 # Desloca os bits mais significativa do Resto para direita

# Imprime Quociente
add $a0, $zero, $s2 # Carrega para $a0 o valor a ser impresso
li $v0, 36 # Imprime inteiro
syscall

li    $a0, ' '
li    $v0, 11
syscall

# Imprime Resto
add $a0, $zero, $s1 # Carrega para $a0 o valor a ser impresso
li $v0, 36 # Imprime inteiro
syscall
#and $t1, $s1, mascMM

.data
Dividendo:	.word	0x0f221a61 # 253.893.217 # 0000 1111 0010 0010 0001 1010 0110 0001
Divisor:	.word	0x00000315 # 789	 # 0000 0000 0000 0000 0000 0011 0001 0101
mascXor:	.word	0xffffffff

tDividendo:	.word	0x00000009
tDivisor:	.word	0x00000002
