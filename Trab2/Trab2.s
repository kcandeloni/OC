# Autor: Kevin Willian Candeloni
# e-Mail: kcandeloni@gmail.com
# Descrição: Implementação em assembly do processador MIPS, do segundo algoritmo da divisão do livro do Patterson. 

.text
main:

lw $s5, mascLSB
lw $s6, mascMSB

passo0:
lw $s1, Dividendo # Carrega o valor do Dividendo para o Resto 
# $s1 e $s2 serao os registradores do resto sendo #s2 o registrador com os bits mais significativos do Resto 
# $s3 e $s4 serao os registradores do Divisor sendo #s4 o registrador com os bits mais significativos do Divisor 
#  $s2$s1
# +$s4$s3

srl $t1, $s1, 31 # Desloca o Dividendo 31 bits para direita

sll $s1, $s1, 1 # Desloca os bits menos significativos do Dividendo em 1 bit para esquerda
sll $s2, $s2, 1 # Desloca os bits mais significativos do Dividendo em 1 bit para esquerda

bne $t1, $zero, soma1 # Verifica se o bit mais significativo de $s1 era != 0 antes do deslocamento
# Se era zero, salta para o passo 2
# Caso contrario adiciona salta para soma1, onde será somado 1 no registrador com os bits mais significativos do Resto
j passo1

soma1:
addi $s2, $s2, 1 # Soma 1 ao registrador com os bits mais significativos do Resto,
# caso tenha ocorrido perde de um bit no deslocamento lógico

passo1:
addi $t9, $zero, $zero # zera o valor da flag $t9, quando for 1, quer dizer que o bit mais significativo da soma dos bits
# mais significativos do Resto e do divisor negado é == 1, e então o valor do Resto deve ser restaurado

lw $s4, Divisor # Carrega para os bits mais significativos o divisor 
# Coloca em complemento de 2 o divisor 
divNegado:
xor $t4, $s4, masXOR # Armazena em $t4 os bits mais significativos do divisor negado
addi $t4, $t4, 1 # Soma 1 ao Divisor negado

add $s2, $s2, $t4 # Soma os bits mais significativos do Resto com o Divisor negado
bne $t9, $zero, passo2 # Se $t9 == 1, quer dizer que o valor do resto foi restaurado, então salta proxímo passo 
srl $t1, $s2, 31 # Desloca 31 bits para direita os bits mais significativos do Resto

beq $t1, $zero, passo2 # Berifica se o bit mais significativo do Resto é == zero
# Se for == 0, salta para o passo 2
# Caso contrário, restara o valor do resto
# Para isso, tira o divisor do complemento de 2 e soma ele ao resto
addi $t9, $zero, 1 # $t9 vai ser uma flag, que controlará quando vai ser restaurado o valor do Resto 
j divNegado # salta para onde será restaurado o valor do Resto

passo2:

#and $t1, $s1, mascMM

.data
Dividendo:	.word	0x0f221a61 # 253.893.217 # 0000 1111 0010 0010 0001 1010 0110 0001
Divisor:	.word	0x00000315 # 789	 # 0000 0000 0000 0000 0000 0011 0001 0101

mascLSB:	.word	0xffff0000 # Mascara para bits mais signifcativos
mascMSB:	.word	0x0000ffff # Mascara para bits menos signifcativos
mascXor:	.word	0xffffffff

tDividendo:	.word	0x00000009
tDivisor:	.word	0x00000002

dividendoMenor:	.ascii	"Dividendo menor que divisor" 

