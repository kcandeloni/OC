#*******************************************************************************
# exercicio057.s               Copyright (C) 2017 Giovani Baratto
# This program is free software under GNU GPL V3 or later version
# see http://www.gnu.org/licences
#
# Autor: Giovani Baratto (GBTO) - UFSM - CT - DELC
# e-mail: giovani.baratto@ufsm.br
# versÃ£o: 0.1
# DescriÃ§Ã£o: Leitura de palavras (grupo de 4 bytes) de um aquivo e escrita do
#            corespondente valor hexadecimal no console. Em uma linha escrevemos
#            8 palavras.
#
# Documentaçãoo:
# Assembler: MARS
# RevisÃµes:
# Rev #  Data           Nome   ComentÃ¡rios
# 0.1    08.05.2017     GBTO   versÃ£o inicial 
#*******************************************************************************
#        1         2         3         4         5         6         7         8
#2345678901234567890123456789012345678901234567890123456789012345678901234567890
#           M     O             #
.text
.globl      main
main:
	lw $s1, addr
	lw $s2, rs
	lw $s3, rt
	lw $s4, rd
	lw $s5, shant
	lw $s6, funct
	lw $s7, imm
            addiu $sp, $sp, 12  # alocamos na pilha espaÃ§o para as variÃ¡veis
            # abertura do arquivo de leitura
            la    $a0, arquivoEntrada # $a0 <- endereÃ§o da string com o nome do arquivo
            li    $a1, 0 # flags: 0  - leitura
            li	  $a2, 0 # modo - atualmente Ã© ignorado pelo serviÃ§o
            #chamamos o serviÃ§o 13 para a abertura do arquivo
            li    $v0, 13	
            syscall   
            sw    $v0, 0($sp)   # gravamos o descritor do arquivo
            slt   $t0, $v0, $zero # verificamos se houve um erro na abertura do arquivo
            bne   $t0, $zero, erroAberturaArquivoLeitura
            #  fazemos um contador igual a 8
            li    $t0, 8
            sw    $t0, 8($sp)
            # enquanto nÃ£o chegamos no final do arquivo executamos o laÃ§o lacoLeiaPalavra
            j     verificaFinalArquivo            
lacoLeiaPalavra:
            # imprimimos a palavra se a leitura foi correta
            lw    $a0, 4($sp)   # tomamos a palavra do buffer
            li    $v0, 34       # serviÃ§o 34: imprime um inteiro em hexadecimal
            syscall
imprimeEspaco:
            # imprimimos um espaÃ§o
            li    $a0, ' '
            li    $v0, 11
            syscall
	    
	    lw $t3, 4($sp) 	# Armazena em $t3 a instrução
	    srl $t4, $t3, 26 	# Desloca 26 bits a direita e armazena os 6 primeiros bits(o opcode) em $t4

################################################################################################################
#################################### Início do código proposto pelos alunos ####################################

# Autores: Kevin Willian Candeloni e Raphael Vieira Miollo
# e-mails: kcandeloni@gmail.com e rmiollo@inf.ufsm.br

# Descrição: Converte instruções em linguagem de máquina em linguagem de montagem do processador MIPS. 

	add $t9, $zero, $zero # Zera $t9: ela será uma flag, que diz se é tipo R ($t9==1)
	add $t8, $zero, $zero # # Zera $t8: ela será uma flag, que diz se é a intrução jr ($t8==1) ou sll, srl, sra ($t8==2)
	
	# Tipo R
	beq $t4, 0, func # Caso o opcode==00, salta para onde a vai verificar o campo func, para saber qual é a instrução
	# Caso contrario, continua verificando o campo opcode
	
#################################### Verifica o opcode da instruções J    
	# VERIFICA INSTRUÇÕES TIPO J
	beq $t4, 2, op_j # Compara se o valor do opcode é referente a instrução op_j, se for saltará para onde é impresso essa instrução
	beq $t4, 3, op_jal
	
#################################### opcode tipo I
	# VERIFICA INSTRUÇÕES TIPO I
	beq $t4, 8, op_addi
	beq $t4, 9, op_addiu	
	beq $t4 12, op_andi
	beq $t4, 13, op_ori
	beq $t4, 14, op_xori
	beq $t4, 10, op_slti
	beq $t4, 11, op_sltiu
	
	beq $t4, 5, op_bne
	beq $t4, 4, op_beq
	
	beq $t4, 32, op_lb
	beq $t4, 35, op_lw
	beq $t4, 36, op_lbu
	
	beq $t4, 15, op_lui
	
	beq $t4, 40, op_sb
	beq $t4, 43, op_sw	
	
	#beq $t4, 41, op_sh
	#beq $t4, 37, op_lhu
	
	# Tipo R*
	beq $t4, 16, op_rfe # Caso especial tipo R (func 0x20) 
	
	j decrementaContador # Se o opcode não estiver listado, pega a proxima instrução

################################## 
# VERIFICA INSTRUÇÔES TIPO R (campo func)
func:	
	and $t4, $t3, $s6 # Mascara do campo func
	addi $t9, $zero, 1 # $t9 será uma flag, que diz se é tipo R ($t9==1)
	
	beq $t4, 32, func_add  # Compara se o valor do campo func é referente a instrução add, se for salta para onde a instrução vai ser impressa
	beq $t4, 33, func_addu # Caso não for, compara com o número da próxima instrução
	beq $t4, 34, func_sub
	beq $t4, 35, func_subu
	beq $t4, 36, func_and
	beq $t4, 37, func_or
	beq $t4, 38, func_xor
	beq $t4, 39, func_nor
	beq $t4, 42, func_slt
	beq $t4, 43, func_sltu
	
	beq $t4, 26, func_div
	beq $t4, 27, func_divu
	beq $t4, 24, func_mult
	beq $t4, 25, func_multu

	beq $t4, 8, func_jr
	
	beq $t4, 0, func_sll
	beq $t4, 2, func_srl
	beq $t4, 3, func_sra
	
	beq $t4, 12, func_syscall
	beq $t4, 13, func_break
 	
 	j decrementaContador # Se não encontrou instrução, continua a leitura do arquivo
	
#################################### IMPRESSÃO
# FUNÇÕES DE IMPRIMIR 

imprime_reg: # Imprime o registrador 
        add $a0, $zero, $t4 # Carrega em $a0 a string referente ao registrador a ser impresso
        li $v0, 4 # Imprime string
	syscall
	jr $ra

imprimeInstr: # Imprime qual é a instrução
	li $v0, 4 # Iprime string
	syscall
	jr $ra
	
imprimeShamt:
	add $a0, $zero, $t4 # Carrega para $a0 o valor a ser impresso
	li $v0, 36 # Imprime inteiro
	syscall
	beq $t9, 3, rs_reg # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm(rs)
	
	j decrementaContador

imprimeImediato: # Imprime valor Hexadecimal de $t4 
	add $a0, $zero, $t4
	li $v0, 34 # Imprime Hexadecimal
	syscall
	
	j decrementaContador 
	
imprimeTipoJ: # Imprime instruções do tipo J
	li $v0, 4 # Imprime string
	syscall
 	
	and $t4, $t3, $s1 # Mascara do campo addr
	
	j imprimeImediato # Imprime o imm

############################### CAMPOS 
# CALCULA OS CAMPOS DA INTRUÇÃO
rd_reg:
	and $t4, $t3, $s4 # Mascara do campo rd
	srl $t4, $t4, 11  # Faz o deslocamento de 11 bits
	jal reg_zero      # Pega o registrador (campo rd)
        jal imprime_reg   # Imprime o registrador
        
        beq $t8, 2, rt_reg # Se a instrução for do tipo deslocaento lógico, salta para rt_reg
rs_reg:
        and $t4, $t3, $s2 # Mascara do campo rs
        srl $t4, $t4, 21  # Faz o deslocamento de 21 bits, para pegar o valor certo do campo rs
	jal reg_zero      # Pega o registrador (campo rd)
	jal imprime_reg   # Imprime o registrador
	
	beq $t8, 1, decrementaContador # Se a instrulção é a jr, só pegao campo rs
	beq $t9, $zero, rt_reg # Se é do tipo I (Branches) salta para rs_reg ( pega valor no campo rs)
	beq $t9, 2, imm_reg # Para os casos da instrução tipo I imprima a ordem dos registradores: rs -> rt -> imm
	beq $t9, 3, decrementaContador # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm(rs)
rt_reg:
	and $t4, $t3, $s3 # Mascara do campo rt
	srl $t4, $t4, 16  # Faz o deslocamento de 16 bits, pega o valor de rt
        jal reg_zero      # Pega o registrador (campo rt)
        jal imprime_reg   # Imprime o registrador
	
	beq $t8, 2, shamt_reg # Se a instrução for do tipo deslocaento lógico, salta para shamt_reg
	beq $t9, $zero, imm_reg # Se é do tipo I (Branches)salta para imm_reg ( pega valor no campo immediate)
	beq $t9, 2, rs_reg # Para os casos da instrução tipo I imprima a ordem dos registradores: rs -> rt -> imm
	beq $t9, 3, imm_reg # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm(rs)
	beq $t9, 4, imm_reg # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm
        j decrementaContador   # Se é do tipo R já imprimiu os registradores
	
imm_reg:
	and $t4, $t3, $s7 # Mascara do campo imm
	beq $t9, 2, imprimeShamt # Tipo I que imprima a ordem dos registradores: rs -> rt -> imm. E imm sejá um número (decimal)
	beq $t9, 3, imprimeShamt # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm(rs) (decimal)
        beq $t9, 4, imprimeShamt # Para os casos da instrução tipo I imprima a ordem dos registradores: rt -> imm (decimal)
        j imprimeImediato

shamt_reg:
	and $t4, $t3, $s5 # Mascara do campo shamt
	srl $t4, $t4, 6  # Faz o deslocamento de 6 bits, pega o valor de shamt
	j imprimeShamt
	
################################## J
# INSTRUÇÕES TIPO J
op_j:
	la $a0, instr_j
	j imprimeTipoJ
op_jal:
	la $a0, instr_jal
	j imprimeTipoJ
################################## I
# INSTRUÇÕES TIPO I
op_bne:
	la $a0, instr_bne # Carrega em $a0 o texto que vai ser impresso
	jal imprimeInstr  # Imprime o texto, que é o referente a instrução
	jal rs_reg        # Chama a função que vai calcular os campos da instrução
op_beq:
	la $a0, instr_beq
	jal imprimeInstr
	jal rs_reg # rs -> rt -> imm
op_addi:
	la $a0, instr_addi
	jal imprimeInstr
	addi $t9, $zero, 2 # Seta a flag o valor 2, para os casos de imprimir os regitradores dos campos rt -> rs -> imm, nessa ordem
	jal rt_reg
op_addiu:
	la $a0, instr_addiu
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg
op_andi:
	la $a0, instr_andi
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg
op_ori:
	la $a0, instr_ori
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg
op_xori:
	la $a0, instr_xori
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg
op_slti:
	la $a0, instr_slti
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg	
op_sltiu:
	la $a0, instr_sltiu
	jal imprimeInstr
	addi $t9, $zero, 2 # rt -> rs -> imm
	jal rt_reg

op_lb:
	la $a0, instr_lb
	jal imprimeInstr
	addi $t9, $zero, 3 # Seta a flag o valor 3, para os casos de imprimir os regitradores dos campos rt -> imm(rs), nessa ordem
	jal rt_reg
op_lw:
	la $a0, instr_lw
	jal imprimeInstr
	addi $t9, $zero, 3 # rt -> imm(rs)
	jal rt_reg	
op_lbu:
	la $a0, instr_lbu
	jal imprimeInstr
	addi $t9, $zero, 3 # Seta a flag o valor 3, para os casos de imprimir os regitradores dos campos rt -> imm(rs), nessa ordem
	jal rt_reg
	
op_lui:
	la $a0, instr_lui
	jal imprimeInstr
	addi $t9, $zero, 4 # Seta a flag o valor 3, para os casos de imprimir os regitradores dos campos rt -> imm, nessa ordem
	jal rt_reg

op_sb:
	la $a0, instr_sb
	jal imprimeInstr
	addi $t9, $zero, 3 # Seta a flag o valor 3, para os casos de imprimir os regitradores dos campos rt -> imm(rs), nessa ordem
	jal rt_reg
op_sw:
	la $a0, instr_sw
	jal imprimeInstr
	addi $t9, $zero, 3 # Seta a flag o valor 3, para os casos de imprimir os regitradores dos campos rt -> imm(rs), nessa ordem
	jal rt_reg

op_lhu:
	la $a0, instr_lhu
	jal imprimeInstr
	jal rt_reg
op_sh:
	la $a0, instr_sh
	jal imprimeInstr
	jal rt_reg	
################################# R
# INSTRUÇÕES TIPO R
func_add:
	la $a0, instr_add # Carrega em $a0 o texto que vai ser impresso 
	jal imprimeInstr  # Imprime o texto, que é o referente a instrução 
	jal rd_reg        # Chama a função que vai calcular os campos da instrução
func_addu:
	la $a0, instr_addu
	jal imprimeInstr
	jal rd_reg
func_sub:
	la $a0, instr_sub
	jal imprimeInstr
	jal rd_reg
func_subu:
	la $a0, instr_subu
	jal imprimeInstr
	jal rd_reg
func_and:
	la $a0, instr_and
	jal imprimeInstr
	jal rd_reg
func_or:
	la $a0, instr_or
	jal imprimeInstr
	jal rd_reg
func_xor:
	la $a0, instr_xor
	jal imprimeInstr
	jal rd_reg
func_nor:
	la $a0, instr_nor
	jal imprimeInstr
	jal rd_reg
func_slt:
	la $a0, instr_slt
	jal imprimeInstr
	jal rd_reg
func_sltu:
	la $a0, instr_sltu
	jal imprimeInstr
	jal rd_reg
	
func_div: # 2 registradores
	la $a0, instr_div
	jal imprimeInstr
	jal rs_reg
func_divu:
	la $a0, instr_divu
	jal imprimeInstr
	jal rs_reg
func_mult:
	la $a0, instr_mult
	jal imprimeInstr
	jal rs_reg
func_multu:
	la $a0, instr_multu
	jal imprimeInstr
	jal rs_reg
	
func_jr: # Imprime apenas o campo rs
	la $a0, instr_jr
	jal imprimeInstr
	addi $t8, $zero, 1
	jal rs_reg

func_sll: # Imprime rd rt shamt
	la $a0, instr_sll
	jal imprimeInstr
	addi $t8, $zero, 2
	jal rd_reg
func_srl: # Imprime rd rt shamt
	la $a0, instr_srl
	jal imprimeInstr
	addi $t8, $zero, 2
	jal rd_reg
func_sra: # Imprime rd rt shamt
	la $a0, instr_sra
	jal imprimeInstr
	addi $t8, $zero, 2
	jal rd_reg

# Funções que não imprimem registradores:
func_syscall:
	la $a0, instr_syscall # Carrega o testo referente a intrução (syscal, texto: "syscall")
	jal imprimeInstr      # Imprime a instrução
	j decrementaContador  # Continua a execução do programa, pega prox palavra
func_break:
	la $a0, instr_break
	jal imprimeInstr
	j decrementaContador
op_rfe: # Caso especial do tipo R
	la $a0, instr_rfe
	jal imprimeInstr
	j decrementaContador
################################ REGISTRADORES
# Verifica qual é o registrador
reg_zero:
	bne $t4, 0, reg_at # Compara com o número do registrador, se for diferente, salta para o proximo registrador para comparar
	la $t4, zero       # Se o número do registrador for o correto, carrega em $t4 o texto referenete ao registrador (nesse caso "$zero ")
	jr $ra             # Retorna para onde a função foi chamada
reg_at:
	bne $t4, 1, reg_v0
	la $t4, at
	jr $ra
reg_v0:
	bne $t4, 2, reg_v1
	la $t4, v0
	jr $ra
reg_v1:
	bne $t4, 3, reg_a0
	la $t4, v1
	jr $ra
reg_a0:
	bne $t4, 4, reg_a1
	la $t4, a0
	jr $ra
reg_a1:
	bne $t4, 5, reg_a2
	la $t4, a1
	jr $ra
reg_a2:
	bne $t4, 6, reg_a3
	la $t4, a2
	jr $ra
reg_a3:
	bne $t4, 7, reg_t0
	la $t4, a3
	jr $ra
reg_t0:
	bne $t4, 8, reg_t1
	la $t4, t0
	jr $ra
reg_t1:
	bne $t4, 9, reg_t2
	la $t4, t1
	jr $ra
reg_t2:
	bne $t4, 10, reg_t3
	la $t4, t2
	jr $ra
reg_t3:
	bne $t4, 11, reg_t4
	la $t4, t3
	jr $ra
reg_t4:
	bne $t4, 12, reg_t5
	la $t4, t4
	jr $ra
reg_t5:
	bne $t4, 13, reg_t6
	la $t4, t5
	jr $ra
reg_t6:
	bne $t4,14, reg_t7
	la $t4, t6
	jr $ra
reg_t7:
	bne $t4, 15, reg_s0
	la $t4, t7
	jr $ra
reg_s0:
	bne $t4, 16, reg_s1
	la $t4, s0
	jr $ra
reg_s1:
	bne $t4, 17, reg_s2
	la $t4, s1
	jr $ra
reg_s2:
	bne $t4, 18, reg_s3
	la $t4, s2
	jr $ra
reg_s3:
	bne $t4, 19, reg_s4
	la $t4, s3
	jr $ra
reg_s4:
	bne $t4, 20, reg_s5
	la $t4, s4
	jr $ra
reg_s5:
	bne $t4, 21, reg_s6
	la $t4, s5
	jr $ra
reg_s6:
	bne $t4, 22, reg_s7
	la $t4, s6
	jr $ra
reg_s7:
	bne $t4, 23, reg_t8
	la $t4, s7
	jr $ra
reg_t8:
	bne $t4, 24, reg_t9
	la $t4, t8
	jr $ra
reg_t9:
	bne $t4, 25, reg_k0
	la $t4, t9
	jr $ra
reg_k0:
	bne $t4, 26, reg_k1
	la $t4, k0
	jr $ra
reg_k1:
	bne $t4, 27, reg_gp
	la $t4, k1
	jr $ra
reg_gp:
	bne $t4, 28, reg_sp
	la $t4, gp
	jr $ra
reg_sp:
	bne $t4, 29, reg_fp
	la $t4, sp
	jr $ra
reg_fp:
	bne $t4, 30, reg_ra
	la $t4, fp
	jr $ra
reg_ra: # Se o número do registrador não é o de nenhum dos outros registradores, carraga o texto referente ao registrador ra e volta
	la $t4, ra
	jr $ra
	
#################################### Fim do código proposto pelos alunos ####################################
decrementaContador:
            # decrementamos o contador
            lw    $t0, 8($sp)
            addiu $t0, $t0, -1
            sw    $t0, 8($sp)
            # se contador=0 (imprimimos 8 palavras) gera uma nova linha
            bne   $t0, $zero, imprimeLinha
            # faz contador igual a 8
            li    $t0, 8
            sw    $t0, 8($sp)
            li    $a0, '\n'
            li    $v0, 11
            syscall
            j     verificaFinalArquivo
imprimeLinha:
            # imprimimos a quebra de linha
            li    $a0,'\n'
            li    $v0, 11
            syscall
verificaFinalArquivo:
            # lemos uma palavra do arquivo
            lw    $a0, 0($sp)   # $a0 <- descritor do arquivo
            addiu $a1, $sp, 4   # $a1 <- endereÃ§o do buffer de entrada 
            li    $a2, 4        # $a2 <- nÃºmero de caracteres lidos
            li    $v0, 14
            syscall
            # verificamos se foram lidos 4 bytes
            slti  $t0, $v0, 4
            beq   $t0, $zero, lacoLeiaPalavra
            # terminamos o programa
            li    $a0, 0 # 0 <- programa terminou de forma normal
            li    $v0, 17 # serviÃ§o exit2 - termina o programa
	    syscall
	   
erroAberturaArquivoLeitura:
            la    $a0, mensagemErroAberturaArquivo # $a0 <- endereÃ§o da string com mensagem de erro
            li    $v0, 4	    # serviÃ§o 4: impressÃ£o de string
            syscall	            # fazemos uma chamada ao sistema: fazemos a impressÃ£o da string, indicando o erro.
            li    $a0, 1 # valor diferente de 0: o programa terminou com erros
            li    $v0, 17 #serviÃ§o exit2 - termina o programa	
            syscall
                  
.data
arquivoEntrada: # nome do arquivo de entrada
.asciiz		"codigo.bin"
mensagemErroAberturaArquivo: # mensagem de erro se o arquivo nÃ£o pode ser aberto
.asciiz		"Erro na abertura do arquivo de entrada\n"

######################################### 
# Funções
# J
instr_j:	.asciiz		"j "
instr_jal:	.asciiz		"jal "

# I
instr_addi: .asciiz	    "addi "
instr_addiu:.asciiz		"addiu "
instr_andi: .asciiz	    "andi "
instr_ori:	.asciiz		"ori "
instr_xori:	.asciiz		"xori "
instr_slti:	.asciiz		"slti "
instr_sltiu:.asciiz		"sltiu "


instr_bne:	.asciiz		"bne "
instr_beq:	.asciiz		"beq "

instr_lui:	.asciiz		"lui "

instr_lb:	.asciiz		"lb "
instr_lw:	.asciiz		"lw "
instr_lbu:	.asciiz		"lbu "

instr_sb:	.asciiz		"sb "
instr_sw:	.asciiz		"sw "

instr_sh:	.asciiz		"sh "
instr_lhu:	.asciiz		"lhu "

# R
instr_add:	.asciiz		"add "
instr_addu:	.asciiz		"addu "
instr_sub:	.asciiz		"sub "
instr_subu:	.asciiz		"subu "
instr_and:	.asciiz		"and "
instr_or:	.asciiz		"or "
instr_xor:	.asciiz		"xor "
instr_nor:	.asciiz		"nor "
instr_slt:	.asciiz		"slt "
instr_sltu:	.asciiz		"sltu "
# R chamada
instr_rfe:	.asciiz		"rfe"
instr_syscall:	.asciiz		"syscall"
instr_break:.asciiz		"break"
# R rs
instr_jr:	.asciiz		"jr "
instr_jalr:	.asciiz		"jalr "
# R 2_reg
instr_mult:	.asciiz		"mult "
instr_multu:.asciiz		"multu "
instr_div:	.asciiz		"div "
instr_divu:	.asciiz		"divu "
# R shamt
instr_sll:	.asciiz		"sll "
instr_srl:	.asciiz		"srl "
instr_sra:	.asciiz		"sra "
########################################
# Registradores
zero:	.asciiz		"$zero "
at:		.asciiz		"$at "
v0:		.asciiz		"$v0 "
v1:		.asciiz		"$v1 "
a0:		.asciiz		"$a0 "
a1:		.asciiz		"$a1 "
a2:		.asciiz		"$a2 "
a3:		.asciiz		"$a3 "
t0:		.asciiz		"$t0 "
t1:		.asciiz		"$t1 "
t2:		.asciiz		"$t2 "
t3:		.asciiz		"$t3 "
t4:		.asciiz		"$t4 "
t5:		.asciiz		"$t5 "
t6:		.asciiz		"$t6 "
t7:		.asciiz		"$t7 "
s0:		.asciiz		"$s0 "
s1:		.asciiz		"$s1 "
s2:		.asciiz		"$s2 "
s3:		.asciiz		"$s3 "
s4:		.asciiz		"$s4 "
s5:		.asciiz		"$s5 "
s6:		.asciiz		"$s6 "
s7:		.asciiz		"$s7 "
t8:		.asciiz		"$t8 "
t9:		.asciiz		"$t9 "
k0:		.asciiz		"$k0 "
k1:		.asciiz		"$k1 "
gp:		.asciiz		"$gp "
sp:		.asciiz		"$sp "
fp:		.asciiz		"$fp "
ra:		.asciiz		"$ra "

#########################################
## R instructions
## opcode  rs  rt  rd  shamt  funct
## 6       5   5   5   5      6
#########################################
## I instructions
## opcode  rs  rt  imm
## 6       5   5   16
#########################################
## J instructions
## opcode  addr
## 6       26
#########################################

# Máscara do campo addr
# 0000 0011 1111 1111 1111 1111 1111 1111
addr: .word 0x03FFFFFF # lw $s1 
		
# Máscara do campo rs
# 0000 0011 1110 0000 0000 0000 0000 0000
rs: .word 0x03E00000 # lw $s2
	
# Máscara do campo rt
# 0000 0000 0001 1111 0000 0000 0000 0000
rt: .word 0x001F0000 # lw $s3

# Máscara do campo rd
# 0000 0000 0000 0000 1111 1000 0000 0000
rd: .word 0x0000F800 # lw $s4
	
# Máscara do campo shant
# 0000 0000 0000 0000 0000 0111 1100 0000
shant: .word 0x000007C0 # lw $s5

# Máscara do campo funct
# 0000 0000 0000 0000 0000 0000 0011 1111
funct: .word 0x0000003F # lw $s6

# Máscara do campo funct
# 0000 0000 0000 0000 1111 1111 1111 1111
imm: .word 0x0000FFFF # lw $s7

# Referências utilizadas:
# https://en.wikipedia.org/wiki/MIPS_architecture
# http://www.mrc.uidaho.edu/mrc/people/jff/digital/MIPSir.html
# http://gec.di.uminho.pt/lesi/ac20203/AssemblyMIPS.pdf
