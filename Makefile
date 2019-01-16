# Para executar as receitas e construir o programa (padrão: modo debug) basta digitar
## make

# Para apagar todos os arquivos objeto temporários e programa final
## make clean

# Para utilizar configurações avançadas de trap de cálculos numéricos (infinity, nan, ...), construir no modo trap, basta digitar
## make -B mode=TRAP

# Para utilizar configurações avançadas de aceleração de cálculos numéricos, construir no modo fast, basta digitar
## make -B mode=FAST

### A flag -B é usada para forçar recompilar todos os arquivos do projeto no modo desejado

# Para compilar uma versão standalone que pode ser distribuída para outras máquinas sem a instalação da toolchain gfortran nelas, construir com a flag static=TRUE, basta digitar
## make static=TRUE

# Para (re-compilar e) rodar o programa
## make run
### vale para qualquer mode 

# Para (re-compilar e) rodar o programa no modo de verificação de memória
## make memcheck
### configurada para usar mode=DEBUG e static=FALSE

# Para (re-compilar e) rodar o programa no modo de debug step-by-step
## make debug
### configurada para usar mode=DEBUG e static=FALSE

# -------------------------------------------------------------------------------------------------------------------
# Arquivos e receitas do projeto:

## Receita para o programa final:
bin/main.elf: obj/main.o obj/module1.o
	make bin/version.txt
	$(LINKER) $(FLOPTS) $^ -o $@

## Receita para cada objeto:
obj/main.o: src/main.f90 obj/module1.o Makefile
	$(COMPILER) $(FCOPTS) -Jobj -c $< -o $@

obj/module1.o: src/module1.f90 Makefile
	$(COMPILER) $(FCOPTS) -Jobj -c $< -o $@

# -------------------------------------------------------------------------------------------------------------------
# Configurações avançadas do Makefile:

#Compiler and linker
COMPILER = gfortran
LINKER = gfortran

BASIC_OPTS = -cpp -fmax-errors=1 -ffree-line-length-0 -Wall -Wextra -fimplicit-none -g

DEBUG_OPTS = -O0 -fcheck=all
TRAP_OPTS = -ffpe-trap=invalid,zero,overflow,underflow,precision,denormal
FAST_OPTS = -O3 -march=native -m64 -Ofast

LINK_OPTS = -fbacktrace

.DEFAULT_GOAL := bin/main.elf

# -------------------------------------------------------------------------------------------------------------------
# public targets
.PHONY: .FORCE

clean: .FORCE
	rm -f bin/*.elf
	rm -f obj/*.o
	rm -f obj/*.mod
	rm -f bin/version.txt

run: bin/main.elf .FORCE
	./bin/main.elf

debug: .FORCE
	make bin/main.elf mode=SAFE static=FALSE 
	#cheat:
	# > start                                        <
	# > s (step)                                     <
	# > break 15                                     <
	# > c (continue)                                 <
	# > n (next)                                     <
	# > break module1.f90:12                         <
	# > p k (print (value of variable k))            <
	# > finish (step out)                            <
	# > q (quit)                                     <
	gdb bin/main.elf

memcheck: .FORCE
	make bin/main.elf mode=SAFE static=FALSE 
	valgrind --gen-suppressions=yes --leak-check=full --track-origins=yes bin/main.elf

bin/version.txt: .FORCE
	git log -1 --pretty=format:"commit %H%n" > bin/version.txt #hash
	git log -1 --pretty=format:"Date: %ad%n" >> bin/version.txt #date
	git status -sb >> bin/version.txt


# -------------------------------------------------------------------------------------------------------------------

mode ?= SAFE
static ?= FALSE
ifeq ($(mode),SAFE)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS)
else ifeq ($(mode), TRAP)
  FCOPTS = $(BASIC_OPTS) $(DEBUG_OPTS) $(TRAP_OPTS)
else ifeq ($(mode), FAST)
  FCOPTS = $(BASIC_OPTS) $(FAST_OPTS)
else
  #mode=SAFE or mode=FAST or mode=TRAP
endif

ifeq ($(static), TRUE)
  FLOPTS = $(LINK_OPTS) -static
else ifeq ($(static), FALSE)
  FLOPTS = $(LINK_OPTS)
else
  #static=TRUE or static=FALSE
endif

