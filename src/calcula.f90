module calcula_mod
!
use modelos_mod
!
!	Rotina que avalia o modelo em cada condição experimental
! e calcula o valor da função objetivo
!	A Função Objetivo é de Maxima-verossimilhança considerando 
! que os experimentos são realizados de forma independente
!
	real(8), intrinsic :: DFLOAT
!
contains

	SUBROUTINE FunObj(Fobj)

	! Modulo com as declarações das variáveis experimentais
	INTEGER :: k, m ! contador
	REAL(8), INTENT(OUT) :: Fobj ! Valor da função objetivo
	REAL(8), allocatable :: VarEnt(:) ! variáveis de entrada de um experimento específico
	REAL(8), allocatable :: VarSai(:) ! variáveis de saida de um experimento específico
	type(M2_list), ALLOCATABLE :: EY(:) !(Nexp,NVsai)! desvios das variáveis de saída

	! avalia o modelo para cada experimento "k"
	do m = 1, nMOD
	allocate(varent(nvent(m)), varsai(nvsai(m)))
		DO k=1,Nexp(m)
			! guada em VarEnt o valor das variáveis independentes no experimento "k"
			VarEnt(:)=X(m)%M(k,:)
			!estimativa inicial para método numérico do modelo
			VarSai(:)=YM(m)%M(k,:)
			! chama o modelo
			CALL lista_de_modelos(m)%modelo%resolve(VarEnt,VarSai,Param)
			! guarda em Y o valor das variáveis dependentes
			Y(m)%M(k,:)=VarSai(:)
		END DO
	deallocate(varent, varsai)
	end do
	!
	allocate(EY(nMOD))
	! cálculo dos desvios das variáveis dependentes
	do m = 1, nMOD
		allocate(EY(m)%M(Nexp(m),Nvsai(m)))
		EY(m)%M(:,:) = Y(m)%M(:,:) - YM(m)%M(:,:)
	end do
	!
	! cálculo da função objetivo
	Fobj = 0.d0
	!
	do m = 1, nMOD
		DO k=1,Nexp(m)
			Fobj = Fobj + DOT_PRODUCT(EY(m)%M(k,:),MATMUL(EVYinv(m)%M(k,:,:),EY(m)%M(k,:)))
		END DO
	end do
	
	do m = 1, nMOD
		deallocate(EY(m)%M)
	end do
	deallocate(EY)
	
	RETURN
	END SUBROUTINE Funobj

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!
	! Rotina que faz teste dos limites das variáveis de entrada e dos parâmetros 
	!
	SUBROUTINE Limites(Ndim, Lim, Var, DelVar, Rpd, ALtol, AL)

	IMPLICIT NONE

	INTEGER i, Ndim
	REAL(8) ALtol, AL, Aux, Rpd
	REAL(8) Lim(Ndim,2), Var(Ndim), DelVar(Ndim)

		i = 1
		DO
			Aux = Var(i) + AL*DelVar(i)
			IF((Aux > Lim(i,1)) .AND. (Aux < Lim(i,2))) THEN
				i = i + 1
			ELSE
				AL = AL/Rpd
				IF (AL < ALtol) CALL erro(2)
			END IF
			IF (i > Ndim) EXIT
		END DO

	RETURN
	END SUBROUTINE Limites

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	!
	! Rotina que retorna mensagens de erro e interrompe a estimação
	!
	SUBROUTINE Erro(N)
	IMPLICIT NONE
	INTEGER N

	! seleciona o tipo de erro
	SELECT CASE (N)
		!Erro 1: 
		CASE (1)
			WRITE(*,*) 'DELX muito grande: A faixa das variaveis independentes esta sendo ultrapassada'
		!Erro 2: 
		CASE (2)
			WRITE(*,*) 'DELP muito grande: A faixa dos parametros esta sendo ultrapassada'
		!Erro 3: 
		CASE (3)
			WRITE(*,*) 'Nao eh caracterizada a convergencia para um minimo: aproximacao linear eh ruim'
		!Erro 4: 
		CASE (4)
			WRITE(*,*) 'O numero maximo de iteracoes foi excedido'

		CASE DEFAULT
			WRITE(*,*) 'ERRO'
	END SELECT
	!pause
	STOP
	END SUBROUTINE Erro
end module calcula_mod
