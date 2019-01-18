module regres_mod
!
	use calcula_mod
!
	implicit none
!
	contains
! Rotina que faz a minimiza��o da fun�ao objetivo por Gauss Newton
!
	SUBROUTINE regres0(Niter,Rpd,LB,Ftol,ALtol,Plim,DP,IT,Fobj,Cov,Pred)

		! Modulo com as declara��es das vari�veis experimentais

		INTEGER i, k, m ! contadores
		INTEGER, INTENT(IN) :: Niter	! n�mero m�ximo de itera��es
		REAL(8), INTENT(IN) :: Rpd		! restri��o no passo
		REAL(8), INTENT(IN) :: LB		! par�metro de Law & Bailey
		REAL(8), INTENT(IN) :: Ftol		! toler�ncia da fun��o objetivo
		REAL(8), INTENT(IN) :: ALtol	! toler�ncia do passo
		REAL(8), INTENT(IN) :: Plim(Npar,2)	! faixa v�lida dos par�metros
		REAL(8), INTENT(IN) :: DP(Npar)			! perturba��o relativa nos par�metros
		INTEGER, INTENT(OUT) :: IT				! n�mero itera��es realizadas
		REAL(8), INTENT(OUT) :: Fobj			! valor da fun��o objetivo
		REAL(8), INTENT(OUT) :: Cov(Npar,Npar)	! matriz de covari�ncia dos par�metros
		type(M3_list), ALLOCATABLE, intent(out) :: Pred(:) !(Nexp,NVsai,Nvsai)! matriz de covari�ncia de predi��o
		type(M3_list), ALLOCATABLE :: DFP(:) !(Nexp,NVsai,Npar)! derivadas da sa�da do modelo em rela��o aos par�metros
		type(M2_list), ALLOCATABLE :: DYO(:) !(Nexp,NVsai)! desvios das vari�veis de sa�da
		REAL(8) T(Npar,Npar)			! aproxima��o da matriz Hessiana (par�metros)
		REAL(8) Tinv(Npar,Npar)			! inversa de T
		REAL(8) U(Npar)					! vetor com o gradiente da fun��o objetivo (par�metros)
		REAL(8) DelP(Npar)		! incremento nos par�metros
		type(M2_list), ALLOCATABLE :: FP(:) !(Nexp,NVsai)	! incremento na fun��o objetivo devido aos par�metros (modelo linear)
		REAL(8) FobjN	! Novo valor da fun��o objetivo
		REAL(8) DFL		! incremento total na fun��o objetivo considerando modelo linear
		REAL(8) DF		! delta da fun��o objetivo
		REAL(8) AL		! tamanho do passo
	
		character(200) :: format_, int_as_char*4
	
		ALLOCATE(Pred(nMOD))
		allocate(DYO(nMOD),FP(nMOD))
		do m = 1, nMOD
			ALLOCATE(Pred(m)%M(Nexp(m),NVsai(m),NVsai(m)))
			ALLOCATE(DYO(m)%M(Nexp(m),NVsai(m)))
			ALLOCATE(FP(m)%M(Nexp(m),NVsai(m)))
		end do
		
		! arquio de saida dos dados obtidos ao longo das itera��es
		OPEN (UNIT=10,FILE='output/saida.dat',STATUS='unknown')

		! iniciando as itera��es
		IT=0

		! chama rotina que avalia o modelo no ponto atual e calcula a fun��o objetivo
		CALL FunObj(Fobj)
		!print*, Y(:,:) ; pause !OK

		if (noreport.ne.1) write(*,*) '  ESTIMACAO DO MODELO '

		! inicia o processo iterativo de estima��o
		principal: DO 

			! escreve os resultados intermedi�rios
			if (noreport.ne.1) write(*,101) IT, Fobj
			WRITE(10,101) IT, Fobj
	!
			write(format_,'(I4.1)') Npar
			format_ = '(2x,'//'"Parametros =",'//trim(format_)//'(5x,E12.6))'
			if (noreport.ne.1) write(*,format_) Param(1:Npar)
			WRITE(10,format_) Param(1:Npar)
	!
			do m=1,nMOD
				write(format_,'(I4.1)') Nvsai(m)
				format_ = '(2x,'//'"Exp =",'//'I4,'//trim(format_)//'(3x,E12.6))'
				if (noreport.ne.1) write(*,*) ' Variaveis Dependentes (Y)' 
				WRITE(10,*) ' Variaveis Dependentes (Y)'
				DO k=1,Nexp(m)
					if (noreport.ne.1) write(*,format_) k, Y(m)%M(k,1:NVsai(m))
					WRITE(10,format_) k, Y(m)%M(k,1:NVsai(m))
				END DO
			end do

			! formatos de impress�o
			101 FORMAT(/,2x,'Iteracao =',I4,/,2x,'Funcao objetivo =',E12.6)

			! adiciona uma itera��o
			IT=IT+1

			! defini��o da  limita��o do passo
			AL=1.d0

			! c�lculo dos desvios das vari�veis
			do m = 1, nMOD
				DYO(m)%M(:,:)=Y(m)%M(:,:)-YM(m)%M(:,:)
			end do

			! chama rotina que calcula as derivadas no ponto atual
			do m = 1, nMOD
				CALL DerivP(DP,DFP)
			end do

			! c�lculo do vetor U[Npar]
			U=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					U = U + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DYO(m)%M(k,:)))
				END DO
			end do
		
			! c�lculo da matriz T[Npar,Npar]
			T=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					T = T + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DFP(m)%M(k,:,:)))
				END DO
			end do
	
			! c�lculo da inversa de T[Npar,Npar]
			CALL Inversa(Npar,T,Tinv)

			! c�lculo do incremento nos par�metros DelP[Npar]
			DelP = -MATMUL(Tinv,U)
	
			! CALCULO DA VARIA��O LINEARIZADA DA FUN��O OBJETIVO
			! c�lculo dos incrementos lineariados dos par�metros, FP[Nexp,NVsai]
			do m = 1, nMOD
				FP(m)%M(:,:)=0.d0
				DO k=1,Nexp(m)
					FP(m)%M(k,:)=MATMUL(DFP(m)%M(k,:,:),DelP(:))
				END DO
			end do

			!c�lculo da diferen�a da fun��o objetivo linearizada, DFL
			do m = 1, nMOD
				DFL=0.d0
				DO k=1,Nexp(m)
					DFL=DFL + DOT_PRODUCT(DYO(m)%M(k,:),MATMUL(EVYinv(m)%M(k,:,:),FP(m)%M(k,:)))
				END DO
			end do
		
			!corrige a dire��o da busca se necess�rio
			IF (DFL > 0.d0) THEN
				DFL=-DFL
				DelP=-DelP
			END IF

			! escreve os incrementos na vari�veis
			if (noreport.ne.1) write(*,05) DFL
			WRITE(10,05) DFL

			write(int_as_char,'(I4.1)') Npar
			format_ = '(2x,'//'"Variacao dos parametros =",'//trim(int_as_char)//'(2x,E12.6))'
			if (noreport.ne.1) write(*,format_) DelP(1:Npar)
			WRITE(10,format_) DelP(1:Npar)

			! formatos de impress�o
			05	FORMAT(1x,/,2x,'Variacao linear da funcao objetivo =',E12.6)

			!teste de consist�ncia f�sica nos par�metros
			CALL Limites(Npar, Plim, Param, DelP, Rpd, ALtol, AL)
	
			!corre��o dos valores das vari�veis de busca
			Param = Param + AL*DelP

			!verifica se chegou em um m�nimo
			CALL FunObj(FobjN)
			!print*, Y(:,:) ; pause !erro aqui
			IF ((FobjN < Ftol).OR.(DABS(FobjN-Fobj)/FobjN < Ftol))	EXIT principal ! busca acabou

			!verifica se h� converg�ncia para um m�nimo
			interno: DO

				! controle de converg�ncia
				if (noreport.ne.1) write(*,08) AL, FobjN
				WRITE(10,08) AL, FobjN
				08	FORMAT(2x,'Passo =',E12.6,5x,'Nova funcao objetivo =',E12.6)

				! calcula a diferen�a da fun��o objetivo
				DF=FobjN-Fobj
		
				!verifica se a converg�ncia para um m�nimo � caracterizada
				IF ((DF < 0.d0).AND.(DF-LB*(2.d0*AL-AL*AL)*DFL < 0.d0))	EXIT interno
	
				! se n�o for caracterizada a converg�ncia para o m�nimo
				!reduz-se o tamanho do passo e calcula-se os novos valores
				AL=AL/Rpd

				IF (AL < ALtol) CALL erro(3)

				Param = Param - (Rpd-1.d0)*AL*DelP

				! chama rotina que avalia o modelo no ponto atual e calcula a fun��o objetivo
				CALL FunObj(FobjN)
				!print*, Y(:,:) ; pause

			END DO interno

			! Atualiza o valor da fun��o objetivo
			! Obs: Param, X e Y j� est�o atualizados
			Fobj=FobjN

			!teste final de converg�ncia
!			IF (DABS(DF)/Fobj < Ftol) EXIT principal	! sai das itera��es com sucesso
			IF (IT >= Niter) CALL erro(4)				! verifica o n�mero de itera��es
	
		END DO principal

		! se sair direto ainda precia atualizar a fun��o objetivo
			Fobj=FobjN

		!####################### estimativa da vari�ncia experimental, 131114
		!Caso n�o se tenha vari�ncia experimental medida, estimamos ela a partir dos res�duos da estima��o
		!Refer�ncia 2005, Van de Geer, least square estimation
			if (FCT == 1) then !checa apenas exp 1 como gatilho para esse procedimento [!] !incluir tolerancia na compara��o ((EVY(1,1,1)-1.d0) < 1.d-8)?
	!			print*, 'executando estimativa da varia_ncia de medida de sai_da para ca_lculo da covaria_ncia dos para_metros a partir dos resi_duos da estimac_a_o'
	!			print*, Fobj; print*, EVY
				EVY(1)%M(:,1,1) = Fobj/dfloat(nexp-npar) !fazemos estimativa da vari�ncia a partir dos res�duos !livro Schwaab e Pinto, 2007, 1a_ed., t�pico 4.7
				EVYinv(1)%M(:,1,1) = EVY(1)%M(1,1,1)**(-1) !cada termo
				Fobj = Fobj*EVYinv(1)%M(1,1,1)
	!			print*, Fobj; print*, EVY
				!pause
			end if
		!agora com essa vari�ncia e fobj atualizada seguimos normalmente para calcular a variancia parametrica e intervalos de confian�a
		!####################### estimativa da vari�ncia experimental, 131114

		! C�lculo da matrizes de covari�ncia dos parametros e de predi��o

		! 1. c�lculo das derivadas
			CALL DerivP(DP,DFP)

		! 2. calculo das matrizes necess�rias
			! c�lculo da matriz T[Npar,Npar]
			T=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					T = T + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DFP(m)%M(k,:,:)))
				END DO
			end do
		
			! c�lculo da inversa de T[Npar,Npar]
			CALL Inversa(Npar,T,Tinv)

		! 3. Definindo a matriz de covari�ncia dos par�metros
			Cov = Tinv

		! 4. C�lculo da matriz de covariancia de predi��o
			do m = 1, nMOD
				DO k=1,Nexp(m)
					Pred(m)%M(k,:,:)=MATMUL(DFP(m)%M(k,:,:),MATMUL(Cov,TRANSPOSE(DFP(m)%M(k,:,:))))
				END DO
			end do

		! fechando arquivo de saida dos dados
		CLOSE (UNIT=10)

		do m = 1, nMOD
			deALLOCATE(DFP(m)%M)
		end do
		deallocate(DFP)

		RETURN
	END SUBROUTINE Regres0

	!
	! Rotina que avalia as derivadas do modelo com rela��o aos par�metros
	!
	SUBROUTINE DerivP(DP,DFP)
	!
	! Modulo com as declara��es das vari�veis experimentais
	!
	! Vari�veis de entrada e sa�da
	REAL(8), INTENT(IN) :: DP(Npar)		! perturba��es para o c�lculo das derivadas
	type(M3_list), ALLOCATABLE, intent(out) :: DFP(:) ! derivadas em rela��o a par�metros
	!
	! Vari�veis locais
	INTEGER :: i, k, m ! contadores
	REAL(8) :: Par(Npar)		! vetor auxiliar que cont�m os par�metros perturbados
	REAL(8), allocatable :: VarEnt(:)	! vari�veis de entrada de um experimento espec�fico
	REAL(8), allocatable :: VarSai(:)	! vari�veis de saida de um experimento espec�fico
	REAL(8) DeltaPar		! soma das perturba�oes absoluta e relativa para c�lculo das derivadas
	!
	! inicializando o vetor com os par�metros perturbados
	Par = Param
	!
	allocate(DFP(nMOD))
	do m = 1, nMOD
		ALLOCATE(DFP(m)%M(Nexp(m),NVsai(m),Npar))
	end do
	
	do m = 1, nMOD
		DO k=1,Nexp(m)
		allocate(varent(nvent(m)),varsai(nvsai(m)))
			! guada em VarEnt o valor das vari�veis independentes no experimento "k"
			VarEnt(:)=X(m)%M(k,:)
			!estimativa inicial para m�todo num�rico do modelo
			VarSai(:)=YM(m)%M(k,:)

			! deriva o modelo com rela��o a cada par�metro "i"
			DO i=1,Npar

				! Calcula a perturba��o no par�metro
				DeltaPar = DP(i)*DABS(Param(i)) + DP(i)

				! perturba o par�metro "i" e chama o modelo
				Par(i)=Param(i)+DeltaPar
				CALL lista_de_modelos(m)%modelo%resolve(VarEnt,VarSai,Par)

				! guarda em DFP o valor da vari�veis calculadas com o par�metro "i" perturbado
				DFP(m)%M(k,:,i)=VarSai(:)

				! perturba o par�metro "i" na dire��o oposta e chama o modelo
				Par(i)=Param(i)-DeltaPar
				CALL lista_de_modelos(m)%modelo%resolve(VarEnt,VarSai,Par)

				! calcula a derivada do modelo com rela��o ao par�metro "i"
				DFP(m)%M(k,:,i)=(DFP(m)%M(k,:,i)-VarSai(:))/(2.d0*DeltaPar)

				! retorna o par�metro "i" para o valor atual
				Par(i)=Param(i)
			END DO
		deallocate(varent,varsai)
		END DO
	end do
	RETURN
	END SUBROUTINE DerivP
	!
	! Rotina que calcula a inversa de uma matriz
	! Usa o m�todo de elimina��o de Gauss com pivota��o parcial
	!
	SUBROUTINE Inversa(ND,A,B)
	IMPLICIT NONE
	INTEGER i, j						! contadores
	INTEGER, INTENT(IN)  :: ND			! dimens�o das matrizes
	REAL(8), INTENT(IN)  :: A(ND,ND)	! matriz original
	REAL(8), INTENT(OUT) :: B(ND,ND)	! matriz invertida
	REAL(8) M(ND,2*ND), V(2*ND), aux	! vari�veis locais auxiliares
	!
	! Inicializando a matriz M <-- [A I]
		M = 0.d0
		M(1:ND,1:ND) = A
		FORALL (i=1:ND) M(i,i+ND) = 1.d0
	!
	! inicia o procedimento de invers�o
		DO i = 1,ND
			! pivota��o
			aux = M(i,i)
			DO j = i+1,ND
				IF (DABS(M(j,i)) <= DABS(aux)) CYCLE
				V(:) = M(j,:)
				M(j,:) = M(i,:)
				M(i,:) = V(:)
				aux = M(i,i)
			END DO
			!		
			! verifica a possibilidade da matriz ser n�o invers�vel
			IF (DABS(aux) < 1.d-10) WRITE(*,*) ' Possibilidade de matriz NAO inversivel'
			!
			! utiliza o m�todo de Gauss para a invers�o
			M(i,:) = M(i,:)/aux
			DO j = 1,ND
				IF (i == j) CYCLE
				aux = M(j,i)
				M(j,:) = M(j,:) - aux*M(i,:)
			END DO
		END DO
	!
	! Fornecendo a matriz inversa: M --> [I B]
		B = M(:,ND+1:2*ND)
	!
	RETURN
	END SUBROUTINE Inversa

end module regres_mod
