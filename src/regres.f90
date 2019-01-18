module regres_mod
!
	use calcula_mod
!
	implicit none
!
	contains
! Rotina que faz a minimização da funçao objetivo por Gauss Newton
!
	SUBROUTINE regres0(Niter,Rpd,LB,Ftol,ALtol,Plim,DP,IT,Fobj,Cov,Pred)

		! Modulo com as declarações das variáveis experimentais

		INTEGER i, k, m ! contadores
		INTEGER, INTENT(IN) :: Niter	! número máximo de iterações
		REAL(8), INTENT(IN) :: Rpd		! restrição no passo
		REAL(8), INTENT(IN) :: LB		! parâmetro de Law & Bailey
		REAL(8), INTENT(IN) :: Ftol		! tolerância da função objetivo
		REAL(8), INTENT(IN) :: ALtol	! tolerância do passo
		REAL(8), INTENT(IN) :: Plim(Npar,2)	! faixa válida dos parâmetros
		REAL(8), INTENT(IN) :: DP(Npar)			! perturbação relativa nos parâmetros
		INTEGER, INTENT(OUT) :: IT				! número iterações realizadas
		REAL(8), INTENT(OUT) :: Fobj			! valor da função objetivo
		REAL(8), INTENT(OUT) :: Cov(Npar,Npar)	! matriz de covariância dos parâmetros
		type(M3_list), ALLOCATABLE, intent(out) :: Pred(:) !(Nexp,NVsai,Nvsai)! matriz de covariância de predição
		type(M3_list), ALLOCATABLE :: DFP(:) !(Nexp,NVsai,Npar)! derivadas da saída do modelo em relação aos parâmetros
		type(M2_list), ALLOCATABLE :: DYO(:) !(Nexp,NVsai)! desvios das variáveis de saída
		REAL(8) T(Npar,Npar)			! aproximação da matriz Hessiana (parâmetros)
		REAL(8) Tinv(Npar,Npar)			! inversa de T
		REAL(8) U(Npar)					! vetor com o gradiente da função objetivo (parâmetros)
		REAL(8) DelP(Npar)		! incremento nos parâmetros
		type(M2_list), ALLOCATABLE :: FP(:) !(Nexp,NVsai)	! incremento na função objetivo devido aos parâmetros (modelo linear)
		REAL(8) FobjN	! Novo valor da função objetivo
		REAL(8) DFL		! incremento total na função objetivo considerando modelo linear
		REAL(8) DF		! delta da função objetivo
		REAL(8) AL		! tamanho do passo
	
		character(200) :: format_, int_as_char*4
	
		ALLOCATE(Pred(nMOD))
		allocate(DYO(nMOD),FP(nMOD))
		do m = 1, nMOD
			ALLOCATE(Pred(m)%M(Nexp(m),NVsai(m),NVsai(m)))
			ALLOCATE(DYO(m)%M(Nexp(m),NVsai(m)))
			ALLOCATE(FP(m)%M(Nexp(m),NVsai(m)))
		end do
		
		! arquio de saida dos dados obtidos ao longo das iterações
		OPEN (UNIT=10,FILE='output/saida.dat',STATUS='unknown')

		! iniciando as iterações
		IT=0

		! chama rotina que avalia o modelo no ponto atual e calcula a função objetivo
		CALL FunObj(Fobj)
		!print*, Y(:,:) ; pause !OK

		if (noreport.ne.1) write(*,*) '  ESTIMACAO DO MODELO '

		! inicia o processo iterativo de estimação
		principal: DO 

			! escreve os resultados intermediários
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

			! formatos de impressão
			101 FORMAT(/,2x,'Iteracao =',I4,/,2x,'Funcao objetivo =',E12.6)

			! adiciona uma iteração
			IT=IT+1

			! definição da  limitação do passo
			AL=1.d0

			! cálculo dos desvios das variáveis
			do m = 1, nMOD
				DYO(m)%M(:,:)=Y(m)%M(:,:)-YM(m)%M(:,:)
			end do

			! chama rotina que calcula as derivadas no ponto atual
			do m = 1, nMOD
				CALL DerivP(DP,DFP)
			end do

			! cálculo do vetor U[Npar]
			U=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					U = U + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DYO(m)%M(k,:)))
				END DO
			end do
		
			! cálculo da matriz T[Npar,Npar]
			T=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					T = T + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DFP(m)%M(k,:,:)))
				END DO
			end do
	
			! cálculo da inversa de T[Npar,Npar]
			CALL Inversa(Npar,T,Tinv)

			! cálculo do incremento nos parâmetros DelP[Npar]
			DelP = -MATMUL(Tinv,U)
	
			! CALCULO DA VARIAÇÃO LINEARIZADA DA FUNÇÃO OBJETIVO
			! cálculo dos incrementos lineariados dos parâmetros, FP[Nexp,NVsai]
			do m = 1, nMOD
				FP(m)%M(:,:)=0.d0
				DO k=1,Nexp(m)
					FP(m)%M(k,:)=MATMUL(DFP(m)%M(k,:,:),DelP(:))
				END DO
			end do

			!cálculo da diferença da função objetivo linearizada, DFL
			do m = 1, nMOD
				DFL=0.d0
				DO k=1,Nexp(m)
					DFL=DFL + DOT_PRODUCT(DYO(m)%M(k,:),MATMUL(EVYinv(m)%M(k,:,:),FP(m)%M(k,:)))
				END DO
			end do
		
			!corrige a direção da busca se necessário
			IF (DFL > 0.d0) THEN
				DFL=-DFL
				DelP=-DelP
			END IF

			! escreve os incrementos na variáveis
			if (noreport.ne.1) write(*,05) DFL
			WRITE(10,05) DFL

			write(int_as_char,'(I4.1)') Npar
			format_ = '(2x,'//'"Variacao dos parametros =",'//trim(int_as_char)//'(2x,E12.6))'
			if (noreport.ne.1) write(*,format_) DelP(1:Npar)
			WRITE(10,format_) DelP(1:Npar)

			! formatos de impressão
			05	FORMAT(1x,/,2x,'Variacao linear da funcao objetivo =',E12.6)

			!teste de consistência física nos parâmetros
			CALL Limites(Npar, Plim, Param, DelP, Rpd, ALtol, AL)
	
			!correção dos valores das variáveis de busca
			Param = Param + AL*DelP

			!verifica se chegou em um mínimo
			CALL FunObj(FobjN)
			!print*, Y(:,:) ; pause !erro aqui
			IF ((FobjN < Ftol).OR.(DABS(FobjN-Fobj)/FobjN < Ftol))	EXIT principal ! busca acabou

			!verifica se há convergência para um mínimo
			interno: DO

				! controle de convergência
				if (noreport.ne.1) write(*,08) AL, FobjN
				WRITE(10,08) AL, FobjN
				08	FORMAT(2x,'Passo =',E12.6,5x,'Nova funcao objetivo =',E12.6)

				! calcula a diferença da função objetivo
				DF=FobjN-Fobj
		
				!verifica se a convergência para um mínimo é caracterizada
				IF ((DF < 0.d0).AND.(DF-LB*(2.d0*AL-AL*AL)*DFL < 0.d0))	EXIT interno
	
				! se não for caracterizada a convergência para o mínimo
				!reduz-se o tamanho do passo e calcula-se os novos valores
				AL=AL/Rpd

				IF (AL < ALtol) CALL erro(3)

				Param = Param - (Rpd-1.d0)*AL*DelP

				! chama rotina que avalia o modelo no ponto atual e calcula a função objetivo
				CALL FunObj(FobjN)
				!print*, Y(:,:) ; pause

			END DO interno

			! Atualiza o valor da função objetivo
			! Obs: Param, X e Y já estão atualizados
			Fobj=FobjN

			!teste final de convergência
!			IF (DABS(DF)/Fobj < Ftol) EXIT principal	! sai das iterações com sucesso
			IF (IT >= Niter) CALL erro(4)				! verifica o número de iterações
	
		END DO principal

		! se sair direto ainda precia atualizar a função objetivo
			Fobj=FobjN

		!####################### estimativa da variância experimental, 131114
		!Caso não se tenha variância experimental medida, estimamos ela a partir dos resíduos da estimação
		!Referência 2005, Van de Geer, least square estimation
			if (FCT == 1) then !checa apenas exp 1 como gatilho para esse procedimento [!] !incluir tolerancia na comparação ((EVY(1,1,1)-1.d0) < 1.d-8)?
	!			print*, 'executando estimativa da varia_ncia de medida de sai_da para ca_lculo da covaria_ncia dos para_metros a partir dos resi_duos da estimac_a_o'
	!			print*, Fobj; print*, EVY
				EVY(1)%M(:,1,1) = Fobj/dfloat(nexp-npar) !fazemos estimativa da variância a partir dos resíduos !livro Schwaab e Pinto, 2007, 1a_ed., tópico 4.7
				EVYinv(1)%M(:,1,1) = EVY(1)%M(1,1,1)**(-1) !cada termo
				Fobj = Fobj*EVYinv(1)%M(1,1,1)
	!			print*, Fobj; print*, EVY
				!pause
			end if
		!agora com essa variância e fobj atualizada seguimos normalmente para calcular a variancia parametrica e intervalos de confiança
		!####################### estimativa da variância experimental, 131114

		! Cálculo da matrizes de covariância dos parametros e de predição

		! 1. cálculo das derivadas
			CALL DerivP(DP,DFP)

		! 2. calculo das matrizes necessárias
			! cálculo da matriz T[Npar,Npar]
			T=0.d0
			do m = 1, nMOD
				DO k=1,Nexp(m)
					T = T + MATMUL(TRANSPOSE(DFP(m)%M(k,:,:)),MATMUL(EVYinv(m)%M(k,:,:),DFP(m)%M(k,:,:)))
				END DO
			end do
		
			! cálculo da inversa de T[Npar,Npar]
			CALL Inversa(Npar,T,Tinv)

		! 3. Definindo a matriz de covariância dos parâmetros
			Cov = Tinv

		! 4. Cálculo da matriz de covariancia de predição
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
	! Rotina que avalia as derivadas do modelo com relação aos parâmetros
	!
	SUBROUTINE DerivP(DP,DFP)
	!
	! Modulo com as declarações das variáveis experimentais
	!
	! Variáveis de entrada e saída
	REAL(8), INTENT(IN) :: DP(Npar)		! perturbações para o cálculo das derivadas
	type(M3_list), ALLOCATABLE, intent(out) :: DFP(:) ! derivadas em relação a parâmetros
	!
	! Variáveis locais
	INTEGER :: i, k, m ! contadores
	REAL(8) :: Par(Npar)		! vetor auxiliar que contém os parâmetros perturbados
	REAL(8), allocatable :: VarEnt(:)	! variáveis de entrada de um experimento específico
	REAL(8), allocatable :: VarSai(:)	! variáveis de saida de um experimento específico
	REAL(8) DeltaPar		! soma das perturbaçoes absoluta e relativa para cálculo das derivadas
	!
	! inicializando o vetor com os parâmetros perturbados
	Par = Param
	!
	allocate(DFP(nMOD))
	do m = 1, nMOD
		ALLOCATE(DFP(m)%M(Nexp(m),NVsai(m),Npar))
	end do
	
	do m = 1, nMOD
		DO k=1,Nexp(m)
		allocate(varent(nvent(m)),varsai(nvsai(m)))
			! guada em VarEnt o valor das variáveis independentes no experimento "k"
			VarEnt(:)=X(m)%M(k,:)
			!estimativa inicial para método numérico do modelo
			VarSai(:)=YM(m)%M(k,:)

			! deriva o modelo com relação a cada parâmetro "i"
			DO i=1,Npar

				! Calcula a perturbação no parâmetro
				DeltaPar = DP(i)*DABS(Param(i)) + DP(i)

				! perturba o parâmetro "i" e chama o modelo
				Par(i)=Param(i)+DeltaPar
				CALL lista_de_modelos(m)%modelo%resolve(VarEnt,VarSai,Par)

				! guarda em DFP o valor da variáveis calculadas com o parâmetro "i" perturbado
				DFP(m)%M(k,:,i)=VarSai(:)

				! perturba o parâmetro "i" na direção oposta e chama o modelo
				Par(i)=Param(i)-DeltaPar
				CALL lista_de_modelos(m)%modelo%resolve(VarEnt,VarSai,Par)

				! calcula a derivada do modelo com relação ao parâmetro "i"
				DFP(m)%M(k,:,i)=(DFP(m)%M(k,:,i)-VarSai(:))/(2.d0*DeltaPar)

				! retorna o parâmetro "i" para o valor atual
				Par(i)=Param(i)
			END DO
		deallocate(varent,varsai)
		END DO
	end do
	RETURN
	END SUBROUTINE DerivP
	!
	! Rotina que calcula a inversa de uma matriz
	! Usa o método de eliminação de Gauss com pivotação parcial
	!
	SUBROUTINE Inversa(ND,A,B)
	IMPLICIT NONE
	INTEGER i, j						! contadores
	INTEGER, INTENT(IN)  :: ND			! dimensão das matrizes
	REAL(8), INTENT(IN)  :: A(ND,ND)	! matriz original
	REAL(8), INTENT(OUT) :: B(ND,ND)	! matriz invertida
	REAL(8) M(ND,2*ND), V(2*ND), aux	! variáveis locais auxiliares
	!
	! Inicializando a matriz M <-- [A I]
		M = 0.d0
		M(1:ND,1:ND) = A
		FORALL (i=1:ND) M(i,i+ND) = 1.d0
	!
	! inicia o procedimento de inversão
		DO i = 1,ND
			! pivotação
			aux = M(i,i)
			DO j = i+1,ND
				IF (DABS(M(j,i)) <= DABS(aux)) CYCLE
				V(:) = M(j,:)
				M(j,:) = M(i,:)
				M(i,:) = V(:)
				aux = M(i,i)
			END DO
			!		
			! verifica a possibilidade da matriz ser não inversível
			IF (DABS(aux) < 1.d-10) WRITE(*,*) ' Possibilidade de matriz NAO inversivel'
			!
			! utiliza o método de Gauss para a inversão
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
