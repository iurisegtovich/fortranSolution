!
!		ESTIMA��O DE PAR�METROS
!
!  Fun��o Objetivo: M�xima Verossimilhan�a sem correla��o entre experimentos
!
!  Utiliza um m�todo h�brido para a minimiza��o da fun��o objetivo. A minimza��o
! inicia com o m�todo do Enxame de Part�culas (Kennedy & Eberhart, Proc IEEE
! International Conference on Neural Networks, Perth, Australia, pp. 1942-1948
! e a melhor solu��o encontrada � usada como estimativa inical para um m�todo
! do tipo Gauss-Newton (Anderson et al.,AIChE J.,1978,24(1),20-29) com acelerador
! de Law e Bailey (Law & Bailey,Chem.Eng.Sci.,1963,18,189-202)
!
!  O algoritmo original foi desenvolvido por:
!	Noronha, F. B., Pinto, J. C., Monteiro, J. L.,	Lob�o, M. W.,
!		Santos, T. J., (1993), ESTIMA - Um Pacote Computacional para
!		Estima��o de Par�metros e Projeto de Experimentos.
!		Guia de Usu�rios, PEQ/COPPE/UFRJ, Rio de Janeiro.
!
! A incorpora��o do m�todo do Enxame de Particulas e convers�o para fortran 90 foram realizada posteriormente.
!
module estima_mod
	!
	use regres_mod
	use enxame_mod
	!
	implicit none
	!
  INTERFACE
    REAL(c_double) FUNCTION DCHIIN(a,b) BIND(C,NAME='gsl_cdf_chisq_Pinv')
      USE,INTRINSIC :: iso_c_binding
      REAL(c_double),VALUE :: a,b
    END FUNCTION DCHIIN
  END INTERFACE
!
!=============================================================================================================================
!
  INTERFACE
    REAL(c_double) FUNCTION DCHIDF(a,b) BIND(C,NAME='gsl_cdf_chisq_P')
      USE,INTRINSIC :: iso_c_binding
      REAL(c_double),VALUE :: a,b
    END FUNCTION DCHIDF
  END INTERFACE
!
!=============================================================================================================================
!
  INTERFACE
    REAL(c_double) FUNCTION DTIN(a,b) BIND(C,NAME='gsl_cdf_tdist_Pinv')
      USE,INTRINSIC :: iso_c_binding
      REAL(c_double),VALUE :: a,b
    END FUNCTION DTIN
  END INTERFACE
!
!=============================================================================================================================
!
  INTERFACE
    REAL(c_double) FUNCTION DFIN(a,b,c) BIND(C,NAME='gsl_cdf_fdist_P')
      USE,INTRINSIC :: iso_c_binding
      REAL(c_double),VALUE :: a,b,c
    END FUNCTION DFIN
END INTERFACE
!
	contains
!
	subroutine estima
		!
		! Declara��o das vari�veis
		INTEGER :: i, j, k, l, m, ii, jj	! contadores

		INTEGER :: RegConf	! define se usa enxame para regi�o de confian�a
		INTEGER :: Niter	! n�mero m�ximo de itera��es
		INTEGER :: IT		! n�mero itera��es realizadas pelo Maxima
		INTEGER :: Npt		! n�mero de part�culas do enxame
		INTEGER :: NPreg	! n�mero de pontos na regi�o de confian�a
		INTEGER :: status	! verifica se leitura do arquivo saida tudo chegou ao fim
		REAL(8) :: C1, C2	! valor das pondera��es da contribui��o individual e global
		REAL(8) :: Wo, Wf	! valor inicial e final do fator de in�rcia
		REAL(8) :: Rpd		! restri��o no passo
		REAL(8) :: LB		! par�metro de Law & Bailey
		REAL(8) :: alfa	! n�vel de confian�a para testes estat�sticos
		REAL(8) :: GL		! graus de liberdade
		integer :: GLint, ncol
		REAL(8) :: TS		! limite de confan�a de acordo com t-Student
		REAL(8) :: Ftol	! toler�ncia da fun��o objetivo
		REAL(8) :: ALtol	! toler�ncia do passo
		REAL(8) :: Fobj	! valor da fun��o objetivo
		REAL(8) :: F		! valor da fun��o objetivo lida do arquivo saida tudo
		REAL(8) :: Freg	! valor da fun��o objetivo limite para regi�o de confian�a

		REAL(8), ALLOCATABLE :: Plim(:,:)	! faixa v�lida dos par�metros
		REAL(8), ALLOCATABLE :: DP(:)		! perturba��o relativa e absoluta nos par�metros
		REAL(8), ALLOCATABLE :: Cov(:,:)	! matriz de covari�ncia dos par�metros
		REAL(8), ALLOCATABLE :: Cor(:,:)	! matriz de correla��o dos par�metros
		REAL(8), ALLOCATABLE :: Sigma(:)	! vetor com os desvios padr�es dos par�metros

		type(M3_list), ALLOCATABLE :: Pred(:)	! matriz de covari�ncia de predi��o
		REAL(8), ALLOCATABLE :: PP(:)		! valor dos parametros lidos do arquivo saidatudo
		REAL(8), ALLOCATABLE :: Pmin(:)		! valor m�nimo dos parametros segundo enxame
		REAL(8), ALLOCATABLE :: Pmax(:)		! valor m�ximo dos parametros segundo enxame
	!
		REAL(8), ALLOCATABLE :: Pabs		! probabilidade absoluta de cada modelo
		REAL(8), ALLOCATABLE :: Chi2, chi2min		! valor do chi-quadrado para um dado alfa e GL
	
		type(M2_list), ALLOCATABLE :: Yswarm(:)
		!
		character(200) :: format_, int_as_char*4
		integer :: parametros 
		
		allocate(Nvent(nMOD), Nvsai(nMOD), Nexp(nMOD))
		
		! chama rotina que faz a leitura dos dados experimentais
		CALL Leitura
		!pause
		! dimensionando a matriz de covariancia de predi��o 		!SER� ALOCADA NA SUB DO REGRES
!		ALLOCATE(Pred(nMOD))
!		do m = 1, nMOD
!			ALLOCATE(Pred(m)%M(Nexp(m),NVsai(m),NVsai(m)))
!		end do
!		#!!! ser� necess�rio ou a sub aloca?
		
		OPEN(11,FILE='input/dadosbusca.dat',STATUS='old',ACTION='read')
		! abre arquivo com dados relativos a busca do m�nimo da fun��o objetivo
		READ(11,*) RegConf
		READ(11,*) Niter	! n�mero m�ximo de itera��es
		READ(11,*) Npt
		IF(Npt == 0) RegConf = 0
		READ(11,*) C1, C2
		READ(11,*) Wo, Wf
		READ(11,*) Rpd		! restri��o ao passo
		READ(11,*) LB		! par�metro de Law & Bayley
		READ(11,*) Ftol		! toler�ncia na fun��o objetivo
		READ(11,*) ALtol	! toler�ncia no passo
		READ(11,*) alfa		! n�vel de confian�a
		!
		! Abre arquivo onde ser�o salvos os pontos para a constru��o da regi�o de confian�a
		IF (RegConf == 1) OPEN (UNIT=13,FILE='output/RegConf.dat',STATUS='unknown',ACTION='write')

		!##### LEITURA DO DADOS BUSCA inicio pt3 modelos ###############################################################
	
		READ(11,*) Npar ! n�meros de par�metros do modelo
		!print*, Npar; pause
		!
		! alocando o vetor de par�metros e as matrizes de covari�ncia e de correla��o
		ALLOCATE(Param(Npar),Sigma(Npar),COV(Npar,Npar),COR(Npar,Npar))
		! alocando as matrizes com a faixa v�lida e perturba��o dos parametros
		ALLOCATE(Plim(Npar,2),DP(Npar))
		!
		! estimativa inicial dos par�metros
		DO i=1,Npar
			READ(11,*) Param(i)
		END DO
		!print*, param; pause
		!
		! faixa v�lida para os par�metros
		DO i=1,Npar
			READ(11,*) Plim(i,1), Plim(i,2)
		END DO
		!print*, Plim; pause
		!
		! perturba��o nos par�metros para o c�lculo das derivadas
		DO i=1,Npar
			READ(11,*) DP(i)
		END DO
			!
		!##### LEITURA DO DADOS BUSCA fim###############################################################
			!
			! X s�o os valores experimentais XM
			X=XM
			!
			! calcula a inversa da matriz dos erros experimentais em Y
			do m = 1, nmod
				DO k=1,Nexp(m)
					CALL inversa(NVsai(m),EVY(m)%M(k,:,:),EVYinv(m)%M(k,:,:))
				END DO
			end do
			!
			! CHAMA A ROTINA DE REGRESS�O
			! estima��o de par�metros
			IF (Npt /= 0) CALL Enxame0(RegConf,Niter,Npt,C1,C2,Wo,Wf,Plim,Fobj)
			
			CALL regres0(Niter,Rpd,LB,Ftol,ALtol,Plim,DP,IT,Fobj,Cov,Pred)
			!
				! c�lculo do vetor de desvios padr�es de cada par�metro
			FORALL(i=1:Npar) Sigma(i) = DSQRT( Cov(i,i) )
			!
			! c�lculo da matriz de correla��o dos par�metros
			FORALL(i=1:Npar,j=1:Npar) Cor(i,j)=	Cov(i,j)/DSQRT(Cov(i,i)*Cov(j,j))
			!
			!Vari�vel para c�lculo dos limites de confian�a
			GLint = 0
			do m = 1, nmod
				GLint = GLint + Nexp(m)*NVsai(m)
			end do
			GLint = GLint - Npar
			GL = DFLOAT(GLint)
			TS = DTIN( (1.d0+alfa)/2 , GL )
			Pabs = 1.d0 - DCHIDF(Fobj,GL)
			Chi2 = DCHIIN((1.d0+alfa)/2.d0,GL) !max
			Chi2min = DCHIIN((1.d0-alfa)/2.d0,GL) !max

			! Verifica se a regi�o de confian�a com os dados do Enxame de Part�culas ser� feita
			IF (RegConf == 1) THEN
				!
				print*, ' '; print*, ' '; print*, 'Gerando arquivo RegConf.dat'; print*, ' '; print*, ' '
		!
				do m = 1, nMOD
					write(int_as_char,'(I4.1)') Npar
					format_ = '('//trim(int_as_char)//'('//'"PP("'//',I4.1,'//'")"'//',A),2(A),'
					write(int_as_char,'(I4.1)') Nexp(m)*Nvsai(m)
					format_ = trim(format_)//trim(int_as_char)//'('//'"Y("'//',I4.1,'//'","'//',I4.1,'//'")"'//',A)'//')'
					WRITE (13,format_) (ii, char(9),ii=1,Npar), 'Fobj', char(9), ((ii,jj,char(9),ii=1,Nexp(m)),jj=1,Nvsai(m))
				end do
				!
				allocate(Yswarm(nMOD))
				do m = 1, nMOD
					allocate(Yswarm(m)%M(nexp(m),nvsai(m)))
				end do
				!
				! Sele��o dos pontos para an�lise da regi�o/intervalo de confian�a
				Freg = Fobj * (1.d0 + DFLOAT(NPAR) / GL * DFIN(alfa, DFLOAT(NPAR), GL)) !pp358-361 do livro "Schwaab, M., Pinto, J. C., An�lise de Dados Experimentais I"
				!
				! Abre arquivo com os dados do Enxame de Part�culas
				OPEN (UNIT=12,FILE='output/Saida_tudo.dat',STATUS='old',ACTION='read')
				!
					! 
					! Aloca e inicializa vetores para os par�metros
					ALLOCATE(PP(Npar),Pmin(Npar),Pmax(Npar))
					Pmax = Param	! valor m�ximo dos parametros na regi�o de confian�a
					Pmin = Param	! valor m�nimo dos parametros na regi�o de confian�a
					NPreg =  0		! n�mero de pontos na regi�o de confian�a
					!
					DO
						! Le o dado gerado pelo Enxame de Part�culas
						
						READ (12,*,IOSTAT = status) k, k, PP(1:Npar), F, (((Yswarm(m)%M(ii,jj),ii=1,Nexp(m)),jj=1,Nvsai(m)),m=1,nMOD)
						!
						! confere se chegou ao final do arquivo
						IF(status /= 0) EXIT
						!

						if (FCT==1) F = F*EVYinv(1)%M(1,1,1) !Fobjs do enxame reescalonadas pela abordagem de inferencia do VarExp a partir dos res�duos

						!print*, Fobj, F, Freg
						! testa o ponto, se estiver na regi�o de confian�a guarda o valor
						IF (F < Freg) THEN

						ncol = 0
						do m = 1, nMOD
							ncol = ncol + Nexp(m)*Nvsai(m)
						end do
						ncol = ncol + npar + 1
						write(int_as_char,'(I4.1)') ncol
						format_ = '('//trim(int_as_char)//'(E12.6,A)'//')'
						WRITE (13,format_) (PP(ii), char(9),ii=1,Npar), F, char(9), (((Yswarm(m)%M(ii,jj),ii=1,Nexp(m)),jj=1,Nvsai(m)),m=1,nMOD)
					
						FORALL(j=1:Npar, PP(j) > Pmax(j)) Pmax(j) = PP(j)
						FORALL(j=1:Npar, PP(j) < Pmin(j)) Pmin(j) = PP(j)
						NPreg = NPreg + 1
						END IF
					END DO
					!
					if (noreport.ne.1) write(*,*) 'Numero de pontos na RC ', NPreg, Freg
					!
				CLOSE(UNIT=12)
				deallocate(Yswarm)
			END IF
			!
			! abre arquivo de saida dos resultados
			OPEN(UNIT=20,FILE='output/relatorio.dat',STATUS='unknown',ACTION='write')
			! Inicia o arquivo com a saida dos resultados de cada modelo
			WRITE(20,101) Nexp, NVent, NVsai
101 FORMAT( /,5x,'RESULTADOS DA ESTIMACAO',/, &
/,3x,'Numero de Experimentos =',I4, &
/,3x,'Numero de Vari�veis de Entrada =',I4, &
/,3x,'Numero de Vari�veis de Saida =',I4 &
)
			! ESCREVE OS RESULTADOS DA ESTIMA��O DO MODELO
			!
			WRITE(20,111) 'RESULTADOS DA ESTIMA��O DO MODELO '
			111 FORMAT(A)
			!
			! N�mero de itera��es
			WRITE(20,222) IT
			222 FORMAT(1x,/,3x,'N�mero de Itera��es = ',I3)
			!
			! Fun��o objetivo
			WRITE(20,3) Fobj
			3 FORMAT(1x,/,3x,'Fun��o Objetivo = ',E12.6)
			!
				! Par�metros
				WRITE(20,4)
				4 FORMAT(1x,/,3x,'Par�metros Estimados, Limites de Confian�a e Desvio Padr�o',/, &
22x,'Param',10x,'LimInf',10x,'LimSup',9x,'DesvPad')
				DO i=1,Npar
					WRITE(20,5) i, Param(i), Param(i) - TS*Sigma(i), Param(i) + TS*Sigma(i), Sigma(i)
				END DO
				5 FORMAT(3x,'Param(',I2.2,') =',4(4x,E12.6))
				!
				! compara limites do enxame com da elipse
				IF (RegConf == 1) THEN
					WRITE(20,6)
					6	FORMAT(1x,/,3x,'Limites de Confian�a do Enxame e da Elipse',/, &
20x,'Param',7x,'LIelipse',6x,'LIenxame',6x,'LSelipse',6x,'LSenxame')
					DO i=1,Npar
						WRITE(20,7) i, Param(i), Param(i) - TS*Sigma(i), Pmin(i), Param(i) + TS*Sigma(i), Pmax(i)
					END DO
					7	FORMAT(3x,'Param(',I2.2,') =',5(2x,E12.6))
				END IF
				!
				! Matriz de covari�ncia dos par�metros
				WRITE(20,8)
				8	FORMAT(1x,/,3x,'Matriz de Covari�ncia dos Par�metros')
			
				write(int_as_char,'(I4.1)') Npar
				format_ = '('//'1x,'//trim(int_as_char)//'(T5,'//trim(int_as_char)//'(2x,E12.6),/)'//')'
				WRITE(20,format_) Cov(1:Npar,1:Npar)
				!
				! Matriz de correla��o dos par�metros
				WRITE(20,10)
				10	FORMAT(3x,'Matriz de Correla��o dos Par�metros')
				!
				write(int_as_char,'(I4.1)') Npar
				format_ = '('//'1x,'//trim(int_as_char)//'(T5,'//trim(int_as_char)//'(2x,E12.6),/)'//')'
				WRITE(20,format_) Cor(1:Npar,1:Npar)
		!
		! Vari�veis independentes
		WRITE(20,13)
		13	FORMAT(1x,3x,'Vari�veis independentes (XM)')
		
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') Nvent(m)
			format_ = '('//'1x,'//trim(int_as_char)//'(3x,E12.6)'//')'
			DO i=1,Nexp(m)
				WRITE(20,format_) XM(m)%M(i,1:NVent(m))
			END DO
		end do
		!
		! Vari�veis dependentes
		WRITE(20,15)
		15 FORMAT(1x,/,3x,'Vari�veis dependentes (YM e Y)')
		
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') 2*Nvsai(m)
			format_ = '('//'1x,'//trim(int_as_char)//'(3x,E12.6)'//')'
			DO i=1,Nexp(m)
				WRITE(20,format_) YM(m)%M(i,1:NVsai(m)),Y(m)%M(i,1:NVsai(m))
			END DO
		end do
		!
		! Matriz de covari�ncia de predi��o
		WRITE(20,20) 'Matriz de covari�cia experimental e de predi��o (componente de covari�ncia de predi��o do modelo em fun��o as covariancias dos parametros)'
		20	FORMAT(1x,/,3x,A)
		
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') Nvsai(m)
			format_ = '('//'1x,'//trim(int_as_char)//'(T5,'//trim(int_as_char)//'(2x,E12.6),/)'//')'
			DO k=1,Nexp(m)
				WRITE(20,21) m, k
				WRITE(20,*) 'EVY'
				WRITE(20,format_) EVY(m)%M(k,1:NVsai(m),1:NVsai(m))
				WRITE(20,*) 'PRED'
				WRITE(20,format_) Pred(m)%M(k,1:NVsai(m),1:NVsai(m))
			END DO
		end do
		
		21	FORMAT(4x, 'Mod', I2, 'Exp', I2)

			! Escreve os valores (cada linha corresponde a um experimento)

		! arquivo para constru��o de graficos
		OPEN (50,file='output/grafico.dat',status='unknown')
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') Nvent(m)
			format_ = '('//'1x,'//'"Exp,",'//trim(int_as_char)//'(7x,'//'"XM("'//'I2.2,'//'"),"'//'),'
			write(int_as_char,'(I4.1)') Nvsai(m)
			format_ = trim(format_)//trim(int_as_char)//'(7x,'//'"YM("'//',I2.2,'//'"),"'//',7x,'//'"YC("'//',I2.2,'//'"),"'//',7x,'//'"YL("'//',I2.2,'//'"),"'//',7x,'//'"YU("'//',I2.2,'//'"),"'//')'//')'
			WRITE(50,format_) (i, i=1,NVent(m)), (i,i,i,i, i=1,NVsai(m))
			write(int_as_char,'(I4.1)') NVent(m)+4*NVsai(m)
			format_ = '('//'2x,I4,'//'",",'//trim(int_as_char)//'(1x,E12.6,'//'","'//')'//')'
			DO i=1,Nexp(m)
				WRITE(50,format_) i,XM(m)%M(i,1:NVent(m)),(YM(m)%M(i,j), Y(m)%M(i,j),Y(m)%M(i,j)-TS*DSQRT(Pred(m)%M(i,j,j)), Y(m)%M(i,j)+TS*DSQRT(Pred(m)%M(i,j,j)), j=1,NVsai(m))
			END DO
		end do
		CLOSE(50)
		!
	
			! desaloca as vari�veis


			IF (RegConf == 1) DEALLOCATE(PP,Pmin,Pmax)
		!
		CLOSE(11)
		CLOSE(13)
		CLOSE(20)

		!
		! AVALIA��O DOS MODELOS
		!
		! Escreve resultados das avalia��es estatisticas
		OPEN(60,FILE='output/avaliacao.dat',STATUS='unknown',ACTION='write')
		!
		if (noreport.ne.1) WRITE(*,61)
		WRITE(60,61)
		if (noreport.ne.1) WRITE( *,62)
		WRITE(60,62)
	!
		if (noreport.ne.1) WRITE(*,64) Chi2min, Fobj, Chi2,  Pabs
		WRITE(60,64) Chi2min, Fobj, Chi2, Pabs
	!
		61	FORMAT(1x,/,5x,'AVALIACOES ESTATISTICAS DOS MODELOS')
		62	FORMAT(1x,/,1x,'Chi2min   Fobj       Chi2      Pabs    ')
		64	FORMAT(1x,F7.4,1x,E10.4,2(1x,F7.4))
		!
		CLOSE(UNIT=60)

		open(newunit=parametros,file=trim('input/parametros'),status='replace',action='write')
		do i=1,npar
				write(parametros,*) param(i)
		end do
		close(parametros)
		
		DEALLOCATE(Param,Plim,DP,Sigma,COV,COR) !npar
		do m = 1, nMOD
			DEALLOCATE(XM(m)%M,X(m)%M)
			DEALLOCATE(YM(m)%M,Y(m)%M,EVY(m)%M,EVYinv(m)%M)
		end do
		DEALLOCATE(XM,X)
		DEALLOCATE(YM,Y,EVY,EVYinv)

		!pause
		!STOP
	end subroutine estima
	
		SUBROUTINE Leitura !dadosexp
		!
		INTEGER :: k, i, j, m ! contadores
		!
		! abrindo arquivo que cont�m os dados experimentais
	!	OPEN(UNIT=10,FILE='input/dadosexp.dat',STATUS='old',ACTION='read')
		OPEN(UNIT=10,FILE=trim(dadosexp_file),STATUS='old',ACTION='read')
			! leitura das dimens�es do problema
			READ(10,*) !cabe�alho
			READ(10,*) FCT					! tipo de fun��o objetivo
			!write(*,*) FCT
			ALLOCATE(XM(nMOD),X(nMOD))
			ALLOCATE(YM(nMOD),Y(nMOD),EVY(nMOD),EVYinv(nMOD))
			do m = 1, nMOD !valor dado no codigo fonte principal, que aloca os modelos
				READ(10,*) !cabe�alho
				READ(10,*) !iMOD
				READ(10,*) !cabe�alho
				IF (FCT == 1 .or. FCT == 2 .or. FCT == 3) then
				READ(10,*) Nexp(m), NVent(m), NVsai(m) ! n�mero de experimentos, de vari�veis de entrada e sa�da
				elseIF (FCT == 4) then
				READ(10,*) Nexp(m), NVent(m), NVsai(m)!, NRec(m), (iREC(m)%V(i),i=1,NRec(m)) ! n�mero de experimentos, de vari�veis de entrada e sa�da
				end if
				!write(*,*) Nexp, NVent, NVsai
				! alocando as vari�veis de entrada e sa�da
				ALLOCATE(XM(m)%M(Nexp(m),NVent(m)),X(m)%M(Nexp(m),NVent(m)))
				ALLOCATE(YM(m)%M(Nexp(m),NVsai(m)),Y(m)%M(Nexp(m),NVsai(m)),EVY(m)%M(Nexp(m),NVsai(m),NVsai(m)),EVYinv(m)%M(Nexp(m),NVsai(m),NVsai(m)))
				! zerando as matrizes dos erros experimentais e suas inversas
				EVY(m)%M(:,:,:)    = 0.d0
				EVYinv(m)%M(:,:,:) = 0.d0
				! Leitura dos dados experimentais
				READ(10,*) !cabe�alho das vari�veis
				SELECT CASE(FCT)
			!		
					CASE(1)		! m�nimos quadrados, IC calculado 'a la statistica', apenas uma variavel de sa�da
						if (nMOD .NE. 1) then; print*, 'nMOD .NE. 1'; stop; end if
						if (nvsai(1) .NE. 1) then; print*, 'nvsai .NE. 1'; stop; end if
						!
						DO k=1,Nexp(1)
							read(10,*) (XM(1)%M(k,i),i=1,NVent(1)), YM(1)%M(k,i) !m�todo com loop implicito
						END DO
						FORALL(k=1:Nexp(m),i=1:NVsai(m)) EVY(1)%M(k,i,i) = 1.d0
					!
					CASE(2)		! m�nimos quadrados ponderados
						DO k=1,Nexp(m)
							read(10,*) (XM(m)%M(k,i),i=1,NVent(m)), (YM(m)%M(k,i),EVY(m)%M(k,i,i),i=1,NVsai(m)) !m�todo com loop implicito
!							print*, nexp(1), xm(m)%M(k,1), k, ym(m)%M(k,1)
						END DO
					!
					CASE(3)		! correla��o entre erros experimentais de vari�veis de sa�da de dado experimento
						DO k=1,Nexp(m)
							DO i=1,NVent(m)
								READ(10,*) XM(m)%M(k,i)
							END DO
							DO i=1,NVsai(m)
								READ(10,*) YM(m)%M(k,i), EVY(m)%M(k,i,1:NVsai(m))
							END DO
						END DO
					CASE(4)
						print*, 'caso FCT==4 n�o implementado'
						stop
						DO k=1,Nexp(m)
!							read(10,*) (XM(m)%M(k,i),EVX(m)%M(k,i,i),i=1,NVent(m)), (YM(m)%M(k,i),EVY(m)%M(k,i,i),i=1,NVsai(m)) !m�todo com loop implicito
						END DO
				END SELECT
			end do
		!
		CLOSE(UNIT=10)
		!
		RETURN
	END SUBROUTINE leitura
	
end module estima_mod
