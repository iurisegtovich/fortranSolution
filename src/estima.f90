!
!		ESTIMAÇÃO DE PARÂMETROS
!
!  Função Objetivo: Máxima Verossimilhança sem correlação entre experimentos
!
!  Utiliza um método híbrido para a minimização da função objetivo. A minimzação
! inicia com o método do Enxame de Partículas (Kennedy & Eberhart, Proc IEEE
! International Conference on Neural Networks, Perth, Australia, pp. 1942-1948
! e a melhor solução encontrada é usada como estimativa inical para um método
! do tipo Gauss-Newton (Anderson et al.,AIChE J.,1978,24(1),20-29) com acelerador
! de Law e Bailey (Law & Bailey,Chem.Eng.Sci.,1963,18,189-202)
!
!  O algoritmo original foi desenvolvido por:
!	Noronha, F. B., Pinto, J. C., Monteiro, J. L.,	Lobão, M. W.,
!		Santos, T. J., (1993), ESTIMA - Um Pacote Computacional para
!		Estimação de Parâmetros e Projeto de Experimentos.
!		Guia de Usuários, PEQ/COPPE/UFRJ, Rio de Janeiro.
!
! A incorporação do método do Enxame de Particulas e conversão para fortran 90 foram realizada posteriormente.
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
		! Declaração das variáveis
		INTEGER :: i, j, k, l, m, ii, jj	! contadores

		INTEGER :: RegConf	! define se usa enxame para região de confiança
		INTEGER :: Niter	! número máximo de iterações
		INTEGER :: IT		! número iterações realizadas pelo Maxima
		INTEGER :: Npt		! número de partículas do enxame
		INTEGER :: NPreg	! número de pontos na região de confiança
		INTEGER :: status	! verifica se leitura do arquivo saida tudo chegou ao fim
		REAL(8) :: C1, C2	! valor das ponderações da contribuição individual e global
		REAL(8) :: Wo, Wf	! valor inicial e final do fator de inércia
		REAL(8) :: Rpd		! restrição no passo
		REAL(8) :: LB		! parâmetro de Law & Bailey
		REAL(8) :: alfa	! nível de confiança para testes estatísticos
		REAL(8) :: GL		! graus de liberdade
		integer :: GLint, ncol
		REAL(8) :: TS		! limite de confança de acordo com t-Student
		REAL(8) :: Ftol	! tolerância da função objetivo
		REAL(8) :: ALtol	! tolerância do passo
		REAL(8) :: Fobj	! valor da função objetivo
		REAL(8) :: F		! valor da função objetivo lida do arquivo saida tudo
		REAL(8) :: Freg	! valor da função objetivo limite para região de confiança

		REAL(8), ALLOCATABLE :: Plim(:,:)	! faixa válida dos parâmetros
		REAL(8), ALLOCATABLE :: DP(:)		! perturbação relativa e absoluta nos parâmetros
		REAL(8), ALLOCATABLE :: Cov(:,:)	! matriz de covariância dos parâmetros
		REAL(8), ALLOCATABLE :: Cor(:,:)	! matriz de correlação dos parâmetros
		REAL(8), ALLOCATABLE :: Sigma(:)	! vetor com os desvios padrões dos parâmetros

		type(M3_list), ALLOCATABLE :: Pred(:)	! matriz de covariância de predição
		REAL(8), ALLOCATABLE :: PP(:)		! valor dos parametros lidos do arquivo saidatudo
		REAL(8), ALLOCATABLE :: Pmin(:)		! valor mínimo dos parametros segundo enxame
		REAL(8), ALLOCATABLE :: Pmax(:)		! valor máximo dos parametros segundo enxame
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
		! dimensionando a matriz de covariancia de predição 		!SERÁ ALOCADA NA SUB DO REGRES
!		ALLOCATE(Pred(nMOD))
!		do m = 1, nMOD
!			ALLOCATE(Pred(m)%M(Nexp(m),NVsai(m),NVsai(m)))
!		end do
!		#!!! será necessário ou a sub aloca?
		
		OPEN(11,FILE='input/dadosbusca.dat',STATUS='old',ACTION='read')
		! abre arquivo com dados relativos a busca do mínimo da função objetivo
		READ(11,*) RegConf
		READ(11,*) Niter	! número máximo de iterações
		READ(11,*) Npt
		IF(Npt == 0) RegConf = 0
		READ(11,*) C1, C2
		READ(11,*) Wo, Wf
		READ(11,*) Rpd		! restrição ao passo
		READ(11,*) LB		! parâmetro de Law & Bayley
		READ(11,*) Ftol		! tolerância na função objetivo
		READ(11,*) ALtol	! tolerância no passo
		READ(11,*) alfa		! nível de confiança
		!
		! Abre arquivo onde serão salvos os pontos para a construção da região de confiança
		IF (RegConf == 1) OPEN (UNIT=13,FILE='output/RegConf.dat',STATUS='unknown',ACTION='write')

		!##### LEITURA DO DADOS BUSCA inicio pt3 modelos ###############################################################
	
		READ(11,*) Npar ! números de parâmetros do modelo
		!print*, Npar; pause
		!
		! alocando o vetor de parâmetros e as matrizes de covariância e de correlação
		ALLOCATE(Param(Npar),Sigma(Npar),COV(Npar,Npar),COR(Npar,Npar))
		! alocando as matrizes com a faixa válida e perturbação dos parametros
		ALLOCATE(Plim(Npar,2),DP(Npar))
		!
		! estimativa inicial dos parâmetros
		DO i=1,Npar
			READ(11,*) Param(i)
		END DO
		!print*, param; pause
		!
		! faixa válida para os parâmetros
		DO i=1,Npar
			READ(11,*) Plim(i,1), Plim(i,2)
		END DO
		!print*, Plim; pause
		!
		! perturbação nos parâmetros para o cálculo das derivadas
		DO i=1,Npar
			READ(11,*) DP(i)
		END DO
			!
		!##### LEITURA DO DADOS BUSCA fim###############################################################
			!
			! X são os valores experimentais XM
			X=XM
			!
			! calcula a inversa da matriz dos erros experimentais em Y
			do m = 1, nmod
				DO k=1,Nexp(m)
					CALL inversa(NVsai(m),EVY(m)%M(k,:,:),EVYinv(m)%M(k,:,:))
				END DO
			end do
			!
			! CHAMA A ROTINA DE REGRESSÃO
			! estimação de parâmetros
			IF (Npt /= 0) CALL Enxame0(RegConf,Niter,Npt,C1,C2,Wo,Wf,Plim,Fobj)
			
			CALL regres0(Niter,Rpd,LB,Ftol,ALtol,Plim,DP,IT,Fobj,Cov,Pred)
			!
				! cálculo do vetor de desvios padrões de cada parâmetro
			FORALL(i=1:Npar) Sigma(i) = DSQRT( Cov(i,i) )
			!
			! cálculo da matriz de correlação dos parâmetros
			FORALL(i=1:Npar,j=1:Npar) Cor(i,j)=	Cov(i,j)/DSQRT(Cov(i,i)*Cov(j,j))
			!
			!Variável para cálculo dos limites de confiança
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

			! Verifica se a região de confiança com os dados do Enxame de Partículas será feita
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
				! Seleção dos pontos para análise da região/intervalo de confiança
				Freg = Fobj * (1.d0 + DFLOAT(NPAR) / GL * DFIN(alfa, DFLOAT(NPAR), GL)) !pp358-361 do livro "Schwaab, M., Pinto, J. C., Análise de Dados Experimentais I"
				!
				! Abre arquivo com os dados do Enxame de Partículas
				OPEN (UNIT=12,FILE='output/Saida_tudo.dat',STATUS='old',ACTION='read')
				!
					! 
					! Aloca e inicializa vetores para os parâmetros
					ALLOCATE(PP(Npar),Pmin(Npar),Pmax(Npar))
					Pmax = Param	! valor máximo dos parametros na região de confiança
					Pmin = Param	! valor mínimo dos parametros na região de confiança
					NPreg =  0		! número de pontos na região de confiança
					!
					DO
						! Le o dado gerado pelo Enxame de Partículas
						
						READ (12,*,IOSTAT = status) k, k, PP(1:Npar), F, (((Yswarm(m)%M(ii,jj),ii=1,Nexp(m)),jj=1,Nvsai(m)),m=1,nMOD)
						!
						! confere se chegou ao final do arquivo
						IF(status /= 0) EXIT
						!

						if (FCT==1) F = F*EVYinv(1)%M(1,1,1) !Fobjs do enxame reescalonadas pela abordagem de inferencia do VarExp a partir dos resíduos

						!print*, Fobj, F, Freg
						! testa o ponto, se estiver na região de confiança guarda o valor
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
/,3x,'Numero de Variáveis de Entrada =',I4, &
/,3x,'Numero de Variáveis de Saida =',I4 &
)
			! ESCREVE OS RESULTADOS DA ESTIMAÇÃO DO MODELO
			!
			WRITE(20,111) 'RESULTADOS DA ESTIMAÇÃO DO MODELO '
			111 FORMAT(A)
			!
			! Número de iterações
			WRITE(20,222) IT
			222 FORMAT(1x,/,3x,'Número de Iterações = ',I3)
			!
			! Função objetivo
			WRITE(20,3) Fobj
			3 FORMAT(1x,/,3x,'Função Objetivo = ',E12.6)
			!
				! Parâmetros
				WRITE(20,4)
				4 FORMAT(1x,/,3x,'Parâmetros Estimados, Limites de Confiança e Desvio Padrão',/, &
22x,'Param',10x,'LimInf',10x,'LimSup',9x,'DesvPad')
				DO i=1,Npar
					WRITE(20,5) i, Param(i), Param(i) - TS*Sigma(i), Param(i) + TS*Sigma(i), Sigma(i)
				END DO
				5 FORMAT(3x,'Param(',I2.2,') =',4(4x,E12.6))
				!
				! compara limites do enxame com da elipse
				IF (RegConf == 1) THEN
					WRITE(20,6)
					6	FORMAT(1x,/,3x,'Limites de Confiança do Enxame e da Elipse',/, &
20x,'Param',7x,'LIelipse',6x,'LIenxame',6x,'LSelipse',6x,'LSenxame')
					DO i=1,Npar
						WRITE(20,7) i, Param(i), Param(i) - TS*Sigma(i), Pmin(i), Param(i) + TS*Sigma(i), Pmax(i)
					END DO
					7	FORMAT(3x,'Param(',I2.2,') =',5(2x,E12.6))
				END IF
				!
				! Matriz de covariância dos parâmetros
				WRITE(20,8)
				8	FORMAT(1x,/,3x,'Matriz de Covariância dos Parâmetros')
			
				write(int_as_char,'(I4.1)') Npar
				format_ = '('//'1x,'//trim(int_as_char)//'(T5,'//trim(int_as_char)//'(2x,E12.6),/)'//')'
				WRITE(20,format_) Cov(1:Npar,1:Npar)
				!
				! Matriz de correlação dos parâmetros
				WRITE(20,10)
				10	FORMAT(3x,'Matriz de Correlação dos Parâmetros')
				!
				write(int_as_char,'(I4.1)') Npar
				format_ = '('//'1x,'//trim(int_as_char)//'(T5,'//trim(int_as_char)//'(2x,E12.6),/)'//')'
				WRITE(20,format_) Cor(1:Npar,1:Npar)
		!
		! Variáveis independentes
		WRITE(20,13)
		13	FORMAT(1x,3x,'Variáveis independentes (XM)')
		
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') Nvent(m)
			format_ = '('//'1x,'//trim(int_as_char)//'(3x,E12.6)'//')'
			DO i=1,Nexp(m)
				WRITE(20,format_) XM(m)%M(i,1:NVent(m))
			END DO
		end do
		!
		! Variáveis dependentes
		WRITE(20,15)
		15 FORMAT(1x,/,3x,'Variáveis dependentes (YM e Y)')
		
		do m = 1, nMOD
			write(int_as_char,'(I4.1)') 2*Nvsai(m)
			format_ = '('//'1x,'//trim(int_as_char)//'(3x,E12.6)'//')'
			DO i=1,Nexp(m)
				WRITE(20,format_) YM(m)%M(i,1:NVsai(m)),Y(m)%M(i,1:NVsai(m))
			END DO
		end do
		!
		! Matriz de covariância de predição
		WRITE(20,20) 'Matriz de covariâcia experimental e de predição (componente de covariância de predição do modelo em função as covariancias dos parametros)'
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

		! arquivo para construção de graficos
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
	
			! desaloca as variáveis


			IF (RegConf == 1) DEALLOCATE(PP,Pmin,Pmax)
		!
		CLOSE(11)
		CLOSE(13)
		CLOSE(20)

		!
		! AVALIAÇÃO DOS MODELOS
		!
		! Escreve resultados das avaliações estatisticas
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
		! abrindo arquivo que contém os dados experimentais
	!	OPEN(UNIT=10,FILE='input/dadosexp.dat',STATUS='old',ACTION='read')
		OPEN(UNIT=10,FILE=trim(dadosexp_file),STATUS='old',ACTION='read')
			! leitura das dimensões do problema
			READ(10,*) !cabeçalho
			READ(10,*) FCT					! tipo de função objetivo
			!write(*,*) FCT
			ALLOCATE(XM(nMOD),X(nMOD))
			ALLOCATE(YM(nMOD),Y(nMOD),EVY(nMOD),EVYinv(nMOD))
			do m = 1, nMOD !valor dado no codigo fonte principal, que aloca os modelos
				READ(10,*) !cabeçalho
				READ(10,*) !iMOD
				READ(10,*) !cabeçalho
				IF (FCT == 1 .or. FCT == 2 .or. FCT == 3) then
				READ(10,*) Nexp(m), NVent(m), NVsai(m) ! número de experimentos, de variáveis de entrada e saída
				elseIF (FCT == 4) then
				READ(10,*) Nexp(m), NVent(m), NVsai(m)!, NRec(m), (iREC(m)%V(i),i=1,NRec(m)) ! número de experimentos, de variáveis de entrada e saída
				end if
				!write(*,*) Nexp, NVent, NVsai
				! alocando as variáveis de entrada e saída
				ALLOCATE(XM(m)%M(Nexp(m),NVent(m)),X(m)%M(Nexp(m),NVent(m)))
				ALLOCATE(YM(m)%M(Nexp(m),NVsai(m)),Y(m)%M(Nexp(m),NVsai(m)),EVY(m)%M(Nexp(m),NVsai(m),NVsai(m)),EVYinv(m)%M(Nexp(m),NVsai(m),NVsai(m)))
				! zerando as matrizes dos erros experimentais e suas inversas
				EVY(m)%M(:,:,:)    = 0.d0
				EVYinv(m)%M(:,:,:) = 0.d0
				! Leitura dos dados experimentais
				READ(10,*) !cabeçalho das variáveis
				SELECT CASE(FCT)
			!		
					CASE(1)		! mínimos quadrados, IC calculado 'a la statistica', apenas uma variavel de saída
						if (nMOD .NE. 1) then; print*, 'nMOD .NE. 1'; stop; end if
						if (nvsai(1) .NE. 1) then; print*, 'nvsai .NE. 1'; stop; end if
						!
						DO k=1,Nexp(1)
							read(10,*) (XM(1)%M(k,i),i=1,NVent(1)), YM(1)%M(k,i) !método com loop implicito
						END DO
						FORALL(k=1:Nexp(m),i=1:NVsai(m)) EVY(1)%M(k,i,i) = 1.d0
					!
					CASE(2)		! mínimos quadrados ponderados
						DO k=1,Nexp(m)
							read(10,*) (XM(m)%M(k,i),i=1,NVent(m)), (YM(m)%M(k,i),EVY(m)%M(k,i,i),i=1,NVsai(m)) !método com loop implicito
!							print*, nexp(1), xm(m)%M(k,1), k, ym(m)%M(k,1)
						END DO
					!
					CASE(3)		! correlação entre erros experimentais de variáveis de saída de dado experimento
						DO k=1,Nexp(m)
							DO i=1,NVent(m)
								READ(10,*) XM(m)%M(k,i)
							END DO
							DO i=1,NVsai(m)
								READ(10,*) YM(m)%M(k,i), EVY(m)%M(k,i,1:NVsai(m))
							END DO
						END DO
					CASE(4)
						print*, 'caso FCT==4 não implementado'
						stop
						DO k=1,Nexp(m)
!							read(10,*) (XM(m)%M(k,i),EVX(m)%M(k,i,i),i=1,NVent(m)), (YM(m)%M(k,i),EVY(m)%M(k,i,i),i=1,NVsai(m)) !método com loop implicito
						END DO
				END SELECT
			end do
		!
		CLOSE(UNIT=10)
		!
		RETURN
	END SUBROUTINE leitura
	
end module estima_mod
