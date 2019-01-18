module enxame_mod
!
	use calcula_mod
	use ziggurat_mod
!
	implicit none
!
	contains
	!
	! Rotina que faz a minimização da funçao objetivo
	! Só otimiza os parâmetros
	!
	!  Utiliza o método do Enxame de Particulas (Kennedy e Eberhart, 1995, 
	! In: Proc. IEEE International Conference on Neural Networks, Perth, Australia, pp. 1942-1948)
	!  Realiza uma busca global, o que aumenta a probabilidade de encontrar o mínimo global, 
	! não utiliza derivadas e não precisa de uma estimativa inicial dos parâmetros.
	!
	SUBROUTINE Enxame0(RegConf,Niter,Npt,C1,C2,Wo,Wf,Plim,Fobj)

	INTEGER i, j, ii, jj, m ! contadores
	integer ncol
	INTEGER IT		! contador das iterações
	INTEGER pos(1)	! posição do menor valor em um vetor
	INTEGER, INTENT(IN) :: RegConf	! define se usa enxame para região de confiança
	INTEGER, INTENT(IN) :: Npt			! número de partículas do enxame
	INTEGER, INTENT(IN) :: Niter		! número de iterações 
	REAL(8), INTENT(IN) :: Wo, Wf		! valor inicial e final do fator de inércia
	REAL(8), INTENT(IN) :: C1, C2		! valor das ponderações da contribuição individual e global
	REAL(8), INTENT(IN) :: Plim(Npar,2)	! limites inferior e supeior dos parâmetros
	REAL(8) W				! valor do fator de inércia ao longo da busca
	REAL(8) P(Npt,Npar)		! matriz com os valores dos parâmetros de cada partícula
	REAL(8) Pvel(Npt,Npar)	! matriz com os valores das velocidades de cada partícula
	REAL(8) Vmax(Npar)		! vetor com as velocidades máximas em cada direção
	REAL(8) Ppt(Npt,Npar)	! matriz com os valores dos parâmetros ótimos de cada partícula
	REAL(8) Potm(Npar)		! vetor com os parâmetros ótimos de todo o conjunto de partículas
	REAL(8) F(Npt)			! vetor com os valores das funções objetivo de cada partícula
	REAL(8) Fpt(Npt)		! vetor com os valores ótimos das funções objetivo de cada partícula
	REAL(8) Fotm, oldFotm	! valor ótimo da função objetivo de todo o conjunto de partículas
	real(8) :: oldPotm(npar)
	REAL(8) Fobj			! valor final da função objetivo
	type(M3_list), allocatable :: Yp(:) !(Npt,Nexp,Nvsai) !variavel de saida de cada particula
	
	character(200) :: format_, int_as_char*4
	character(100) :: paused
		integer, intrinsic :: TIME
!	print*, 'enxame:', __FILE__, __LINE__
	! Inicializando as variáveis
	Fotm = 1.d100
	Fpt = 1.d100
	allocate(Yp(nMOD))
	do m = 1, nMOD
		allocate(Yp(m)%M(Npt,Nexp(m),Nvsai(m)))
	end do

	! inicialização do gerador de números aleatórios
	if (hasseed.ne.1) then
		CALL zigset( TIME()**2 ); hasseed = 1 !chama semente e liga flag para não chamar de novo
	end if
	!
	! define a velocidade máxima
	Vmax = (Plim(:,2) - Plim(:,1))/2.d0

	! gerando enxame inicial e velocidades iniciais
	DO i=1,Npt
		DO j=1,Npar
			P(i,j) = Plim(j,1) + uni()*(Plim(j,2) - Plim(j,1))
			Pvel(i,j) = Vmax(j)*(2.d0*uni() - 1.d0)
		END DO
	END DO
	
	! 1 partícula pode ter a estimativa inicial do dados busca:
		DO j=1,Npar
			P(1,j) = Param(j)
		END DO

	! abre arquivo com saída com os pontos ótimos
	OPEN (UNIT=110,FILE='output/Saida_bom.dat',STATUS='unknown',ACTION='write')
	IF(RegConf == 1) OPEN (UNIT=120,FILE='output/Saida_tudo.dat',STATUS='unknown',ACTION='write')

	DO IT = 1,Niter
		! chama rotina que avalia a função objetivo para cada partícula
		DO i=1,Npt
			Param(:) = P(i,:)
			! chama rotina que calcula a função objetivo para parícula 'i'
			CALL Funobj(F(i))
			do m = 1, nMOD
				do ii = 1,Nexp(m)
					do jj = 1,nvsai(m)
						Yp(m)%M(i,ii,jj)=Y(m)%M(ii,jj)
					end do
				end do
			end do
		END DO

		IF (RegConf == 1) THEN
			ncol = 0
			do m = 1, nMOD
				ncol = ncol + Nexp(m)*NVsai(m)
			end do
			ncol = ncol + npar + 1
			write(format_,'(I4.1)') ncol
			format_ = '(1x,I5,1x,I3,'//trim(format_)//'(2x,E12.6))'
			DO i=1,Npt
				WRITE(120,format_) IT, i, P(i,:), F(i), (((Yp(m)%M(i,ii,jj),ii=1,Nexp(m)),jj=1,Nvsai(m)), m=1,nMOD)
			END DO
		END IF

!		print*, F(2), P(2,:)
!		print*, uni()

		! Verifica se a menor Fobj desta iteração é menor que o ótimo atual
		pos=MINLOC(F)					
		IF (F(pos(1)) < Fotm) THEN				! Se for menor que o melhor global
			oldFotm = Fotm
			Fotm=F(pos(1))
			oldPotm(:) = Potm(:)						! Atualiza o valor ótimo da função 
			Potm=P(pos(1),:)					! Atualiza a posição ótima da função

			write(format_,'(I4.1)') Npar+1
			format_ = '(2x,I5.1,'//trim(format_)//'(2x,E12.6))'

			WRITE(110,format_) IT, Potm, Fotm			! Escreve o novo ótimo global no arquivo
			if (noreport.ne.1) write(*,format_) IT, Fotm, Potm(:)		  	! Escreve o novo ótimo global na tela
		END IF
		!
		! Verifica se a menor Fobj de cada particula diminuiu
		FORALL(i=1:Npt,F(i) < Fpt(i))
			Fpt(i) = F(i)
			Ppt(i,:) = P(i,:)
		END FORALL

		! define ponderação decrescente para W
		W = Wo + (Wf-Wo) * DFLOAT(IT-1) / DFLOAT(Niter-1)
	
		! geraçao dos novos pontos	
		DO i=1,Npt	
			DO j=1,Npar
				! passo na direção aterior 
				Pvel(i,j) = W*Pvel(i,j)
				! passo na direção do melhor individual
				Pvel(i,j) = Pvel(i,j) + C1*uni()*(Ppt(i,j) - P(i,j))
				! passo na direção do melhor global
				Pvel(i,j) = Pvel(i,j) + C2*uni()*(Potm(j) - P(i,j))
				! Controle de velocidade
				IF( ABS(Pvel(i,j)) > Vmax(j) ) Pvel(i,j) = SIGN(Vmax(j),Pvel(i,j))
				! atualiza posição
				P(i,j) = P(i,j) + Pvel(i,j)
			END DO
		
		END DO

		! verifica se os limites de busca foram ultrapassados
		! limite inferior
		FORALL(i=1:Npt, j=1:Npar, P(i,j) < Plim(j,1))
			P(i,j) = Plim(j,1)
			Pvel(i,j) = - Pvel(i,j)/2.d0
		END FORALL
		!limite superior
		FORALL(i=1:Npt,j=1:Npar, P(i,j) > Plim(j,2))
			P(i,j) = Plim(j,2)
			Pvel(i,j) = - Pvel(i,j)/2.d0
		END FORALL

!!141009 critério de convergência antecipada
!		if(dabs(Fotm-oldFotm)/oldFotm < 1.d-7 ) then !convergência relativa na Fobj
!			if(sum(dabs(Potm(:)-oldPotm(:))/oldPotm(:),dim=1) < 1.d-7 ) then !convergência relativa na Fobj
!!				exit
!			end if 
!		end if 
!141009

!	print*, 'it', it, ' @ ', __FILE__, __LINE__

	END DO

	Fobj = Fotm
	Param = Potm
	
			CALL Funobj(F(1))
				OPEN (50,file='output/graficoPSO.dat',status='unknown')
				do m = 1, nMOD
					write(int_as_char,'(I4.1)') Nvent(m)
					format_ = '('//'1x,'//'"Exp,",'//trim(int_as_char)//'(7x,'//'"XM("'//'I2.2,'//'"),"'//'),'
					write(int_as_char,'(I4.1)') Nvsai(m)
					format_ = trim(format_)//trim(int_as_char)//'(7x,'//'"YM("'//',I2.2,'//'"),"'//',7x,'//'"YC("'//',I2.2,'//'"),"'//',7x,'//')'//')'
					WRITE(50,format_) (i, i=1,NVent(m)), (i,i, i=1,NVsai(m))
					write(int_as_char,'(I4.1)') NVent(m)+2*NVsai(m)
					format_ = '('//'2x,I4,'//'",",'//trim(int_as_char)//'(1x,E12.6,'//'","'//')'//')'
					DO i=1,Nexp(m)
						WRITE(50,format_) i,XM(m)%M(i,1:NVent(m)),(YM(m)%M(i,j), Y(m)%M(i,j), j=1,NVsai(m))
					END DO
				end do
				CLOSE(50)
	
	CLOSE(UNIT=110)
	IF(RegConf == 1) CLOSE(UNIT=120)
	print*, 'Param', Param; write(*,*) 'paused at ', __FILE__, __LINE__; read(*,*) paused
	RETURN
	END SUBROUTINE Enxame0
	
end module enxame_mod
