program main_estima
!
	use estima_mod
	use modelo1_mod										!ESPECIFICAR MODELO
	!use modelos_mod !implicito pela cadeia de uso

	implicit none
	integer :: i
	character(100) :: string
		integer, intrinsic :: TIME
	!inicializa gerador de números aleatórios
	CALL zigset( TIME()**2 ); hasseed = 1
!
	!inicializa as classes do modelo, se pertinente
	nMOD = 1
	allocate(lista_de_modelos(nMOD))
	allocate(lista_de_modelos(1)%modelo, source = i_modelo_1())				!ESPECIFICAR MODELO				!ESPECIFICAR MODELO
	!
	noreport = 0

	call estima
!	call chama_modelo
	!
	print *, ''; print *, ''; print *, '##### Execucao Concluida #####'; print *, ''; print *, ''
	!
	contains
	
end program main_estima
