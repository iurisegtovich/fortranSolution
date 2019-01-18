module modelos_mod
!
	implicit none
	
	INTEGER :: nMOD, Npar
	integer, allocatable :: NVent(:), NVsai(:), Nexp(:)
	!
	type M2_list
		real(8), allocatable :: M(:,:)
	end type
	
	type M3_list
		real(8), allocatable :: M(:,:,:)
	end type
	
	INTEGER :: FCT		! define a função objetivo
	
	! Matrizes com dados das variáveis independentes
	type(M2_list), ALLOCATABLE :: XM(:), X(:)
	! Matrizes com dados das variáveis dependentes
	type(M2_list), ALLOCATABLE :: YM(:), Y(:)
	type(M3_list), ALLOCATABLE :: EVY(:), EVYinv(:)
	! Vetor com os Parâmetros
	REAL(8), ALLOCATABLE :: Param(:) 
	!
	!outros flags
	integer :: hasseed
	integer :: noreport
	!
	!nome de arquivo
	character(100) :: dadosexp_file = 'input/dadosexp.dat'
	
	type modelos
		contains
		procedure :: resolve => resolve_deferred
	end type modelos
	
	type lista_de_modelos_c
		class(modelos), pointer :: modelo
	end type lista_de_modelos_c
	
	type(lista_de_modelos_c), pointer :: lista_de_modelos(:)
	
	contains
	!templates
	subroutine resolve_deferred(z,varent, varsai, param)
		class(modelos) :: z
		!declaration
		real(8) :: param(npar)
		real(8) :: varent(:), varsai(:)
		!implementation
		!DEFERRED
	end subroutine resolve_deferred
	
end module modelos_mod
