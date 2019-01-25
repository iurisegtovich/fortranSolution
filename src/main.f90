program main

  use, intrinsic :: iso_c_binding

  use module1 !nlopd include there
    
   call globalOptimize()
   
   call optimize()
   
   contains
   
       subroutine globalOptimize()
    ! This subroutine optimizes as a function of real(8) using NLOPT routines

    !double precision lb(1),ub(1)
    double precision lb(2),ub(2)
    !double precision lb(3),ub(3)
    integer(8) opt
    !double precision x(1), minf
    double precision x(2), minf
    !double precision x(3), minf
    integer ires
    !Create the optimization problem
    call nlo_create(opt, NLOPT_GN_DIRECT_L, 2)

    ! Define lower bounds
    call nlo_get_lower_bounds(ires, opt, lb)
    lb(1) = 10.!g
    lb(2) = 0. !Ng
    call nlo_set_lower_bounds(ires, opt, lb)
    
    ! Define upper bounds
    call nlo_get_upper_bounds(ires, opt, ub)
    ub(1) = 300.d0
    ub(2) = 200.

    call nlo_set_upper_bounds(ires, opt, ub)
    ! Point to objective function
    call nlo_set_min_objective(ires, opt, objF, 0)
    ! Set relative tolerance
    call nlo_set_maxeval(ires, opt, 10000)
    ! Guess
    x(1) = (lb(1)+ub(1))/2.d0
    x(2) = (lb(2)+ub(2))/2.d0
    ! Run optimization problem
    call nlo_optimize(ires, opt, x, minf)

    ! Error handling
    if (ires.lt.0) then
        write(*,*) 'nlopt failed!'
    else
        write(*,*) 'nlopt ok!'
    endif
    call nlo_destroy(opt)
   
   end subroutine
   
       subroutine optimize()
    ! This subroutine optimizes a function of real(8) using NLOPT routines
    double precision lb(2),ub(2)
    integer(8) opt
    double precision x(2), minf
    integer ires
    call nlo_create(opt, NLOPT_LN_COBYLA, 2) !Constrained local derivative free
    call nlo_get_lower_bounds(ires, opt, lb)
    lb(1) = 0.80
    lb(2) = 0.8d0
    call nlo_set_lower_bounds(ires, opt, lb)
    call nlo_get_upper_bounds(ires, opt, ub)
    ub(1) = 1.2d0
    ub(2) = 1.2d0
    call nlo_set_upper_bounds(ires, opt, ub)
    ! Point to objective function
    call nlo_set_min_objective(ires, opt, objF, 0)
    ! Set inequality constraint
    call nlo_add_inequality_constraint(ires, opt, constraint, 0, 1.D-10)
    ! Set relative tolerance
    call nlo_set_xtol_rel(ires, opt, 1.D-8)
    ! Guess
    x(1) = 1.
    x(2) = 1.
    ! Run optimization problem
    call nlo_optimize(ires, opt, x, minf)
    ! Error handling
    if (ires.lt.0) then
        write(*,*) 'nlopt failed!'
    else
        write(*,*) 'nlopt ok!'
    endif
    call nlo_destroy(opt)
    end subroutine
    
    !!! Objective Function
    subroutine objF(val, nvar, x, grad, need_gradient, f_data)
    integer nvar, need_gradient
    double precision val, x(:), grad(:), f_data
    integer i
    val=0
    do i = i,size(x)
      val = val+ x(i)**2
    enddo
    end subroutine
   
       subroutine constraint(val, nvar, x, grad, need_gradient, d)
    implicit none
    integer need_gradient, nvar
    double precision val, x(nvar), grad(nvar), d
    val =  x(1)*x(2)
    end
   
  end program main
