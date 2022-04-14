!>@file   t_normalize_parameter.f90
!!@brief  module t_normalize_parameter
!!
!!@author H. Matsui
!!@date Programmed in 2005
!!@date Modified in Jan., 2007
!
!>@brief  dimensionless number list for each term
!!
!!@verbatim
!!      subroutine alloc_dimless_list(dimless_list)
!!      subroutine dealloc_dimless_list(dimless_list)
!!        type(list_of_dimless), intent(inout) :: dimless_list
!!      subroutine alloc_coef_power_list(coef_list)
!!      subroutine dealloc_coef_power_list(coef_list)
!!        type(powers_4_coefficients), intent(inout) :: coef_list
!!
!!      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!!        type(ctl_array_cr), intent(in) :: coef_ctl
!!        type(list_of_dimless), intent(inout) :: dimless_list
!!      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!!        type(ctl_array_cr), intent(in) :: coef_ctl
!!        type(powers_4_coefficients), intent(inout) :: coef_list
!!@endverbatim
!!
      module t_normalize_parameter
!
      use m_precision
!
      implicit  none
!
!
!>      Structure of dimensionless numbers
      type list_of_dimless
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of dimensionless number's name
        character(len=kchara), allocatable :: name(:)
!>        List of values
        real (kind = kreal), allocatable :: value(:)
      end type list_of_dimless
!
!
!>      Structure of powers for coefficients
      type powers_4_coefficients
!>        Number of parameters
        integer(kind=kint) :: num
!>        List of parameter name
        character(len=kchara), allocatable :: name(:)
!>        List of power
        real (kind = kreal), allocatable :: power(:)
      end type powers_4_coefficients
!
!
!>       List of parameters to construct coefficients
      type coef_parameters_list
!>        Dimensionless numbers list
        type(list_of_dimless) :: dimless_list
!
!>        Dimensionless numbers for heat flux
        type(powers_4_coefficients) :: coefs_termal
!>        Dimensionless numbers for momentum flux
        type(powers_4_coefficients) :: coefs_momentum
!>        Dimensionless numbers for pressure gradient
        type(powers_4_coefficients) :: coefs_pressure
!>        Dimensionless numbers for heat evolution of magnetic field
        type(powers_4_coefficients) :: coefs_magnetic
!>        Dimensionless numbers for heat electric potential
        type(powers_4_coefficients) :: coefs_magne_p
!>        Dimensionless numbers for heat composition flux
        type(powers_4_coefficients) :: coefs_composition
!
!>        Dimensionless numbers for heat thermal diffusion
        type(powers_4_coefficients) :: coefs_t_diffuse
!>        Dimensionless numbers for heat viscous diffusion
        type(powers_4_coefficients) :: coefs_v_diffuse
!>        Dimensionless numbers for heat magnetic diffusion
        type(powers_4_coefficients) :: coefs_m_diffuse
!>        Dimensionless numbers for heat compositional diffusion
        type(powers_4_coefficients) :: coefs_c_diffuse
!
!>        Dimensionless numbers for heat thermal buoyancy flux
        type(powers_4_coefficients) :: coefs_buoyancy
!>        Dimensionless numbers for heat compositional buoyancy flux
        type(powers_4_coefficients) :: coefs_comp_buo
!>        Dimensionless numbers for heat Coriolis force
        type(powers_4_coefficients) :: coefs_Coriolis
!>        Dimensionless numbers for heat Lorengtz force
        type(powers_4_coefficients) :: coefs_Lorentz
!>        Dimensionless numbers for heat magnetic induction
        type(powers_4_coefficients) :: coefs_induction
!>        Dimensionless numbers for heat heat source
        type(powers_4_coefficients) :: coefs_h_source
!>        Dimensionless numbers for heat compositional source
        type(powers_4_coefficients) :: coefs_c_source
!
!>        Dimensionless numbers for magnetic energy scaling
        type(powers_4_coefficients) :: coefs_me_to_ke
      end type coef_parameters_list
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      allocate(dimless_list%name(dimless_list%num))
      allocate(dimless_list%value(dimless_list%num))
      if(dimless_list%num .gt. 0) dimless_list%value = 0.0d0
!
      end subroutine alloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine alloc_coef_power_list(coef_list)
!
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      allocate(coef_list%name(coef_list%num))
      allocate(coef_list%power(coef_list%num))
      if(coef_list%num .gt. 0) coef_list%power = 0.0d0
!
      end subroutine alloc_coef_power_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_dimless_list(dimless_list)
!
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      deallocate(dimless_list%name, dimless_list%value)
!
      end subroutine dealloc_dimless_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_coef_power_list(coef_list)
!
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      deallocate(coef_list%name, coef_list%power)
!
      end subroutine dealloc_coef_power_list
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_dimless_from_ctl(coef_ctl, dimless_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(list_of_dimless), intent(inout) :: dimless_list
!
!
      call alloc_dimless_list(dimless_list)
      if (dimless_list%num .le. 0) return
!
      dimless_list%name(1:dimless_list%num)                            &
     &             = coef_ctl%c_tbl(1:dimless_list%num)
      dimless_list%value(1:dimless_list%num)                           &
     &             = coef_ctl%vect(1:dimless_list%num)
!
      end subroutine copy_dimless_from_ctl
!
! -----------------------------------------------------------------------
!
      subroutine copy_power_and_names_from_ctl(coef_ctl, coef_list)
!
      use t_control_array_charareal
!
      type(ctl_array_cr), intent(in) :: coef_ctl
      type(powers_4_coefficients), intent(inout) :: coef_list
!
!
      call alloc_coef_power_list(coef_list)
      if (coef_list%num .le. 0) return
!
      coef_list%name(1:coef_list%num) = coef_ctl%c_tbl(1:coef_list%num)
      coef_list%power(1:coef_list%num) = coef_ctl%vect(1:coef_list%num)
!
      end subroutine copy_power_and_names_from_ctl
!
! -----------------------------------------------------------------------
!
      end module t_normalize_parameter
