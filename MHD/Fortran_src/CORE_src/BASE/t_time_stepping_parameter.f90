!>@file   t_time_stepping_parameter.f90
!!@brief  module t_time_stepping_parameter
!!
!!@author H. Matsui
!!@date Programmed in July., 2001
!!@n    Modified by H. Matsui in 2003
!!@n    Modified by H. Matsui in 2016
!
!> @brief Structure for time steppings
!
      module t_time_stepping_parameter
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      Scheme ID for no evolution
      integer (kind=kint), parameter :: id_no_evolution =     0
!>      Scheme ID for explicit Euler scheme
      integer (kind=kint), parameter :: id_explicit_euler =   1
!>      Scheme ID for 2nd order Adams-Bashforth Scheme
      integer (kind=kint), parameter :: id_explicit_adams2 =  2
!>      Scheme ID for Crank-Nicolson Scheme
      integer (kind=kint), parameter :: id_Crank_nicolson =   3
!>      Scheme ID for Crank-Nicolson Scheme with consistent mass matrix
      integer (kind=kint), parameter :: id_Crank_nicolson_cmass = 4
!>      TIme evolution schme flag
!
!
      type time_evolution_params
!>        Time evolution flag for velocity
        integer (kind=kint) :: iflag_scheme = id_no_evolution
!>        Coefficient of implicit term
        real(kind = kreal) :: coef_imp = half
!>        Coefficient of explicit term
        real(kind = kreal) :: coef_exp = half
      end type time_evolution_params
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_implicit_coefs(coef_imp_ctl, evo)
!
      use t_control_elements
!
      type(read_real_item), intent(in) :: coef_imp_ctl
      type(time_evolution_params), intent(inout) :: evo
!
!
        if(evo%iflag_scheme .ge. id_Crank_nicolson) then
          if (coef_imp_ctl%iflag .eq. 0) then
            evo%coef_imp = 0.5d0
          else
            evo%coef_imp = coef_imp_ctl%realvalue
          end if
        else
          evo%coef_imp = 0.0d0
        end if
        evo%coef_exp = 1.0d0 - evo%coef_imp
!
      end subroutine set_implicit_coefs
!
! -----------------------------------------------------------------------
!
      end module  t_time_stepping_parameter
