!>@file   t_coef_sph_velocity_BCs.f90
!!@brief  module t_coef_sph_velocity_BCs
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2024
!
!>@brief  Structure for basic boundary conditions for velocity
!!
!!
!!@verbatim
!!@endverbatim
!!
      module t_coef_sph_velocity_BCs
!
      use m_precision
!
      implicit none
!
!>      Structure for basic boundary condition parameters
      type velocity_boundary_FDMs
!>        Matrix to evaluate radial derivative at ICB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_ICB(0:1,2)
!>        Matrix to evaluate radial derivative at CMB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_CMB(-1:0,2)
      end type velocity_boundary_FDMs
!
      end module t_coef_sph_velocity_BCs
!
