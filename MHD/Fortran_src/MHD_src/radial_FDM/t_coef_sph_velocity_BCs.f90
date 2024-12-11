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
      use t_coef_fdm2_free_slip_ICB
      use t_coef_fdm2_free_slip_CMB
      use t_coef_fdm3_n2e_zero_vp_ICB
      use t_coef_fdm3_n2e_zero_vp_CMB
      use t_coef_fdm3_n2e_zero_vp_CTR
      use t_coef_fdm3_n2e_free_vp_ICB
      use t_coef_fdm3_n2e_free_vp_CMB
!
      use t_coef_fdm4_zero_vpol_ICB
      use t_coef_fdm4_zero_vpol_CMB
      use t_coef_fdm4_free_vpol_ICB
      use t_coef_fdm4_free_vpol_CMB
!
      implicit none
!
!>      Structure for Additional boundary condition matrices for velocity
      type velocity_boundary_FDMs
!>        Structure for FDM matrix of free slip boundary at ICB
        type(fdm2_ICB_free_slip) :: fdm2_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB
        type(fdm2_CMB_free_slip) :: fdm2_free_CMB
!
!>        Matrix to evaluate radial derivative at ICB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_ICB(0:1,2)
!>        Matrix to evaluate radial derivative at CMB with fixed field
!!        with first order accuracy
        real(kind = kreal) :: fdm1_fix_fld_CMB(-1:0,2)
!
!>        Structure for FDM matrix at ICB element with zero poloidal
        type(fdm3_n2e_ICB_zero_vpol) :: fdm3e_vp0_ICB
!>        Structure for FDM matrix at CMB element with zero poloidal
        type(fdm3_n2e_CMB_zero_vpol) :: fdm3e_vp0_CMB
!>        Structure for FDM matrix of free slip boundary at center
        type(fdm3_n2e_CTR_vpol) :: fdm3e_center
!
!>        Structure for FDM matrix of free slip boundary at ICB element
        type(fdm3_n2e_ICB_free_vpol) :: fdm3e_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB element
        type(fdm3_n2e_CMB_free_vpol) :: fdm3e_free_CMB
!
!>        Structure for 4th order FDM matrix of non-slip boundary at ICB
        type(fdm4_ICB_zero_vpol) :: fdm4_noslip_ICB
!>        Structure for 4th order FDM matrix of non-slip boundary at CMB
        type(fdm4_CMB_zero_vpol) :: fdm4_noslip_CMB
!
!>        Structure for 4th order FDM matrix of free slip boundary at ICB
        type(fdm4_ICB_free_vpol) :: fdm4_free_vp_ICB
!>        Structure for 4th order FDM matrix of free slip boundary at CMB
        type(fdm4_CMB_free_vpol) :: fdm4_free_vp_CMB
      end type velocity_boundary_FDMs
!
      end module t_coef_sph_velocity_BCs
!
