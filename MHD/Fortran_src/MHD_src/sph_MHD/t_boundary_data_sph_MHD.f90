!>@file   t_boundary_data_sph_MHD.f90
!!@brief  module t_boundary_data_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine alloc_sph_scalar_bc_array(jmax, bc_Sspectr)
!!        type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!!      subroutine alloc_sph_vector_bc_array(jmax, sph_MHD_bc)
!!        type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!!
!!      subroutine dealloc_sph_scalar_bc_array(bc_Sspectr)
!!        type(sph_scalar_BC_spectr), intent(inout) :: bc_Sspectr
!!      subroutine dealloc_sph_vector_bc_array(sph_MHD_bc)
!!        type(sph_vector_BC_spectr), intent(inout) :: bc_Uspectr
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module t_boundary_data_sph_MHD
!
      use m_precision
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_fdm2_MHD_boundaries
      use t_coef_fdm4_MHD_boundaries
!
      implicit none
!
!
!>      Structure for boundary conditions
      type sph_MHD_boundary_data
!>        Structure for basic velocity boundary condition parameters
        type(sph_boundary_type) :: sph_bc_U
!>        Structure for basic magnetic boundary condition parameters
        type(sph_boundary_type) :: sph_bc_B
!>        Structure for basic thermal boundary condition parameters
        type(sph_boundary_type) :: sph_bc_T
!>        Structure for basic compositional boundary condition parameters
        type(sph_boundary_type) :: sph_bc_C
!
!>        Structure for boundary velocity spectr
        type(sph_vector_BC_spectr) :: bc_Uspectr
!>        Structure for boundary magnetic spectr
        type(sph_vector_BC_spectr) :: bc_Bspectr
!>        Structure for boundary temeprture spectr
        type(sph_scalar_BC_spectr) :: bc_Tspectr
!>        Structure for boundary composition spectr
        type(sph_scalar_BC_spectr) :: bc_Cspectr
!
!
!>        Structure for FDM matrix of center
        type(fdm2_center_mat) :: fdm2_center
!>        Structure for FDM matrix of free slip boundary at ICB
        type(fdm2_free_slip) :: fdm2_free_ICB
!>        Structure for FDM matrix of free slip boundary at CMB
        type(fdm2_free_slip) :: fdm2_free_CMB
!
!>        Structure for 4th order FDM matrix of non-slip boundary at ICB
        type(fdm4_ICB_vpol) :: fdm4_noslip_ICB
!>        Structure for 4th order FDM matrix of free slip boundary at ICB
        type(fdm4_ICB_vpol) :: fdm4_free_ICB
!
!>        Structure for 4th order FDM matrix of non-slip boundary at CMB
        type(fdm4_CMB_vpol) :: fdm4_noslip_CMB
!>        Structure for 4th order FDM matrix of free slip boundary at CMB
        type(fdm4_CMB_vpol) :: fdm4_free_CMB
      end type sph_MHD_boundary_data
!
      end module t_boundary_data_sph_MHD
