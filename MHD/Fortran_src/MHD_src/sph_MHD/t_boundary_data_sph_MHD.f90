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
!!      subroutine set_MHD_evolved_boundaries                           &
!!     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!!      subroutine set_cv_evolved_boundaries                            &
!!     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_grids), intent(in) :: sph
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
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
      use t_time_data
      use t_spheric_parameter
      use t_control_parameter
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
        type(sph_vector_BC_coef) :: ICB_Uspec
!>        Structure for boundary velocity spectr
        type(sph_vector_BC_coef) :: CMB_Uspec
!>        Structure for boundary magnetic spectr
        type(sph_vector_BC_coef) :: ICB_Bspec
!>        Structure for boundary magnetic spectr
        type(sph_vector_BC_coef) :: CMB_Bspec
!>        Structure for boundary temeprture spectr
        type(sph_scalar_BC_coef) :: bc_Tspec
!>        Structure for boundary composition spectr
        type(sph_scalar_BC_coef) :: bc_Cspec
!
!>        Structure for evoluved boundary velocity spectr
        type(sph_vector_BC_evo) :: ICB_Uevo
!>        Structure for evoluved boundary velocity spectr
        type(sph_vector_BC_evo) :: CMB_Uevo
!>        Structure for evoluved boundary magnetic spectr
        type(sph_vector_BC_evo) :: ICB_Bevo
!>        Structure for evoluved boundary magnetic spectr
        type(sph_vector_BC_evo) :: CMB_Bevo
!>        Structure for evoluved boundary temeprture spectr
        type(sph_scalar_BC_evo) :: ICB_Tevo
!>        Structure for evoluved boundary temeprture spectr
        type(sph_scalar_BC_evo) :: CMB_Tevo
!>        Structure for evoluved boundary composition spectr
        type(sph_scalar_BC_evo) :: ICB_Cevo
!>        Structure for evoluved boundary composition spectr
        type(sph_scalar_BC_evo) :: CMB_Cevo
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
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_MHD_evolved_boundaries                             &
     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!
      use set_evoluved_boundaries
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      call set_cv_evolved_boundaries(time_d, sph, MHD_prop, sph_MHD_bc)
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call set_evo_vector_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_B,                                        &
     &      sph_MHD_bc%ICB_Bevo, sph_MHD_bc%CMB_Bevo,                   &
     &      sph_MHD_bc%ICB_Bspec, sph_MHD_bc%CMB_Bspec)
      end if
!
      end subroutine set_MHD_evolved_boundaries
!
! ----------------------------------------------------------------------
!
      subroutine set_cv_evolved_boundaries                              &
     &         (time_d, sph, MHD_prop, sph_MHD_bc)
!
      use set_evoluved_boundaries
!
      type(time_data), intent(in) :: time_d
      type(sph_grids), intent(in) :: sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
!
      type(sph_MHD_boundary_data), intent(inout) :: sph_MHD_bc
!
!
      if(MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_vector_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_U,                                        &
     &      sph_MHD_bc%ICB_Uevo, sph_MHD_bc%CMB_Uevo,                   &
     &      sph_MHD_bc%ICB_Uspec, sph_MHD_bc%CMB_Uspec)
      end if
      if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_scalar_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_T,                                        &
     &      sph_MHD_bc%ICB_Tevo, sph_MHD_bc%CMB_Tevo,                   &
     &      sph_MHD_bc%bc_Tspec)
      end if
      if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
        call set_evo_scalar_boundaries(time_d%time, sph%sph_rj,         &
     &      sph_MHD_bc%sph_bc_C,                                        &
     &      sph_MHD_bc%ICB_Cevo, sph_MHD_bc%CMB_Cevo,                   &
     &      sph_MHD_bc%bc_Cspec)
      end if
!
      end subroutine set_cv_evolved_boundaries
!
! ----------------------------------------------------------------------
!
      end module t_boundary_data_sph_MHD
