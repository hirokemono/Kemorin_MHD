!>@file   int_vol_lumped_mat_crank.f90
!!@brief  module int_vol_lumped_mat_crank
!!
!!@author H. Matsui and H. Okuda
!!@date  programmed by H.Matsui and H. Okuda in  July 2000 (ver 1.1)
!!@n    Modified in Aug., 2005
!!@n    Modified in Nov., 2013
!
!>      DJDS matrix data
!!
!!@verbatim
!!      subroutine int_vol_crank_mat_lump(mesh, fluid, conduct,         &
!!     &          DJDS_table, DJDS_table_fluid, mlump_fl, mlump_cd,     &
!!     &          mat_velo, mat_magne, mat_temp, mat_light)
!!      subroutine add_lumped_coriolis_matrix(mesh, fluid,              &
!!     &          DJDS_table_fluid, mhd_fem_wk, Vmat_DJDS)
!!        type(mesh_data), intent(in) ::              mesh
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(DJDS_ordering_table),  intent(in) :: DJDS_table
!!        type(DJDS_ordering_table),  intent(in) :: DJDS_table_fluid
!!        type(lumped_mass_matrices), intent(in) :: mlump_fl, mlump_cd
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!!@endverbatim
!
      module int_vol_lumped_mat_crank
!
      use m_precision
      use m_constants
!
      use m_physical_property
!
      use t_mesh_data
      use t_geometry_data
      use t_geometry_data_MHD
      use t_MHD_finite_element_mat
      use t_solver_djds
!
      use init_djds_matrix_lump
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_lump(mesh, fluid, conduct,           &
     &          DJDS_table, DJDS_table_fluid, mlump_fl, mlump_cd,       &
     &          mat_velo, mat_magne, mat_temp, mat_light)
!
      use m_control_parameter
!
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(DJDS_ordering_table), intent(in) :: DJDS_table
      type(DJDS_ordering_table), intent(in) :: DJDS_table_fluid
      type(lumped_mass_matrices), intent(in) :: mlump_fl, mlump_cd
!
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
!
!
!$omp parallel
      if (iflag_t_evo_4_velo .eq. id_Crank_nicolson                     &
     &     .and. coef_velo .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,         &
     &      DJDS_table_fluid%OLDtoNEW, mlump_fl%ml_o,                   &
     &      mat_velo%num_non0, mat_velo%aiccg)
      end if
!
      if (iflag_t_evo_4_temp .eq. id_Crank_nicolson                     &
     &     .and. coef_temp .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,         &
     &      DJDS_table_fluid%OLDtoNEW, mlump_fl%ml_o,                   &
     &      mat_temp%num_non0, mat_temp%aiccg)
      end if
!
      if (iflag_t_evo_4_magne .eq. id_Crank_nicolson                    &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, conduct%numnod_fld, conduct%inod_fld,     &
     &      DJDS_table%OLDtoNEW, mlump_cd%ml_o,                         &
     &      mat_magne%num_non0, mat_magne%aiccg)
      end if
!
      if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson                   &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, conduct%numnod_fld, conduct%inod_fld,     &
     &      DJDS_table%OLDtoNEW, mlump_cd%ml_o,                         &
     &      mat_magne%num_non0, mat_magne%aiccg)
      end if
!
      if (evo_comp%iflag_scheme .eq. id_Crank_nicolson                  &
     &     .and. coef_light .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,         &
     &      DJDS_table_fluid%OLDtoNEW, mlump_fl%ml_o,                   &
     &      mat_light%num_non0, mat_light%aiccg)
      end if
!$omp end parallel
!
      end subroutine int_vol_crank_mat_lump
!
! ----------------------------------------------------------------------
!
      subroutine add_lumped_coriolis_matrix(mesh, fluid,                &
     &          DJDS_table_fluid, mlump_fl, mat_velo)
!
      use cal_coriolis_mat33
!
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(DJDS_ordering_table), intent(in) :: DJDS_table_fluid
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(DJDS_MATRIX), intent(inout) :: mat_velo
!
!
      call cal_lumped_coriolis_matrix                                   &
     &   (mesh%node%numnod, fluid%numnod_fld,                           &
     &    fluid%inod_fld, DJDS_table_fluid%OLDtoNEW, coef_cor, angular, &
     &    mlump_fl%ml_o, mat_velo%num_non0, mat_velo%aiccg)
!
      end subroutine add_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!
      end module int_vol_lumped_mat_crank
