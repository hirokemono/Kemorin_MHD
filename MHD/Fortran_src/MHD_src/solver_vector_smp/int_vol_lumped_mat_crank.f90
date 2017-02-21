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
!!      subroutine int_vol_crank_mat_lump(evo_V, evo_B, evo_A,          &
!!     &          evo_T, evo_C, mesh, fluid, conduct,                   &
!!     &          fl_prop, cd_prop, ht_prop, cp_prop,                   &
!!     &          DJDS_table, DJDS_table_fluid, mlump_fl, mlump_cd,     &
!!     &          mat_velo, mat_magne, mat_temp, mat_light)
!!      subroutine add_lumped_coriolis_matrix(mesh, fluid, fl_prop,     &
!!     &          DJDS_table_fluid,  mlump_fl, mat_velo)
!!        type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(mesh_data), intent(in) ::              mesh
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
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
      use t_time_stepping_parameter
      use t_physical_property
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
      subroutine int_vol_crank_mat_lump(evo_V, evo_B, evo_A,            &
     &          evo_T, evo_C, mesh, fluid, conduct,                     &
     &          fl_prop, cd_prop, ht_prop, cp_prop,                     &
     &          DJDS_table, DJDS_table_fluid, mlump_fl, mlump_cd,       &
     &          mat_velo, mat_magne, mat_temp, mat_light)
!
      type(time_evolution_params), intent(in) :: evo_V, evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
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
      if (evo_V%iflag_scheme .eq. id_Crank_nicolson                     &
     &     .and. fl_prop%coef_velo .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,         &
     &      DJDS_table_fluid%OLDtoNEW, mlump_fl%ml_o,                   &
     &      mat_velo%num_non0, mat_velo%aiccg)
      end if
!
      if (evo_T%iflag_scheme .eq. id_Crank_nicolson                     &
     &     .and. ht_prop%coef_advect .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,         &
     &      DJDS_table_fluid%OLDtoNEW, mlump_fl%ml_o,                   &
     &      mat_temp%num_non0, mat_temp%aiccg)
      end if
!
      if (evo_B%iflag_scheme .eq. id_Crank_nicolson                     &
     &     .and. cd_prop%coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, conduct%numnod_fld, conduct%inod_fld,     &
     &      DJDS_table%OLDtoNEW, mlump_cd%ml_o,                         &
     &      mat_magne%num_non0, mat_magne%aiccg)
      end if
!
      if (evo_A%iflag_scheme .eq. id_Crank_nicolson                     &
     &     .and. cd_prop%coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (mesh%node%numnod, conduct%numnod_fld, conduct%inod_fld,     &
     &      DJDS_table%OLDtoNEW, mlump_cd%ml_o,                         &
     &      mat_magne%num_non0, mat_magne%aiccg)
      end if
!
      if (evo_C%iflag_scheme .eq. id_Crank_nicolson                     &
     &     .and. cp_prop%coef_advect .gt. zero) then
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
      subroutine add_lumped_coriolis_matrix(mesh, fluid, fl_prop,       &
     &          DJDS_table_fluid,  mlump_fl, mat_velo)
!
      use cal_coriolis_mat33
!
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid
      type(fluid_property), intent(in) :: fl_prop
      type(DJDS_ordering_table), intent(in) :: DJDS_table_fluid
      type(lumped_mass_matrices), intent(in) :: mlump_fl
!
      type(DJDS_MATRIX), intent(inout) :: mat_velo
!
!
      call cal_lumped_coriolis_matrix                                   &
     &   (mesh%node%numnod, fluid%numnod_fld, fluid%inod_fld,           &
     &    DJDS_table_fluid%OLDtoNEW, fl_prop%coef_cor, fl_prop%sys_rot, &
     &    fl_prop%coef_imp, mlump_fl%ml_o, mat_velo%num_non0,           &
     &    mat_velo%aiccg)
!
      end subroutine add_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!
      end module int_vol_lumped_mat_crank
