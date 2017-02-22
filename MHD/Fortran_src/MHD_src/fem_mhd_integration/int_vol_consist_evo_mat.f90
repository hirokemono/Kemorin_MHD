!int_vol_consist_evo_mat.f90
!      module int_vol_consist_evo_mat
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_vol_crank_mat_consist                            &
!!     &        (num_int, evo_B, evo_A, evo_T, evo_C,                   &
!!     &         mesh, fl_prop, cd_prop, ht_prop, cp_prop,              &
!!     &         jac_3d, rhs_tbl, MG_mat_fl_q, MG_mat_full_cd_q, fem_wk,&
!!     &         mat_velo, mat_magne, mat_temp, mat_light)
!!        type(time_evolution_params), intent(in) :: evo_B, evo_A
!!        type(time_evolution_params), intent(in) :: evo_T, evo_C
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: MG_mat_fl_q
!!        type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!!        type(DJDS_MATRIX),  intent(inout) :: mat_light
!
      module int_vol_consist_evo_mat
!
      use m_precision
!
      use t_time_stepping_parameter
      use t_physical_property
      use t_mesh_data
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_sorted_node_MHD
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_consist                              &
     &         (num_int, evo_B, evo_A, evo_T, evo_C,                    &
     &          mesh, fl_prop, cd_prop, ht_prop, cp_prop,               &
     &          jac_3d, rhs_tbl, MG_mat_fl_q, MG_mat_full_cd_q, fem_wk, &
     &          mat_velo, mat_magne, mat_temp, mat_light)
!
      use m_phys_constants
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      integer(kind = kint), intent(in) :: num_int
      type(time_evolution_params), intent(in) :: evo_B, evo_A
      type(time_evolution_params), intent(in) :: evo_T, evo_C
      type(mesh_geometry), intent(in) :: mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: MG_mat_fl_q
      type(table_mat_const), intent(in) :: MG_mat_full_cd_q
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_light
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, mesh%ele%nnod_4_ele
        call reset_sk6(n_scalar, mesh%ele, fem_wk%sk6)
        call fem_skv_mass_matrix_type(mesh%ele%istack_ele_smp,          &
     &      num_int, k2, mesh%ele, jac_3d, fem_wk%sk6)
!
        if (fl_prop%iflag_scheme .eq. id_Crank_nicolson_cmass           &
     &      .and. fl_prop%coef_velo.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix33(mesh%ele, rhs_tbl,              &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_velo%num_non0, mat_velo%aiccg)
        end if
!
        if ( evo_T%iflag_scheme .eq. id_Crank_nicolson_cmass            &
     &      .and. ht_prop%coef_advect.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix11(mesh%ele, rhs_tbl,              &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_temp%num_non0, mat_temp%aiccg)
        end if
!
        if ( evo_C%iflag_scheme .eq. id_Crank_nicolson_cmass            &
     &      .and. cp_prop%coef_advect .gt. 0.0d0) then
          call add_skv1_to_crs_matrix11(mesh%ele, rhs_tbl,              &
     &        MG_mat_fl_q, k2, fem_wk%sk6,                              &
     &        mat_light%num_non0, mat_light%aiccg)
        end if
!
        if ( evo_B%iflag_scheme .eq. id_Crank_nicolson_cmass            &
     &      .and. cd_prop%coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (mesh%ele, rhs_tbl, MG_mat_full_cd_q,                      &
     &        k2, fem_wk%sk6, mat_magne%num_non0, mat_magne%aiccg)
        end if
!
        if ( evo_A%iflag_scheme .eq. id_Crank_nicolson_cmass            &
     &      .and. cd_prop%coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (mesh%ele, rhs_tbl, MG_mat_full_cd_q,                      &
     &        k2, fem_wk%sk6, mat_magne%num_non0, mat_magne%aiccg)
        end if
      end do
!
      end subroutine int_vol_crank_mat_consist
!
! ----------------------------------------------------------------------
!
      end module int_vol_consist_evo_mat
