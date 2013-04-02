!int_crank_mat_consist_type.f90
!      module int_crank_mat_consist_type
!
!      Written by H. Matsui on July, 2005
!
!      subroutine s_int_crank_mat_consist_type(mesh, jac_3d, rhs_tbl,   &
!     &          djds_const_fl, djds_const_cd, fem_wk,                  &
!     &          mat_velo, mat_magne, mat_temp, mat_d_scalar)
!        type(mesh_data), intent(in) ::              mesh
!        type(jacobians_3d), intent(in) ::           jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) :: djds_const_fl
!        type(table_mat_const), intent(in) :: djds_const_cd
!
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(DJDS_MATRIX),  intent(inout) :: mat_velo
!        type(DJDS_MATRIX),  intent(inout) :: mat_magne
!        type(DJDS_MATRIX),  intent(inout) :: mat_temp
!        type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
      module int_crank_mat_consist_type
!
      use m_precision
!
      use m_machine_parameter
      use t_finite_element_mat
      use t_table_FEM_const
      use t_solver_djds
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_int_crank_mat_consist_type(mesh, jac_3d, rhs_tbl,    &
     &          djds_const_fl, djds_const_cd, fem_wk,                   &
     &          mat_velo, mat_magne, mat_temp, mat_d_scalar)
!
      use m_control_parameter
      use m_physical_property
      use t_mesh_data
      use t_jacobians
      use t_geometry_data_MHD
!
      use fem_skv_mass_mat_type
      use add_skv1_2_matrix_type
!
!
      type(mesh_geometry), intent(in) ::          mesh
      type(jacobians_3d), intent(in) ::           jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: djds_const_fl
      type(table_mat_const), intent(in) :: djds_const_cd
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(DJDS_MATRIX),  intent(inout) :: mat_velo
      type(DJDS_MATRIX),  intent(inout) :: mat_magne
      type(DJDS_MATRIX),  intent(inout) :: mat_temp
      type(DJDS_MATRIX),  intent(inout) :: mat_d_scalar
!
       integer(kind = kint) :: k2
!
!
      do  k2 = 1, mesh%ele%nnod_4_ele
        call reset_sk6_type(n_scalar, mesh%ele%numele,                  &
     &      mesh%ele%nnod_4_ele, fem_wk)
        call fem_skv_mass_matrix_type(mesh%ele%istack_ele_smp,          &
     &      intg_point_t_evo, k2, mesh%ele, jac_3d, fem_wk)
!
        if ( iflag_t_evo_4_velo.eq.4 .and. coef_velo.gt.0.0d0 ) then
          write(*,*) 'init_consist_mat_velo', k2
          call add_skv1_2_matrix33_type(mesh%ele, rhs_tbl,              &
     &        djds_const_fl, fem_wk, k2, mat_velo)
        end if
!
        if ( iflag_t_evo_4_temp.eq.4 .and. coef_temp.gt.0.0d0 ) then
          write(*,*) 'init_consist_mat_temp', k2
          call add_skv1_2_matrix11_type(mesh%ele, rhs_tbl,              &
     &        djds_const_fl, fem_wk, k2, mat_temp)
        end if
!
        if ( iflag_t_evo_4_composit.eq.4                                &
     &      .and. coef_scalar.gt.0.0d0) then
          write(*,*) 'init_consist_mat_d_scalar_type', k2
          call add_skv1_2_matrix11_type(mesh%ele, rhs_tbl,              &
     &        djds_const_fl, fem_wk, k2, mat_d_scalar)
        end if
!
        if ( iflag_t_evo_4_magne.eq.4 .and. coef_magne.gt.0.0d0) then
          write(*,*) 'init_consist_mat_magne_type', k2
          call add_skv1_2_matrix33_type(mesh%ele, rhs_tbl,              &
     &        djds_const_cd, fem_wk, k2, mat_magne)
        end if
!
        if ( iflag_t_evo_4_vect_p.eq.4 .and. coef_magne.gt.0.0d0) then
          write(*,*) 'init_consist_mat_vect_p', k2
          call add_skv1_2_matrix33_type(mesh%ele, rhs_tbl,              &
     &        djds_const_cd, fem_wk, k2, mat_magne)
        end if
!
      end do
!
      end subroutine s_int_crank_mat_consist_type
!
!  ---------------------------------------------------------------------
!
      end module int_crank_mat_consist_type
