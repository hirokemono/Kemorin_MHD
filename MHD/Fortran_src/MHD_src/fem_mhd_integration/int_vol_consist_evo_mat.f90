!int_vol_consist_evo_mat.f90
!      module int_vol_consist_evo_mat
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine int_vol_crank_mat_consist                            &
!!     &         (ele, jac_3d, rhs_tbl, MHD_mat_tbls, fem_wk)
!!        type(element_data), intent(in) :: ele
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!
      module int_vol_consist_evo_mat
!
      use m_precision
!
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_sorted_node_MHD
      use t_finite_element_mat
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
     &         (ele, jac_3d, rhs_tbl, MHD_mat_tbls, fem_wk)
!
      use m_control_parameter
      use m_physical_property
      use m_phys_constants
!
      use m_solver_djds_MHD
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(tables_MHD_mat_const), intent(in) :: MHD_mat_tbls
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
!
      integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele%nnod_4_ele
        call reset_sk6(n_scalar, ele, fem_wk%sk6)
!
        call fem_skv_mass_matrix_type(ele%istack_ele_smp,               &
     &      intg_point_t_evo, k2, ele, jac_3d, fem_wk%sk6)
!
        if ( iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_velo.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix33(ele, rhs_tbl,                   &
     &        MHD_mat_tbls%fluid_q, k2, fem_wk%sk6,                     &
     &        MHD1_matrices%Vmat_MG_DJDS(0)%num_non0,                   &
     &        MHD1_matrices%Vmat_MG_DJDS(0)%aiccg)
        end if
!
        if ( iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_temp.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix11(ele, rhs_tbl,                   &
     &        MHD_mat_tbls%fluid_q, k2, fem_wk%sk6,                     &
     &        Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_composit .eq. id_Crank_nicolson_cmass        &
     &      .and. coef_light .gt. 0.0d0) then
          call add_skv1_to_crs_matrix11(ele, rhs_tbl,                   &
     &        MHD_mat_tbls%fluid_q, k2, fem_wk%sk6,                     &
     &        Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass           &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele, rhs_tbl, MHD_mat_tbls%full_conduct_q,                &
     &        k2, fem_wk%sk6, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_vect_p .eq. id_Crank_nicolson_cmass          &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele, rhs_tbl, MHD_mat_tbls%full_conduct_q,                &
     &        k2, fem_wk%sk6, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
        end if
      end do
!
      end subroutine int_vol_crank_mat_consist
!
! ----------------------------------------------------------------------
!
      end module int_vol_consist_evo_mat
