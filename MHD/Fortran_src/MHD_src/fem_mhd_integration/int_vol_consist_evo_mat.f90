!int_vol_consist_evo_mat.f90
!      module int_vol_consist_evo_mat
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine int_vol_crank_mat_consist
!
      module int_vol_consist_evo_mat
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_consist
!
      use m_control_parameter
      use m_geometry_data
      use m_physical_property
      use m_phys_constants
      use m_sorted_node_MHD
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
!
      use m_temp_matrix
      use m_light_element_matrix
      use m_velo_matrix
      use m_magne_matrix
!
      use fem_skv_mass_mat_type
      use cal_skv_to_ff_smp
      use add_skv1_to_crs_matrix
!
       integer(kind = kint) :: k2
!
!
      do  k2 = 1, ele1%nnod_4_ele
        call reset_sk6(n_scalar, ele1, fem1_wk%sk6)
!
        call fem_skv_mass_matrix_type(ele1%istack_ele_smp,              &
     &      intg_point_t_evo, k2, ele1, jac1_3d_q, fem1_wk%sk6)
!
        if ( iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_velo.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix33(ele1, rhs_tbl1, mat_tbl_fl_q,   &
     &        k2, fem1_wk%sk6, Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_temp.gt.0.0d0 ) then
          call add_skv1_to_crs_matrix11(ele1, rhs_tbl1, mat_tbl_fl_q,   &
     &        k2, fem1_wk%sk6, Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_composit .eq. id_Crank_nicolson_cmass        &
     &      .and. coef_light .gt. 0.0d0) then
          call add_skv1_to_crs_matrix11(ele1, rhs_tbl1, mat_tbl_fl_q,   &
     &        k2, fem1_wk%sk6, Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass           &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele1, rhs_tbl1, mat_tbl_full_cd_q,                        &
     &        k2, fem1_wk%sk6, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_vect_p .eq. id_Crank_nicolson_cmass          &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_to_crs_matrix33                                 &
     &       (ele1, rhs_tbl1, mat_tbl_full_cd_q,                        &
     &        k2, fem1_wk%sk6, Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
        end if
      end do
!
      end subroutine int_vol_crank_mat_consist
!
! ----------------------------------------------------------------------
!
      end module int_vol_consist_evo_mat
