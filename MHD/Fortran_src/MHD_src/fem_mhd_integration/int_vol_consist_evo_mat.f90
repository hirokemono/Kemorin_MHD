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
      use m_geometry_parameter
      use m_physical_property
      use m_phys_constants
      use m_sorted_node_MHD
      use m_finite_element_matrix
!
      use m_temp_matrix
      use m_light_element_matrix
      use m_velo_matrix
      use m_magne_matrix
!
      use fem_skv_mass_mat_1st
      use cal_poisson_matrices_1st
!
       integer(kind = kint) :: k2
!
!
      do  k2 = 1, nnod_4_ele
        call reset_sk6(n_scalar)
!
        call fem_skv_mass_matrix_1st(iele_smp_stack,                    &
     &      intg_point_t_evo, k2, sk6)
!
        if ( iflag_t_evo_4_velo .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_velo.gt.0.0d0 ) then
          call add_skv1_2_MHD_matrix33(idx_4_fl_mat, k2, sk6,           &
     &        Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
        end if
!
        if ( iflag_t_evo_4_temp .eq. id_Crank_nicolson_cmass            &
     &      .and. coef_temp.gt.0.0d0 ) then
          call add_skv1_2_MHD_matrix11(idx_4_fl_mat, k2, sk6,           &
     &        num_temp_comp, aiccg_temp)
        end if
!
        if ( iflag_t_evo_4_composit .eq. id_Crank_nicolson_cmass        &
     &      .and. coef_light .gt. 0.0d0) then
          call add_skv1_2_MHD_matrix11(idx_4_fl_mat, k2, sk6,           &
     &        num_composit_comp, aiccg_composit)
        end if
!
        if ( iflag_t_evo_4_magne .eq. id_Crank_nicolson_cmass           &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_2_MHD_matrix33(idx_4_cd_mat_full, k2, sk6,      &
     &        num_mag_comp, aiccg_magne)
        end if
!
        if ( iflag_t_evo_4_vect_p .eq. id_Crank_nicolson_cmass          &
     &      .and. coef_magne.gt.0.0d0) then
          call add_skv1_2_MHD_matrix33(idx_4_cd_mat_full, k2, sk6,      &
     &        num_mag_comp, aiccg_magne)
        end if
      end do
!
      end subroutine int_vol_crank_mat_consist
!
! ----------------------------------------------------------------------
!
      end module int_vol_consist_evo_mat
