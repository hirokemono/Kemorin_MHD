!
!      module int_vol_lumped_mat_crank
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2005
!
!      subroutine int_vol_crank_mat_lump
!
      module int_vol_lumped_mat_crank
!
      use m_precision
      use m_constants
!
      use m_geometry_parameter
      use m_geometry_data_MHD
      use m_finite_element_matrix
      use init_djds_matrix_lump
!
      implicit none
!
      private :: init_velo_matrix_lump, init_temp_matrix_lump
      private :: init_magne_matrix_lump, init_vect_p_matrix_lump
      private :: init_d_scalar_matrix_lump
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine int_vol_crank_mat_lump
!
      use m_physical_property
      use m_control_parameter
!
!$omp parallel
      if (iflag_t_evo_4_velo .eq. id_Crank_nicolson                     &
     &     .and. coef_velo .gt. zero) then
        call init_velo_matrix_lump
      end if
!
      if (iflag_t_evo_4_temp .eq. id_Crank_nicolson                     &
     &     .and. coef_temp .gt. zero) then
        call init_temp_matrix_lump
      end if
!
      if (iflag_t_evo_4_magne .eq. id_Crank_nicolson                    &
     &     .and. coef_magne .gt. zero) then
        call init_magne_matrix_lump
      end if
!
       if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson                  &
     &     .and. coef_magne .gt. zero) then
        call init_vect_p_matrix_lump
       end if
!
      if (iflag_t_evo_4_composit .eq. id_Crank_nicolson                 &
     &     .and. coef_light .gt. zero) then
        call init_d_scalar_matrix_lump
      end if
!$omp end parallel
!
      end subroutine int_vol_crank_mat_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_velo_matrix_lump
!
      use m_solver_djds_fluid
      use m_velo_matrix
!
!
      call init_33_matrix_lump(numnod, numnod_fluid, inod_fluid,        &
     &    OLDtoNEW, ml_o_fl, Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
!
      end subroutine init_velo_matrix_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_temp_matrix_lump
!
      use m_solver_djds_fluid
      use m_temp_matrix
!
!
      call init_11_matrix_lump(numnod, numnod_fluid, inod_fluid,        &
     &    OLDtoNEW, ml_o_fl, num_temp_comp, aiccg_temp)
!
      end subroutine init_temp_matrix_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_magne_matrix_lump
!
      use m_solver_djds
      use m_magne_matrix
!
!
      call init_33_matrix_lump(numnod, numnod_conduct, inod_conduct,    &
     &    OLDtoNEW, ml_o_cd, num_mag_comp, aiccg_magne)
!
      end subroutine init_magne_matrix_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_vect_p_matrix_lump
!
      use m_solver_djds
      use m_magne_matrix
!
!
      call init_33_matrix_lump(numnod, numnod_conduct, inod_conduct,    &
     &    OLDtoNEW, ml_o_cd, num_mag_comp, aiccg_magne)
!
      end subroutine init_vect_p_matrix_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_d_scalar_matrix_lump
!
      use m_solver_djds_fluid
      use m_light_element_matrix
!
!
      call init_11_matrix_lump(numnod, numnod_fluid, inod_fluid,        &
     &    OLDtoNEW, ml_o_fl, num_composit_comp, aiccg_composit)
!
      end subroutine init_d_scalar_matrix_lump
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_lumped_coriolis_matrix
!
      use m_solver_djds_fluid
      use m_physical_property
      use m_velo_matrix
!
      use cal_coriolis_mat33
!
      call cal_lumped_coriolis_matrix(numnod, numnod_fluid, inod_fluid, &
     &    OLDtoNEW, coef_cor, angular, ml_o_fl,                         &
     &    Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
!
      end subroutine add_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!
      end module int_vol_lumped_mat_crank
