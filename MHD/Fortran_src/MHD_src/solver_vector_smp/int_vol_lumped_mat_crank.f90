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
!!      subroutine int_vol_crank_mat_lump
!!      subroutine add_lumped_coriolis_matrix
!!@endverbatim
!
      module int_vol_lumped_mat_crank
!
      use m_precision
      use m_constants
!
      use m_geometry_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
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
      subroutine int_vol_crank_mat_lump
!
      use m_physical_property
      use m_control_parameter
!
      use m_solver_djds_MHD
      use m_velo_matrix
      use m_magne_matrix
      use m_temp_matrix
      use m_light_element_matrix
      use m_int_vol_data
!
!$omp parallel
      if (iflag_t_evo_4_velo .eq. id_Crank_nicolson                     &
     &     .and. coef_velo .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node1%numnod, numnod_fluid, inod_fluid,                     &
     &      DJDS_fluid%OLDtoNEW, mhd_fem1_wk%mlump_fl%ml_o,             &
     &      Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_temp .eq. id_Crank_nicolson                     &
     &     .and. coef_temp .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (node1%numnod, numnod_fluid, inod_fluid,                     &
     &      DJDS_fluid%OLDtoNEW, mhd_fem1_wk%mlump_fl%ml_o,             &
     &      Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_magne .eq. id_Crank_nicolson                    &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node1%numnod, numnod_conduct, inod_conduct,                 &
     &      DJDS_entire%OLDtoNEW, mhd_fem1_wk%mlump_cd%ml_o,            &
     &      Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson                   &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node1%numnod, numnod_conduct, inod_conduct,                 &
     &      DJDS_entire%OLDtoNEW, mhd_fem1_wk%mlump_cd%ml_o,            &
     &      Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_composit .eq. id_Crank_nicolson                 &
     &     .and. coef_light .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (node1%numnod, numnod_fluid, inod_fluid,                     &
     &      DJDS_fluid%OLDtoNEW, mhd_fem1_wk%mlump_fl%ml_o,             &
     &      Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
      end if
!$omp end parallel
!
      end subroutine int_vol_crank_mat_lump
!
! ----------------------------------------------------------------------
!
      subroutine add_lumped_coriolis_matrix
!
      use m_solver_djds_MHD
      use m_physical_property
      use m_velo_matrix
      use m_int_vol_data
!
      use cal_coriolis_mat33
!
      call cal_lumped_coriolis_matrix(node1%numnod, numnod_fluid,       &
     &    inod_fluid, DJDS_fluid%OLDtoNEW, coef_cor, angular,           &
     &    mhd_fem1_wk%mlump_fl%ml_o,                                    &
     &    Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
!
      end subroutine add_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!
      end module int_vol_lumped_mat_crank
