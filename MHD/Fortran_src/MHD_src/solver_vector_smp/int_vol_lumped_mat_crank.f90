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
!!      subroutine int_vol_crank_mat_lump                               &
!!     &         (node, fluid, conduct, mhd_fem_wk)
!!      subroutine add_lumped_coriolis_matrix(node, fluid,              &
!!     &          DJDS_fluid, mhd_fem_wk, Vmat_DJDS)
!!        type(node_data), intent(in) :: node
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(DJDS_ordering_table), intent(in) :: DJDS_fluid
!!        type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!!        type(DJDS_MATRIX), intent(inout) :: Vmat_DJDS
!!@endverbatim
!
      module int_vol_lumped_mat_crank
!
      use m_precision
      use m_constants
!
      use m_physical_property
!
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
      subroutine int_vol_crank_mat_lump                                 &
     &         (node, fluid, conduct, mhd_fem_wk)
!
      use m_control_parameter
!
      use m_solver_djds_MHD
!
      type(node_data), intent(in) :: node
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
!
!
!
!$omp parallel
      if (iflag_t_evo_4_velo .eq. id_Crank_nicolson                     &
     &     .and. coef_velo .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node%numnod, fluid%numnod_fld, fluid%inod_fld,              &
     &      DJDS_fluid%OLDtoNEW, mhd_fem_wk%mlump_fl%ml_o,              &
     &      Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_temp .eq. id_Crank_nicolson                     &
     &     .and. coef_temp .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (node%numnod, fluid%numnod_fld, fluid%inod_fld,              &
     &      DJDS_fluid%OLDtoNEW, mhd_fem_wk%mlump_fl%ml_o,              &
     &      Tmat_DJDS%num_non0, Tmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_magne .eq. id_Crank_nicolson                    &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node%numnod, conduct%numnod_fld, conduct%inod_fld,          &
     &      DJDS_entire%OLDtoNEW, mhd_fem_wk%mlump_cd%ml_o,             &
     &      Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_vect_p .eq. id_Crank_nicolson                   &
     &     .and. coef_magne .gt. zero) then
        call init_33_matrix_lump                                        &
     &     (node%numnod, conduct%numnod_fld, conduct%inod_fld,          &
     &      DJDS_entire%OLDtoNEW, mhd_fem_wk%mlump_cd%ml_o,             &
     &      Bmat_DJDS%num_non0, Bmat_DJDS%aiccg)
      end if
!
      if (iflag_t_evo_4_composit .eq. id_Crank_nicolson                 &
     &     .and. coef_light .gt. zero) then
        call init_11_matrix_lump                                        &
     &     (node%numnod, fluid%numnod_fld, fluid%inod_fld,              &
     &      DJDS_fluid%OLDtoNEW, mhd_fem_wk%mlump_fl%ml_o,              &
     &      Cmat_DJDS%num_non0, Cmat_DJDS%aiccg)
      end if
!$omp end parallel
!
      end subroutine int_vol_crank_mat_lump
!
! ----------------------------------------------------------------------
!
      subroutine add_lumped_coriolis_matrix(node, fluid,                &
     &          DJDS_fluid, mhd_fem_wk, Vmat_DJDS)
!
      use cal_coriolis_mat33
!
      type(node_data), intent(in) :: node
      type(field_geometry_data), intent(in) :: fluid
!
      type(DJDS_ordering_table), intent(in) :: DJDS_fluid
      type(work_MHD_fe_mat), intent(in) :: mhd_fem_wk
      type(DJDS_MATRIX), intent(inout) :: Vmat_DJDS
!
!
      call cal_lumped_coriolis_matrix(node%numnod, fluid%numnod_fld,    &
     &    fluid%inod_fld, DJDS_fluid%OLDtoNEW, coef_cor, angular,       &
     &    mhd_fem_wk%mlump_fl%ml_o,                                     &
     &    Vmat_DJDS%num_non0, Vmat_DJDS%aiccg)
!
      end subroutine add_lumped_coriolis_matrix
!
! ----------------------------------------------------------------------
!
      end module int_vol_lumped_mat_crank
