!>@file   back_sph_trans_4_all_field.f90
!!@brief  module back_sph_trans_4_all_field
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine init_sph_back_transform                              &
!!     &         (SPH_model, trans_p, WK, SPH_MHD)
!!        type(SPH_MHD_model_data), intent(in) :: SPH_model
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!      subroutine sph_all_back_transform(sph, comms_sph, trans_p,      &
!!     &           rj_fld, trns_MHD, WK_sph)
!!      subroutine sph_back_transform_dual(sph, comms_sph, trans_p,     &
!!     &          ref_rj_fld, rj_fld, trns_MHD, WK_sph,                 &
!!     &          nnod_rtp, ncomp_rtp, fld1_rtp)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(phys_data), intent(in) :: rj_fld
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(spherical_trns_works), intent(inout) :: WK_sph
!!@endverbatim
!
      module back_sph_trans_4_all_field
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_work_4_sph_trans
      use t_sph_trans_arrays_MHD
      use t_sph_transforms
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_sph_back_transform                                &
     &         (SPH_model, trans_p, WK, SPH_MHD)
!
      use t_physical_property
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_sph_multi_FFTW
!
      use set_address_sph_trans_MHD
      use init_sphrical_transform_MHD
      use set_address_all_sph_trans
!
      type(SPH_MHD_model_data), intent(in) :: SPH_model
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans
!
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_all_spherical_transform'
      call set_all_spherical_transform(SPH_MHD%fld, WK%trns_MHD,        &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      call alloc_sph_trans_address(SPH_MHD%sph%sph_rtp, WK)
!
      call init_leg_fourier_trans_MHD                                   &
     &   (SPH_model%sph_MHD_bc, SPH_MHD%sph, SPH_MHD%comms,             &
     &    ncomp_max_trans, trans_p, WK)
!
      call sel_sph_transform_MHD                                        &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_model%omega_sph,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%WK_sph, trans_p, WK%gt_cor, WK%cor_rlm,       &
     &    SPH_MHD%fld)
!
      end subroutine init_sph_back_transform
!
!-----------------------------------------------------------------------
!
      subroutine sph_all_back_transform(sph, comms_sph, trans_p,        &
     &           rj_fld, trns_MHD, WK_sph)
!
      use m_solver_SR
      use spherical_SRs_N
!
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_sph_multi_FFTW
!
      use m_legendre_transform_list
      use copy_all_trans_send_recv
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: rj_fld
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(trns_MHD%backward%ncomp .le. 0) return
!
      nscalar_trans = trns_MHD%backward%num_scalar                      &
     &               + 6*trns_MHD%backward%num_tensor
      call check_calypso_sph_comm_buf_N(trns_MHD%backward%ncomp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%backward%ncomp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_all_spectr_to_send                                      &
     &   (sph%sph_rtp%nnod_pole, trns_MHD%backward%ncomp,               &
     &    sph%sph_rj, comms_sph%comm_rj, rj_fld, trns_MHD%backward,     &
     &    SR_r1%n_WS, SR_r1%WS, trns_MHD%backward%flc_pole)
!
      call sph_b_trans_w_poles                                          &
     &   (trns_MHD%backward%ncomp, trns_MHD%backward%num_vector,        &
     &    nscalar_trans, sph, comms_sph, trans_p,                       &
     &    SR_r1%n_WS, SR_r1%n_WR, SR_r1%WS(1), SR_r1%WR(1),             &
     &    trns_MHD%backward%fld_rtp, trns_MHD%backward%flc_pole,        &
     &    trns_MHD%backward%fld_pole, WK_sph)
!
      end subroutine sph_all_back_transform
!
!-----------------------------------------------------------------------
!
      subroutine sph_back_transform_dual(sph, comms_sph, trans_p,       &
     &          ref_rj_fld, rj_fld, trns_MHD, WK_sph,                   &
     &          nnod_rtp, ncomp_rtp, fld1_rtp)
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_data), intent(in) :: ref_rj_fld
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: nnod_rtp, ncomp_rtp
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(spherical_trns_works), intent(inout) :: WK_sph
!
      real(kind = kreal), intent(inout) :: fld1_rtp(nnod_rtp,ncomp_rtp)
!
!       Transform first data
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph, comms_sph, trans_p,              &
     &    ref_rj_fld, trns_MHD, WK_sph)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!$omp parallel workshare
      fld1_rtp(1:nnod_rtp,1:ncomp_rtp)                                  &
           = trns_MHD%backward%fld_rtp(1:nnod_rtp,1:ncomp_rtp)
!$omp end parallel workshare
!
!       Transform second data
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_MHD, WK_sph)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
      end subroutine sph_back_transform_dual
!
! ----------------------------------------------------------------------
!
      end module back_sph_trans_4_all_field
