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
!!     &         (fl_prop, sph_MHD_bc, ipol, idpdr, itor, iphys,        &
!!     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(phys_address), intent(in) :: iphys
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_rotation), intent(in) :: omega_sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
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
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_work_4_sph_trans
      use t_addresses_sph_transform
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
     &         (fl_prop, sph_MHD_bc, ipol, idpdr, itor, iphys,          &
     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!
      use t_physical_property
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_sph_multi_FFTW
!
      use set_address_sph_trans_MHD
      use pole_sph_transform
      use MHD_FFT_selector
      use init_sphrical_transform_MHD
      use set_address_all_sph_trans
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
      type(fluid_property), intent(in) :: fl_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
!>      total number of components for spherical harmonics transform
      integer(kind = kint), save :: ncomp_max_trans
!>      total number of vectors for spherical harmonics transform
      integer(kind = kint), save :: nvector_max_trans
!>      total number of svalars for spherical harmonics transform
      integer(kind = kint), save :: nscalar_max_trans
!
      integer(kind = kint) :: i_fld
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_backward_trans'
      call set_addresses_backward_trans(rj_fld, WK%trns_MHD,            &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(ipol, idpdr, itor, iphys,      &
     &      WK%trns_MHD, ncomp_max_trans)
        do i_fld = 1, rj_fld%num_phys_viz
          write(*,*) i_fld, trim(WK%trns_MHD%b_trns_name(i_fld))
        end do
      end if
!
      call alloc_sph_trans_address(sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD(izero, ipol, fl_prop,                  &
     &    sph_MHD_bc%sph_bc_U, sph, comms_sph, omega_sph,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    WK%trns_MHD, WK%trns_SGS, WK%WK_sph, WK%MHD_mul_FFTW,         &
     &    WK%SGS_mul_FFTW, trans_p, WK%gt_cor, WK%cor_rlm, rj_fld)
!
      end subroutine init_sph_back_transform
!
!-----------------------------------------------------------------------
!
      subroutine sph_all_back_transform(sph, comms_sph, trans_p,        &
     &           rj_fld, trns_MHD, WK_sph)
!
      use m_solver_SR
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_sph_multi_FFTW
!
      use m_legendre_transform_list
      use copy_all_trans_send_recv
      use sph_mhd_rms_IO
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
      if(trns_MHD%ncomp_rj_2_rtp .le. 0) return
!
      nscalar_trans = trns_MHD%nscalar_rj_2_rtp                         &
     &               + 6*trns_MHD%ntensor_rj_2_rtp
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_all_spectr_to_send                                      &
     &   (sph%sph_rtp%nnod_pole, trns_MHD%ncomp_rj_2_rtp,               &
     &    sph%sph_rj, comms_sph%comm_rj, rj_fld, trns_MHD,              &
     &    n_WS, WS, trns_MHD%flc_pole)
!
      call sph_b_trans_w_poles                                          &
     &   (trns_MHD%ncomp_rj_2_rtp, trns_MHD%nvector_rj_2_rtp,           &
     &    nscalar_trans, sph, comms_sph, trans_p,                       &
     &    n_WS, n_WR, WS(1), WR(1), trns_MHD%fld_rtp,                   &
     &    trns_MHD%flc_pole, trns_MHD%fld_pole, WK_sph)
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
      call start_elapsed_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph, comms_sph, trans_p,              &
     &    ref_rj_fld, trns_MHD, WK_sph)
      call end_elapsed_time(9)
!
!$omp parallel workshare
      fld1_rtp(1:nnod_rtp,1:ncomp_rtp)                                  &
           = trns_MHD%fld_rtp(1:nnod_rtp,1:ncomp_rtp)
!$omp end parallel workshare
!
!       Transform second data
!
      call start_elapsed_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform(sph, comms_sph, trans_p,              &
     &    rj_fld, trns_MHD, WK_sph)
      call end_elapsed_time(9)
!
      end subroutine sph_back_transform_dual
!
! ----------------------------------------------------------------------
!
      end module back_sph_trans_4_all_field
