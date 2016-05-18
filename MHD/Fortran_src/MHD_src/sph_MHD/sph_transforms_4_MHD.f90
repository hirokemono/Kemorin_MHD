!>@file   sph_transforms_4_MHD.f90
!!@brief  module sph_transforms_4_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD(sph, comms_sph, rj_fld)
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sph_back_trans_4_MHD(sph, comms_sph, rj_fld)
!!      subroutine sph_forward_trans_4_MHD(sph, comms_sph, rj_fld)
!!
!!      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, rj_fld)
!!      subroutine sph_forward_trans_snapshot_MHD                       &
!!     &         (sph, comms_sph, rj_fld)
!!
!!      subroutine sph_forward_trans_tmp_snap_MHD                       &
!!     &         (sph, comms_sph, rj_fld)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine sph_transform_4_licv(rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module sph_transforms_4_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_data
!
      use legendre_transform_select
!
      implicit  none
!
      integer(kind = kint), parameter :: num_test =  6
      integer(kind = kint), parameter :: list_test(num_test)            &
     &        = (/iflag_leg_krloop_inner,                               &
     &            iflag_leg_sym_spin_loop,                              &
     &            iflag_leg_sym_matmul,                                 &
     &            iflag_leg_sym_dgemm,                                  &
     &            iflag_leg_sym_matmul_big,                             &
     &            iflag_leg_sym_dgemm_big/)
!
      private :: num_test, list_test
      private :: select_legendre_transform
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_transform_MHD(sph, comms_sph, rj_fld)
!
      use calypso_mpi
      use m_addresses_trans_sph_MHD
      use m_addresses_trans_sph_snap
      use m_addresses_trans_sph_tmp
      use m_work_4_sph_trans
!
      use init_sph_trans
      use init_FFT_4_MHD
      use set_address_sph_trans_MHD
      use const_wz_coriolis_rtp
      use const_coriolis_sph_rlm
      use pole_sph_transform
      use skip_comment_f
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara) :: tmpchara
!
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD(trns_MHD,                        &
     &    ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans)
      call set_addresses_snapshot_trans
      call set_addresses_temporal_trans
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(trns_MHD, ncomp_sph_trans)
        call check_addresses_snapshot_trans
        call check_addresses_temporal_trans
      end if
!
      call alloc_nonlinear_data(sph%sph_rtp%nnod_rtp, trns_MHD)
      call allocate_snap_trans_rtp(sph%sph_rtp%nnod_rtp)
      call allocate_tmp_trans_rtp(sph%sph_rtp%nnod_rtp)
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans(sph, comms_sph)
      call init_fourier_transform_4_MHD(ncomp_sph_trans,                &
     &    trns_MHD%ncomp_rtp_2_rj, trns_MHD%ncomp_rj_2_rtp,             &
     &    sph%sph_rtp, comms_sph%comm_rtp)
!
      if (iflag_debug.eq.1) write(*,*) 'set_colatitude_rtp'
      call set_colatitude_rtp(sph%sph_rtp, sph%sph_rj)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm                                        &
     &   (sph%sph_params%l_truncation, sph%sph_rlm)
!
      if(id_legendre_transfer .eq. iflag_leg_undefined) then
        if (iflag_debug.eq.1) write(*,*) 'select_legendre_transform'
        call select_legendre_transform(sph, comms_sph, rj_fld)
      end if
!
      call sel_init_legendre_trans                                      &
     &    (ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans,       &
     &     sph%sph_rtm, sph%sph_rlm)
!
      if(my_rank .ne. 0) return
        if     (id_legendre_transfer .eq. iflag_leg_orginal_loop) then
          write(tmpchara,'(a)') trim(leg_orginal_loop)
        else if(id_legendre_transfer .eq. iflag_leg_blocked) then
          write(tmpchara,'(a)') trim(leg_blocked_loop)
        else if(id_legendre_transfer .eq. iflag_leg_krloop_inner) then
          write(tmpchara,'(a)') trim(leg_krloop_inner)
        else if(id_legendre_transfer .eq. iflag_leg_krloop_outer) then
          write(tmpchara,'(a)') trim(leg_krloop_outer)
        else if(id_legendre_transfer .eq. iflag_leg_symmetry) then
          write(tmpchara,'(a)') trim(leg_sym_org_loop)
        else if(id_legendre_transfer .eq. iflag_leg_sym_spin_loop) then
          write(tmpchara,'(a)') trim(leg_sym_spin_loop)
        else if(id_legendre_transfer .eq. iflag_leg_matmul) then
          write(tmpchara,'(a)') trim(leg_matmul)
        else if(id_legendre_transfer .eq. iflag_leg_dgemm) then
          write(tmpchara,'(a)') trim(leg_dgemm)
        else if(id_legendre_transfer .eq. iflag_leg_matprod) then
          write(tmpchara,'(a)') trim(leg_matprod)
        else if(id_legendre_transfer .eq. iflag_leg_sym_matmul) then
          write(tmpchara,'(a)') trim(leg_sym_matmul)
        else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm) then
          write(tmpchara,'(a)') trim(leg_sym_dgemm)
        else if(id_legendre_transfer .eq. iflag_leg_sym_matprod) then
          write(tmpchara,'(a)') trim(leg_sym_matprod)
        else if(id_legendre_transfer .eq. iflag_leg_sym_matmul_big)     &
     &          then
          write(tmpchara,'(a)') trim(leg_sym_matmul_big)
        else if(id_legendre_transfer .eq. iflag_leg_sym_dgemm_big) then
          write(tmpchara,'(a)') trim(leg_sym_dgemm_big)
        else if(id_legendre_transfer .eq. iflag_leg_sym_matprod_big)    &
     &          then
          write(tmpchara,'(a)') trim(leg_sym_matprod_big)
        else if(id_legendre_transfer .eq. iflag_leg_test_loop) then
          write(tmpchara,'(a)') trim(leg_test_loop)
        end if
        call change_2_upper_case(tmpchara)
!
        write(*,'(a,i4)', advance='no')                                 &
     &         'Selected id_legendre_transfer: ', id_legendre_transfer
        write(*,'(a,a,a)') ' (', trim(tmpchara), ') '
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_4_MHD(sph, comms_sph, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_MHD
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(in) :: rj_fld
!
!
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rj_2_rtp,        &
     &   comms_sph%comm_rtm, comms_sph%comm_rtp)
!
!      call start_eleps_time(51)
      if(iflag_debug .gt. 0) write(*,*) 'copy_mhd_spectr_to_send'
      call copy_mhd_spectr_to_send                                      &
     &   (trns_MHD%ncomp_rj_2_rtp, trns_MHD%b_trns, comms_sph%comm_rj,  &
     &    rj_fld, n_WS, WS)
!      call end_eleps_time(51)
!
      if(trns_MHD%ncomp_rj_2_rtp .eq. 0) return
      call sph_b_trans_w_coriolis(trns_MHD%ncomp_rj_2_rtp,              &
     &    trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,         &
     &    sph, comms_sph, n_WS, n_WR, WS(1), WR(1), trns_MHD%fld_rtp)
!
      end subroutine sph_back_trans_4_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_4_MHD(sph, comms_sph, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_MHD
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N(trns_MHD%ncomp_rtp_2_rj,        &
     &    comms_sph%comm_rlm, comms_sph%comm_rj)
!
      if(trns_MHD%ncomp_rtp_2_rj .eq. 0) return
      call sph_f_trans_w_coriolis(trns_MHD%ncomp_rtp_2_rj,              &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%nscalar_rtp_2_rj,         &
     &    sph, comms_sph, trns_MHD%frc_rtp, n_WS, n_WR, WS(1), WR(1))
!
      call copy_mhd_spectr_from_recv(trns_MHD%ncomp_rtp_2_rj,           &
     &    trns_MHD%f_trns, comms_sph%comm_rj, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_4_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_back_trans_snapshot_MHD(sph, comms_sph, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_snap
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: nscalar_trans
!
!
      if(ncomp_snap_rj_2_rtp .le. 0) return
!
      nscalar_trans = nscalar_snap_rj_2_rtp + 6*ntensor_snap_rj_2_rtp
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_snap_rj_2_rtp, comms_sph%comm_rj, comms_sph%comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_snap_rj_2_rtp, comms_sph%comm_rtm, comms_sph%comm_rtp)
!
      call copy_snap_spectr_to_send(ncomp_snap_rj_2_rtp, bs_trns,       &
     &    sph%sph_rj, comms_sph%comm_rj, rj_fld, n_WS, WS, flc_pl)
!
      call sph_backward_transforms                                      &
     &   (ncomp_snap_rj_2_rtp, nvector_snap_rj_2_rtp, nscalar_trans,    &
     &    sph, comms_sph, n_WS, n_WR, WS(1), WR(1),                     &
     &    fls_rtp, flc_pl, fls_pl)
!
      end subroutine sph_back_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_snapshot_MHD                         &
     &         (sph, comms_sph, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_snap
      use m_work_4_sph_trans
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ncomp_snap_rtp_2_rj .le. 0) return
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_snap_rtp_2_rj, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_snap_rtp_2_rj, comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms(ncomp_snap_rtp_2_rj,                  &
     &    nvector_snap_rtp_2_rj, nscalar_snap_rtp_2_rj,                 &
     &    sph, comms_sph, frs_rtp, n_WS, n_WR, WS(1), WR(1))
!
      call copy_snap_vec_spec_from_trans                                &
     &   (ncomp_snap_rtp_2_rj, fs_trns,      &
     &    comms_sph%comm_rj, n_WR, WR(1), rj_fld)
!
      end subroutine sph_forward_trans_snapshot_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_forward_trans_tmp_snap_MHD                         &
     &         (sph, comms_sph, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_tmp
      use m_work_4_sph_trans
      use sph_transforms
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(ncomp_tmp_rtp_2_rj .eq. 0) return
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_tmp_rtp_2_rj, comms_sph%comm_rtp, comms_sph%comm_rtm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (ncomp_tmp_rtp_2_rj, comms_sph%comm_rlm, comms_sph%comm_rj)
!
!   transform for vectors and scalars
      call sph_forward_transforms(ncomp_tmp_rtp_2_rj,                   &
     &    nvector_tmp_rtp_2_rj, nscalar_tmp_rtp_2_rj,                   &
     &    sph, comms_sph, frt_rtp, n_WS, n_WR, WS, WR)
!
      call copy_tmp_scl_spec_from_trans                                 &
     &   (ncomp_tmp_rtp_2_rj, ft_trns,        &
     &    comms_sph%comm_rj, n_WR, WR, rj_fld)
!
      end subroutine sph_forward_trans_tmp_snap_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sph_transform_4_licv                                   &
     &         (sph_rlm, comm_rlm, comm_rj, rj_fld)
!
      use m_solver_SR
      use m_addresses_trans_sph_MHD
      use sph_trans_w_coriols
      use copy_sph_MHD_4_send_recv
      use spherical_SRs_N
!
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_comm_tbl), intent(in) :: comm_rlm
      type(sph_comm_tbl), intent(in) :: comm_rj
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(trns_MHD%ncomp_rj_2_rtp .eq. 0                                 &
     &   .or. trns_MHD%ncomp_rtp_2_rj .eq. 0) return
!
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%ncomp_rj_2_rtp, comm_rj, comm_rlm)
      call check_calypso_sph_comm_buf_N                                 &
     &   (trns_MHD%ncomp_rtp_2_rj, comm_rlm, comm_rj)
!
      call copy_mhd_spectr_to_send(trns_MHD%ncomp_rj_2_rtp,             &
     &   trns_MHD%b_trns, comm_rj, rj_fld, n_WS, WS(1))
!
      call sph_b_trans_licv(trns_MHD%ncomp_rj_2_rtp,                    &
     &    sph_rlm, comm_rlm, comm_rj, n_WR, WR(1))
      call sph_f_trans_licv(trns_MHD%ncomp_rtp_2_rj,                    &
     &    sph_rlm, comm_rlm, comm_rj, n_WS, WS(1))
!
      call copy_mhd_spectr_from_recv                                    &
     &   (trns_MHD%ncomp_rtp_2_rj, trns_MHD%f_trns, comm_rj,            &
     &    n_WR, WR(1), rj_fld)
!
      end subroutine sph_transform_4_licv
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine select_legendre_transform(sph, comms_sph, rj_fld)
!
      use calypso_mpi
      use m_machine_parameter
      use m_work_4_sph_trans
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
!
      type(phys_data), intent(inout) :: rj_fld
!
      real(kind = kreal) :: starttime, etime_shortest
      real(kind = kreal) :: endtime(ntype_Leg_trans_loop)
      real(kind = kreal) :: etime_trans(ntype_Leg_trans_loop)
      real(kind = kreal) :: etime_max(ntype_Leg_trans_loop)
!
      integer(kind = kint) :: id, iloop_type
!
!
      endtime(1:ntype_Leg_trans_loop) =     zero
      etime_trans(1:ntype_Leg_trans_loop) = zero
      etime_max(1:ntype_Leg_trans_loop) =   zero
      do iloop_type = 1, num_test
        id_legendre_transfer = list_test(iloop_type)
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'Test SPH transform for ', id_legendre_transfer
        call sel_init_legendre_trans                                    &
     &     (ncomp_sph_trans, nvector_sph_trans, nscalar_sph_trans,      &
     &      sph%sph_rtm, sph%sph_rlm)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, rj_fld)
        call sph_forward_trans_4_MHD(sph, comms_sph, rj_fld)
        endtime(id_legendre_transfer) = MPI_WTIME() - starttime
!
        call sel_finalize_legendre_trans
      end do
!
      call MPI_allREDUCE (endtime, etime_trans, ntype_Leg_trans_loop,   &
     &    CALYPSO_REAL, MPI_SUM, CALYPSO_COMM, ierr_MPI)
      call MPI_allREDUCE (endtime, etime_max, ntype_Leg_trans_loop,     &
     &    CALYPSO_REAL, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      etime_trans(1:ntype_Leg_trans_loop)                               &
     &      = etime_trans(1:ntype_Leg_trans_loop) / dble(nprocs)
!
      etime_shortest =  1.0d30
      do iloop_type = 1, num_test
        id = list_test(iloop_type)
        if(etime_max(id) .lt. etime_shortest) then
          id_legendre_transfer = id
          etime_shortest =       etime_max(id)
        end if
      end do
!
      if(my_rank .gt. 0) return
        write(*,'(a)') 'Loop ID: type, maximum time, average time'
        if(etime_trans(iflag_leg_orginal_loop) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 1: elapsed by original loop:      ',  &
     &            etime_max(iflag_leg_orginal_loop),  &
     &            etime_trans(iflag_leg_orginal_loop)
        if(etime_trans(iflag_leg_blocked) .gt. zero)                    &
     &  write(*,'(a,1p2e16.6)') ' 2: elapsed by blocked loop:      ',   &
     &            etime_max(iflag_leg_blocked),                         &
     &            etime_trans(iflag_leg_blocked)
        if(etime_trans(iflag_leg_krloop_inner) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 3: elapsed by inner radius loop:  ',  &
     &            etime_max(iflag_leg_krloop_inner),                    &
     &            etime_trans(iflag_leg_krloop_inner)
        if(etime_trans(iflag_leg_krloop_outer) .gt. zero)               &
     &  write(*,'(a,1p2e16.6)') ' 4: elapsed by outer radius loop:  ',  &
     &            etime_max(iflag_leg_krloop_outer),                    &
     &            etime_trans(iflag_leg_krloop_outer)
        if(etime_trans(iflag_leg_symmetry) .gt. zero)                   &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          ' 5: elapsed by original loop with symmetric: ',        &
     &            etime_max(iflag_leg_symmetry),                        &
     &            etime_trans(iflag_leg_symmetry)
        if(etime_trans(iflag_leg_sym_spin_loop) .gt. zero)              &
     &  write(*,'(a,1p2e16.6)') ' 6: elapsed by sym. outer radius: ',   &
     &            etime_max(iflag_leg_sym_spin_loop),                   &
     &            etime_trans(iflag_leg_sym_spin_loop)
        if(etime_trans(iflag_leg_matmul) .gt. zero)                     &
     &  write(*,'(a,1p2e16.6)') ' 7: elapsed by matmul: ',              &
     &            etime_max(iflag_leg_matmul),                          &
     &            etime_trans(iflag_leg_matmul)
        if(etime_trans(iflag_leg_dgemm) .gt. zero)                      &
     &  write(*,'(a,1p2e16.6)') ' 8: elapsed by BLAS: ',                &
     &            etime_max(iflag_leg_dgemm),                           &
     &            etime_trans(iflag_leg_dgemm)
        if(etime_trans(iflag_leg_matprod) .gt. zero)                    &
     &  write(*,'(a,1p2e16.6)') ' 9: elapsed by matrix product: ',      &
     &            etime_max(iflag_leg_matprod),                         &
     &            etime_trans(iflag_leg_matprod)
        if(etime_trans(iflag_leg_sym_matmul) .gt. zero)                 &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '10: elapsed by matmul with symmetric: ',               &
     &            etime_max(iflag_leg_sym_matmul),                      &
     &            etime_trans(iflag_leg_sym_matmul)
        if(etime_trans(iflag_leg_sym_dgemm) .gt. zero)                  &
     &  write(*,'(a,1p2e16.6)') '11: elapsed by BLAS with symmetric: ', &
     &            etime_max(iflag_leg_sym_dgemm),                       &
     &            etime_trans(iflag_leg_sym_dgemm)
        if(etime_trans(iflag_leg_sym_matprod) .gt. zero)                &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '12: elapsed by matrix prod. with symm.: ',             &
     &            etime_max(iflag_leg_sym_matprod),                     &
     &            etime_trans(iflag_leg_sym_matprod)
        if(etime_trans(iflag_leg_sym_matmul_big) .gt. zero)             &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '13: elapsed by big matmul with symmetric: ',           &
     &            etime_max(iflag_leg_sym_matmul_big),                  &
     &            etime_trans(iflag_leg_sym_matmul_big)
        if(etime_trans(iflag_leg_sym_dgemm_big) .gt. zero)              &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '14: elapsed by big BLAS with symmetric: ',             &
     &            etime_max(iflag_leg_sym_dgemm_big),                   &
     &            etime_trans(iflag_leg_sym_dgemm_big)
        if(etime_trans(iflag_leg_sym_matprod_big) .gt. zero)            &
     &  write(*,'(a,1p2e16.6)')                                         &
     &          '15: elapsed by big matrix prod. with symm.: ',         &
     &            etime_max(iflag_leg_sym_matprod_big),                 &
     &            etime_trans(iflag_leg_sym_matprod_big)
!
      end subroutine select_legendre_transform
!
!-----------------------------------------------------------------------
!
      end module sph_transforms_4_MHD
