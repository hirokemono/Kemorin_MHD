!>@file   init_sphrical_transform_MHD.f90
!!@brief  module init_sphrical_transform_MHD
!!
!!@date  Programmed by H.Matsui on Oct., 2009
!!@n     Modified by H.Matsui on March., 2013
!
!>@brief Perform spherical harmonics transform for MHD dynamo model
!!
!!@verbatim
!!      subroutine init_sph_transform_MHD(ipol, idpdr, itor, iphys,     &
!!     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!!        type(phys_address), intent(in) :: ipol, idpdr, itor
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(parameters_4_sph_trans), intent(inout) :: trans_p
!!        type(works_4_sph_trans_MHD), intent(inout) :: WK
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
      module init_sphrical_transform_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_work_time
!
      use calypso_mpi
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_phys_address
      use t_phys_data
      use t_addresses_sph_transform
      use t_poloidal_rotation
      use t_sph_trans_arrays_MHD
      use t_schmidt_poly_on_rtm
      use t_work_4_sph_trans
      use t_sph_multi_FFTW
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
      subroutine init_sph_transform_MHD(ipol, idpdr, itor, iphys,       &
     &          sph, comms_sph, omega_sph, trans_p, WK, rj_fld)
!
      use m_control_parameter
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_snap
      use set_address_sph_trans_tmp
      use pole_sph_transform
      use MHD_FFT_selector
!
      type(phys_address), intent(in) :: ipol, idpdr, itor
      type(phys_address), intent(in) :: iphys
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
!
      call init_pole_transform(sph%sph_rtp)
!
      if (iflag_debug .ge. iflag_routine_msg) write(*,*)                &
     &                     'set_addresses_trans_sph_MHD'
      call set_addresses_trans_sph_MHD(ipol, WK%trns_MHD,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_SGS(ipol, WK%trns_SGS,               &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_trans_sph_Csim(ipol, WK%trns_Csim,             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_snapshot_trans(ipol, WK%trns_snap,             &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
      call set_addresses_temporal_trans(ipol, WK%trns_tmp,              &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans)
!
      if(iflag_debug .ge. iflag_routine_msg) then
        call check_address_trans_sph_MHD(ipol, idpdr, itor, iphys,      &
     &      WK%trns_MHD, ncomp_max_trans)
        call check_address_trans_sph_SGS(ipol, idpdr, itor, iphys,      &
     &      WK%trns_SGS)
        call check_address_trans_sph_Csim(ipol, idpdr, itor, iphys,     &
     &      WK%trns_Csim)
        call check_address_trans_sph_snap(ipol, idpdr, itor, iphys,     &
     &      WK%trns_snap)
        call check_address_trans_sph_tmp(ipol, idpdr, itor, iphys,      &
     &      WK%trns_tmp)
      end if
!
      call alloc_sph_trans_address(sph%sph_rtp, WK)
!
      call sel_sph_transform_MHD(ipol, sph, comms_sph, omega_sph,       &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    trans_p, WK, rj_fld)
!
      if(iflag_SGS_model .gt. 0) then
        call init_MHD_FFT_select(my_rank, sph%sph_rtp, ncomp_max_trans, &
     &      WK%trns_SGS%ncomp_rtp_2_rj,                                 &
     &      WK%trns_SGS%ncomp_rj_2_rtp, WK%SGS_mul_FFTW)
      end if
!
      end subroutine init_sph_transform_MHD
!
!-----------------------------------------------------------------------
!
      subroutine sel_sph_transform_MHD                                  &
     &         (ipol, sph, comms_sph, omega_sph,                        &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          trans_p, WK, rj_fld)
!
      use init_sph_trans
      use init_FFT_4_MHD
      use set_address_sph_trans_MHD
      use set_address_sph_trans_SGS
      use set_address_sph_trans_snap
      use set_address_sph_trans_tmp
      use const_wz_coriolis_rtp
      use const_coriolis_sph_rlm
      use pole_sph_transform
      use skip_comment_f
!
      type(phys_address), intent(in) :: ipol
!
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(parameters_4_sph_trans), intent(inout) :: trans_p
      type(works_4_sph_trans_MHD), intent(inout) :: WK
      type(phys_data), intent(inout) :: rj_fld
!
      character(len=kchara) :: tmpchara
!
!
      if (iflag_debug.eq.1) write(*,*) 'initialize_legendre_trans'
      call initialize_legendre_trans(ncomp_max_trans,                   &
     &    sph, comms_sph, trans_p%leg, trans_p%idx_trns)
      call init_fourier_transform_4_MHD(ncomp_max_trans,                &
     &    sph%sph_rtp, comms_sph%comm_rtp, WK%trns_MHD,                 &
     &    WK%MHD_mul_FFTW)
!
      if (iflag_debug.eq.1) write(*,*) 'set_colatitude_rtp'
      call set_colatitude_rtp(sph%sph_rtp, sph%sph_rj, trans_p%leg)
      if (iflag_debug.eq.1) write(*,*) 'init_sum_coriolis_rlm'
      call init_sum_coriolis_rlm                                        &
     &   (sph%sph_params%l_truncation, sph%sph_rlm, trans_p%leg)
!
      if (iflag_debug.eq.1) write(*,*) 'select_legendre_transform'
      call select_legendre_transform                                    &
     &   (sph, comms_sph, omega_sph, trans_p, ipol,                     &
     &    ncomp_max_trans, nvector_max_trans, nscalar_max_trans,        &
     &    rj_fld, WK%trns_MHD, WK%MHD_mul_FFTW)
!
      call sel_init_legendre_trans                                      &
     &    (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,       &
     &     sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns)
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
      end subroutine sel_sph_transform_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine select_legendre_transform                              &
     &         (sph, comms_sph, omega_sph, trans_p, ipol,               &
     &          ncomp_max_trans, nvector_max_trans, nscalar_max_trans,  &
     &          rj_fld, trns_MHD, MHD_mul_FFTW)
!
      use sph_transforms_4_MHD
!
      type(sph_grids), intent(in) :: sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(sph_rotation), intent(in) :: omega_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
      type(phys_address), intent(in) :: ipol
!
      integer(kind = kint), intent(in) :: ncomp_max_trans
      integer(kind = kint), intent(in) :: nvector_max_trans
      integer(kind = kint), intent(in) :: nscalar_max_trans
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(work_for_sgl_FFTW), intent(inout) :: MHD_mul_FFTW
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
      if(id_legendre_transfer .ne. iflag_leg_undefined) return
!
      endtime(1:ntype_Leg_trans_loop) =     zero
      etime_trans(1:ntype_Leg_trans_loop) = zero
      etime_max(1:ntype_Leg_trans_loop) =   zero
      do iloop_type = 1, num_test
        id_legendre_transfer = list_test(iloop_type)
        if(my_rank .eq. 0) write(*,*)                                   &
     &            'Test SPH transform for ', id_legendre_transfer
        call sel_init_legendre_trans                                    &
     &     (ncomp_max_trans, nvector_max_trans, nscalar_max_trans,      &
     &      sph%sph_rtm, sph%sph_rlm, trans_p%leg, trans_p%idx_trns)
!
        starttime = MPI_WTIME()
        call sph_back_trans_4_MHD(sph, comms_sph, omega_sph,            &
     &      trans_p, ipol, rj_fld, trns_MHD, MHD_mul_FFTW)
        call sph_forward_trans_4_MHD(sph, comms_sph, trans_p,           &
     &      ipol, trns_MHD, MHD_mul_FFTW, rj_fld)
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
      end module init_sphrical_transform_MHD
