!cmp_trans_sph_indices.f90
!      module cmp_trans_sph_indices
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_idx_sph_recieve
!      subroutine deallocate_idx_sph_recieve
!
!!      subroutine sph_indices_transfer                                 &
!!     &          (itype, nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,        &
!!     &           idx_global_rtp, idx_global_rtm,                      &
!!     &           idx_global_rlm, idx_global_rj)
!!      subroutine compare_transfer_sph_indices(id_check)
!!     &          sph_rtp, sph_rtm, sph_rlm, sph_rj,                    &
!!      subroutine check_missing_sph_indices(id_check)
!!     &          sph_rtp, sph_rtm, sph_rlm, sph_rj,                    &
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rtm_grid), intent(in) :: sph_rtm
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!
      module cmp_trans_sph_indices
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), allocatable :: idx_rtp_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rtm_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rlm_recieve(:,:)
      integer(kind = kint), allocatable :: idx_rj_recieve(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_sph_recieve
!
      use m_spheric_parameter
!
      allocate( idx_rtp_recieve(nnod_rtp,3) )
      allocate( idx_rtm_recieve(nnod_rtm,3) )
      allocate( idx_rlm_recieve(nnod_rlm,2) )
      allocate( idx_rj_recieve(nnod_rj,2) )
!
      idx_rtp_recieve = 0
      idx_rtm_recieve = 0
      idx_rlm_recieve = 0
      idx_rj_recieve =  0
!
      end subroutine allocate_idx_sph_recieve
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_sph_recieve
!
      deallocate(idx_rtp_recieve)
      deallocate(idx_rtm_recieve)
      deallocate(idx_rlm_recieve)
      deallocate(idx_rj_recieve )
!
      end subroutine deallocate_idx_sph_recieve
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_indices_transfer                                   &
     &          (itype, nnod_rtp, nnod_rtm, nnod_rlm, nnod_rj,          &
     &           idx_global_rtp, idx_global_rtm,                        &
     &           idx_global_rlm, idx_global_rj)
!
      use calypso_mpi
      use spherical_SRs_N
      use m_sph_trans_comm_table
      use m_sel_spherical_SRs
!
      integer(kind = kint), intent(in) :: itype
      integer(kind = kint), intent(in) :: nnod_rtp, nnod_rtm
      integer(kind = kint), intent(in) :: nnod_rlm, nnod_rj
      integer(kind = kint), intent(in) :: idx_global_rtp(nnod_rtp,3)
      integer(kind = kint), intent(in) :: idx_global_rtm(nnod_rtm,3)
      integer(kind = kint), intent(in) :: idx_global_rlm(nnod_rlm,2)
      integer(kind = kint), intent(in) :: idx_global_rj(nnod_rj,2)
!
!
      iflag_sph_SR_int = itype
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtp_2_rtm_int'
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtp, nnod_rtm, comm_rtp1, comm_rtm1,                     &
     &    idx_global_rtp(1,1), idx_rtm_recieve(1,1) )
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtp, nnod_rtm, comm_rtp1, comm_rtm1,                     &
     &    idx_global_rtp(1,2), idx_rtm_recieve(1,2) )
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtp, nnod_rtm, comm_rtp1, comm_rtm1,                     &
     &    idx_global_rtp(1,3), idx_rtm_recieve(1,3) )
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rtm_2_rtp_int'
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtm, nnod_rtp, comm_rtm1, comm_rtp1,                     &
     &    idx_global_rtm(1,1), idx_rtp_recieve(1,1) )
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtm, nnod_rtp, comm_rtm1, comm_rtp1,                     &
     &    idx_global_rtm(1,2), idx_rtp_recieve(1,2) )
      call send_recv_sph_trans_int                                      &
     &   (nnod_rtm, nnod_rtp, comm_rtm1, comm_rtp1,                     &
     &    idx_global_rtm(1,3), idx_rtp_recieve(1,3) )
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rj_2_rlm_int'
      idx_rlm_recieve = -1
      call send_recv_sph_trans_int(nnod_rj, nnod_rlm,                   &
     &   comm_rj1, comm_rlm1, idx_global_rj(1,1), idx_rlm_recieve(1,1))
      call send_recv_sph_trans_int(nnod_rj, nnod_rlm,                   &
     &   comm_rj1, comm_rlm1, idx_global_rj(1,2), idx_rlm_recieve(1,2))
!
      if (my_rank .eq. 0) write(*,*) 'send_recv_rlm_2_rj_int'
      idx_rj_recieve = -1
      call send_recv_sph_trans_int(nnod_rlm, nnod_rj,                   &
     &   comm_rlm1, comm_rj1, idx_global_rlm(1,1), idx_rj_recieve(1,1))
      call send_recv_sph_trans_int(nnod_rlm, nnod_rj,                   &
     &   comm_rlm1, comm_rj1, idx_global_rlm(1,2), idx_rj_recieve(1,2))
!
      end subroutine sph_indices_transfer
!
! -----------------------------------------------------------------------
!
      subroutine compare_transfer_sph_indices(id_check,                 &
     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      integer(kind = kint) :: inod
!
!
      write(id_check,*) 'Wrong commnication in rtm => rtp'
      do inod = 1, sph_rtp%nnod_rtp
        if(idx_rtp_recieve(inod,1) .ne. sph_rtp%idx_global_rtp(inod,1)  &
     &        .and. idx_rtp_recieve(inod,1) .ne. 0) then
          if (   idx_rtp_recieve(inod,2)                                &
     &           .ne. sph_rtp%idx_global_rtp(inod,2)                    &
     &        .and. idx_rtp_recieve(inod,2) .ne. 0) then
            if ( idx_rtp_recieve(inod,3)                                &
     &           .ne. sph_rtp%idx_global_rtp(inod,3)                    &
     &        .and. idx_rtp_recieve(inod,3) .ne. 0) then
              write(id_check,'(i16,6i5)') inod,                         &
     &          sph_rtp%idx_global_rtp(inod,1:3),                       &
     &          idx_rtp_recieve(inod,1:3)
            end if
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rtp => rtm'
      do inod = 1, sph_rtm%nnod_rtm
        if(idx_rtm_recieve(inod,1) .ne. sph_rtm%idx_global_rtm(inod,1)  &
     &        .and. idx_rtm_recieve(inod,1) .ne. 0) then
          if(idx_rtm_recieve(inod,2)                                    &
     &         .ne. sph_rtm%idx_global_rtm(inod,2)    &
     &        .and. idx_rtm_recieve(inod,2) .ne. 0) then
            if ( idx_rtm_recieve(inod,3)                                &
     &         .ne. sph_rtm%idx_global_rtm(inod,3)    &
     &        .and. idx_rtm_recieve(inod,3) .ne. 0) then
              write(id_check,'(i16,6i5)') inod,                         &
     &          sph_rtm%idx_global_rtm(inod,1:3),                       &
     &          idx_rtm_recieve(inod,1:3)
            end if
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rj => rlm'
      do inod = 1, sph_rlm%nnod_rlm
        if(idx_rlm_recieve(inod,1) .ne. sph_rlm%idx_global_rlm(inod,1)  &
     &      .and. idx_rlm_recieve(inod,1) .ge. 0) then
          if(idx_rlm_recieve(inod,2)                                    &
     &       .ne. sph_rlm%idx_global_rlm(inod,2)                        &
     &      .and. idx_rlm_recieve(inod,2) .ge. 0) then
            write(id_check,'(i16,6i5)') inod,                           &
     &        sph_rlm%idx_global_rlm(inod,1:2),                         &
     &        idx_rlm_recieve(inod,1:2)
          end if
        end if
      end do
!
      write(id_check,*) 'Wrong commnication in rlm => rj'
      do inod = 1, sph_rj%nnod_rj
        if(idx_rj_recieve(inod,1) .ne. sph_rj%idx_global_rj(inod,1)     &
     &      .and. idx_rj_recieve(inod,1) .ge. 0) then
          if(idx_rj_recieve(inod,2) .ne. sph_rj%idx_global_rj(inod,2)   &
     &      .and. idx_rj_recieve(inod,2) .ge. 0) then
            write(id_check,'(i16,6i5)') inod,                           &
     &        sph_rj%idx_global_rj(inod,1:2), idx_rj_recieve(inod,1:2)
          end if
        end if
      end do
!
      end subroutine compare_transfer_sph_indices
!
! -----------------------------------------------------------------------
!
      subroutine check_missing_sph_indices(id_check,                    &
     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      integer(kind = kint) :: inod, kr, lth, mphi, j
!
!
      write(id_check,*) 'No commnication data in rtm => rtp'
      write(id_check,*)                                                 &
     &   'local_id, global_id, global_r, global_t, global_p, global_m'
      do mphi = 1, sph_rtp%nidx_rtp(3)
        do lth = 1, sph_rtp%nidx_rtp(2)
          do kr = 1, sph_rtp%nidx_rtp(1)
            inod = kr + sph_rtp%nidx_rtp(1) * (lth - 1)                 &
     &                + sph_rtp%nidx_rtp(1) * sph_rtp%nidx_rtp(2)       &
     &                 * (mphi - 1)
            if(    idx_rtp_recieve(inod,1) .eq. 0                       &
     &       .or. idx_rtp_recieve(inod,2) .eq. 0                        &
     &       .or. idx_rtp_recieve(inod,3) .eq. 0) then
              write(id_check,'(5i16,6i5)') inod,                        &
     &           sph_rtp%idx_global_rtp(inod,1:3),                      &
     &           sph_rtp%idx_gl_1d_rtp_r(kr),                           &
     &           sph_rtp%idx_gl_1d_rtp_t(lth),                          &
     &           sph_rtp%idx_gl_1d_rtp_p(mphi,1:2)
            end if
          end do
        end do
      end do
!
      write(id_check,*) 'No commnication in rtp => rtm'
      write(id_check,*)                                                 &
     &         'local_id, global_id, global_r, global_t, global_m'
      do mphi = 1, sph_rtm%nidx_rtm(3)
        do kr = 1, sph_rtm%nidx_rtm(1)
          do lth = 1, sph_rtm%nidx_rtm(2)
            inod =  lth + (kr-1) * sph_rtm%nidx_rtm(2)                  &
     &                  + (mphi-1) * sph_rtm%nidx_rtm(1)                &
     &                             * sph_rtm%nidx_rtm(2)
            if(   idx_rtm_recieve(inod,1) .eq. 0                        &
     &       .or. idx_rtm_recieve(inod,2) .eq. 0                        &
     &       .or. idx_rtm_recieve(inod,3) .eq. 0) then
              write(id_check,'(5i16,6i5)') inod,                        &
     &             sph_rtm%idx_global_rtm(inod,1:3),                    &
     &             sph_rtm%idx_gl_1d_rtm_r(kr),                         &
     &             sph_rtm%idx_gl_1d_rtm_t(lth),                        &
     &             sph_rtm%idx_gl_1d_rtm_m(mphi,1:2)
            end if
          end do
        end do
      end do
!
      write(id_check,*) 'No commnication in rj => rlm'
      write(id_check,*)                                                 &
     &     'local_id, global_r, global_j, global_r, global_l, global_m'
      do kr = 1, sph_rlm%nidx_rlm(1)
        do j = 1, sph_rlm%nidx_rlm(2)
          inod = j + (kr-1) * sph_rlm%nidx_rlm(2)
          if(      idx_rlm_recieve(inod,1) .lt. 0                       &
     &        .or. idx_rlm_recieve(inod,2) .lt. 0) then
              write(id_check,'(4i16,6i5)') inod,                        &
     &        sph_rlm%idx_global_rlm(inod,1:2),                         &
     &        sph_rlm%idx_gl_1d_rlm_r(kr),                              &
     &        sph_rlm%idx_gl_1d_rlm_j(j,2:3)
          end if
        end do
      end do
!
      write(id_check,*) 'No commnication in rlm => rj'
      write(id_check,*)                                                 &
     &     'local_id, global_r, global_j, global_r, global_l, global_m'
      do kr = 1, sph_rj%nidx_rj(1)
        do j = 1, sph_rj%nidx_rj(2)
          inod = j + (kr-1) * sph_rj%nidx_rj(2)
          if(      idx_rj_recieve(inod,1) .lt. 0                        &
     &        .or. idx_rj_recieve(inod,2) .lt. 0) then
              write(id_check,'(4i16,6i5)') inod,                        &
     &          sph_rj%idx_global_rj(inod,1:2),                         &
     &          sph_rj%idx_gl_1d_rj_r(kr),                              &
     &          sph_rj%idx_gl_1d_rj_j(j,2:3)
          end if
        end do
      end do
!
      end subroutine check_missing_sph_indices
!
! -----------------------------------------------------------------------
!
      end module cmp_trans_sph_indices
