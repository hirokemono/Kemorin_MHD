!>@file   set_global_spherical_param.f90
!!@brief  module set_global_spherical_param
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Set global resolution of spherical hermonics
!!
!!@verbatim
!!      subroutine set_global_sph_resolution(l_truncation, m_folding,   &
!!     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!!
!!      subroutine set_global_rtm_resolution(l_truncation, m_folding,   &
!!     &                                     sph_rtp, sph_rtm)
!!      subroutine set_global_rlm_resolution(l_truncation, m_folding,   &
!!     &                                     nidx_global_rtp_r, sph_rlm)
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!        required resoplution:  nidx_global_rtp(1)
!!                               nidx_global_rtp(2)
!!        required resolution:  l_truncation
!!
!!      subroutine set_gl_nnod_spherical(nproc,                         &
!!     &          ndomain_1, ndomain_2, ndomain_3, id_gl_rank,          &
!!     &          nidx_local_1, nidx_local_2, nidx_local_3,             &
!!     &          nidx_local, nnod_local)
!!      subroutine set_gl_nnod_spheric_rj(nproc, ndomain_1, ndomain_2,  &
!!     &          id_gl_rank, nidx_local_1, nidx_local_2,               &
!!     &          nidx_local, nnod_local)
!!
!!      subroutine set_gl_rank_3d                                       &
!!     &         (inner_r_flag, nproc, ndomain_3d, id_gl_rank)
!!      subroutine set_gl_rank_2d                                       &
!!     &         (inner_r_flag, nproc, ndomain_2d, id_gl_rank)
!!
!!      integer(kind = kint) function set_rank_by_1b_sph_rank            &
!!     &                  (inner_r_flag, ndomain_3d, ip_r, ip_t, ip_p)
!!      integer(kind = kint) function set_rank_by_1b_rj_rank             &
!!     &                  (inner_r_flag, ndomain_2d, ip_r, ip_j)
!!
!!      subroutine check_spheric_global_numnod(id_rank)
!!@endverbatim
!
      module set_global_spherical_param
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_global_sph_resolution(l_truncation, m_folding,     &
     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: l_truncation, m_folding
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      sph_rtp%nidx_global_rtp(3) = 2 * sph_rtp%nidx_global_rtp(2)       &
     &                            / m_folding
!
      sph_rj%nidx_global_rj(1) = sph_rtp%nidx_global_rtp(1)
      sph_rj%nidx_global_rj(2)                                          &
     &                   = (l_truncation+2)*l_truncation / m_folding
!
      call set_global_rtm_resolution                                    &
     &   (l_truncation, m_folding, sph_rtp, sph_rtm)
      call set_global_rlm_resolution                                    &
     &   (l_truncation, m_folding, sph_rtp%nidx_global_rtp(1), sph_rlm)
!
!      call check_spheric_global_numnod                                 &
!     &   (my_rank+50, sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      end subroutine set_global_sph_resolution
!
! -----------------------------------------------------------------------
!
      subroutine set_global_rtm_resolution(l_truncation, m_folding,     &
     &                                     sph_rtp, sph_rtm)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: l_truncation, m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
!
!
      sph_rtm%nidx_global_rtm(1:2) = sph_rtp%nidx_global_rtp(1:2)
      sph_rtm%nidx_global_rtm(3) =   2*(l_truncation/m_folding + 1) 
!
      end subroutine set_global_rtm_resolution
!
! -----------------------------------------------------------------------
!
      subroutine set_global_rlm_resolution(l_truncation, m_folding,     &
     &                                     nidx_global_rtp_r, sph_rlm)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: l_truncation, m_folding
      integer(kind = kint), intent(in) :: nidx_global_rtp_r
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      sph_rlm%nidx_global_rlm(1) = nidx_global_rtp_r
      sph_rlm%nidx_global_rlm(2)                                        &
     &                   = (l_truncation+2)*l_truncation / m_folding
!
      end subroutine set_global_rlm_resolution
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gl_nnod_spherical(nproc,                           &
     &          ndomain_1, ndomain_2, ndomain_3, id_gl_rank,            &
     &          nidx_local_1, nidx_local_2, nidx_local_3,               &
     &          nidx_local, nnod_local)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_1, ndomain_2
      integer(kind = kint), intent(in) :: ndomain_3
      integer(kind = kint), intent(in) :: id_gl_rank(3,0:(nproc-1))
      integer(kind = kint), intent(in) :: nidx_local_1(ndomain_1)
      integer(kind = kint), intent(in) :: nidx_local_2(ndomain_2)
      integer(kind = kint), intent(in) :: nidx_local_3(ndomain_3)
!
      integer(kind = kint), intent(inout) :: nidx_local(nproc,3)
      integer(kind = kint), intent(inout) :: nnod_local(nproc)
!
      integer(kind = kint) :: i1, i2, i3, i, ip_rank
!
!
      do i = 1, nproc
        ip_rank = i - 1
        i1 = id_gl_rank(1,ip_rank) + 1
        i2 = id_gl_rank(2,ip_rank) + 1
        i3 = id_gl_rank(3,ip_rank) + 1
        nidx_local(i,1) = nidx_local_1(i1)
        nidx_local(i,2) = nidx_local_2(i2)
        nidx_local(i,3) = nidx_local_3(i3)
        nnod_local(i) =  nidx_local(i,1)                                &
     &                 * nidx_local(i,2)                                &
     &                 * nidx_local(i,3)
      end do
!
      end subroutine set_gl_nnod_spherical
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_nnod_spheric_rj(nproc, ndomain_1, ndomain_2,    &
     &          id_gl_rank, nidx_local_1, nidx_local_2,                 &
     &          nidx_local, nnod_local)
!
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_1, ndomain_2
      integer(kind = kint), intent(in) :: id_gl_rank(2,0:(nproc-1))
      integer(kind = kint), intent(in) :: nidx_local_1(ndomain_1)
      integer(kind = kint), intent(in) :: nidx_local_2(ndomain_2)
!
      integer(kind = kint), intent(inout) :: nidx_local(nproc,2)
      integer(kind = kint), intent(inout) :: nnod_local(nproc)
!
      integer(kind = kint) :: i1, i2, i, ip_rank
!
!
      do i = 1, nproc
        ip_rank = i - 1
        i1 = id_gl_rank(1,ip_rank) + 1
        i2 = id_gl_rank(2,ip_rank) + 1
        nidx_local(i,1) = nidx_local_1(i1)
        nidx_local(i,2) = nidx_local_2(i2)
        nnod_local(i) =  nidx_local(i,1)                                &
     &                 * nidx_local(i,2)
      end do
!
      end subroutine set_gl_nnod_spheric_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_gl_rank_3d                                         &
     &         (inner_r_flag, nproc, ndomain_3d, id_gl_rank)
!
      logical, intent(in) :: inner_r_flag
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_3d(3)
      integer(kind = kint), intent(inout) :: id_gl_rank(3,0:(nproc-1))
      integer(kind = kint) :: ip_rank
      integer(kind = kint) :: itmp
!
!
      if(inner_r_flag) then
        do ip_rank = 0, nproc-1
          id_gl_rank(1,ip_rank) = mod(ip_rank,ndomain_3d(1))
          itmp = (ip_rank-id_gl_rank(1,ip_rank)) / ndomain_3d(1)
          id_gl_rank(3,ip_rank) = mod(itmp,ndomain_3d(3))
          id_gl_rank(2,ip_rank) = (itmp-id_gl_rank(3,ip_rank))          &
     &                           / ndomain_3d(3)
        end do
      else
        do ip_rank = 0, nproc-1
          id_gl_rank(3,ip_rank) = mod(ip_rank,ndomain_3d(3))
          itmp = (ip_rank-id_gl_rank(3,ip_rank)) / ndomain_3d(3)
          id_gl_rank(2,ip_rank) = mod(itmp,ndomain_3d(2))
          id_gl_rank(1,ip_rank) = (itmp-id_gl_rank(2,ip_rank))          &
     &                           / ndomain_3d(2)
        end do
      end if
!
      end subroutine set_gl_rank_3d
!
! -----------------------------------------------------------------------
!
      subroutine set_gl_rank_2d                                         &
     &         (inner_r_flag, nproc, ndomain_2d, id_gl_rank)
!
      logical, intent(in) :: inner_r_flag
      integer(kind = kint), intent(in) :: nproc
      integer(kind = kint), intent(in) :: ndomain_2d(2)
      integer(kind = kint), intent(inout) :: id_gl_rank(2,0:(nproc-1))
      integer(kind = kint) :: ip_rank
!
!
      if(inner_r_flag) then
        do ip_rank = 0, nproc-1
          id_gl_rank(1,ip_rank) = mod(ip_rank,ndomain_2d(1))
          id_gl_rank(2,ip_rank) = (ip_rank-id_gl_rank(1,ip_rank))       &
     &                           / ndomain_2d(1)
        end do
      else
        do ip_rank = 0, nproc-1
          id_gl_rank(2,ip_rank) = mod(ip_rank,ndomain_2d(2))
          id_gl_rank(1,ip_rank) = (ip_rank-id_gl_rank(2,ip_rank))       &
     &                           / ndomain_2d(2)
        end do
      end if
!
      end subroutine set_gl_rank_2d
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function set_rank_by_1b_sph_rank              &
     &                  (inner_r_flag, ndomain_3d, ip_r, ip_t, ip_p)
!
      logical, intent(in) :: inner_r_flag
      integer(kind = kint), intent(in) :: ip_r, ip_t, ip_p
      integer(kind = kint), intent(in) :: ndomain_3d(3)
!
!
      if(inner_r_flag) then
        set_rank_by_1b_sph_rank =  ip_r                                 &
     &                           + ip_t*ndomain_3d(1)*ndomain_3d(3)     &
     &                           + ip_p*ndomain_3d(1)
      else
        set_rank_by_1b_sph_rank =  ip_r*ndomain_3d(2)*ndomain_3d(3)     &
     &                           + ip_t*ndomain_3d(3)                   &
     &                           + ip_p
      end if
!
      end function set_rank_by_1b_sph_rank
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function set_rank_by_1b_rj_rank               &
     &                  (inner_r_flag, ndomain_2d, ip_r, ip_j)
!
      logical, intent(in) :: inner_r_flag
      integer(kind = kint), intent(in) :: ip_r, ip_j
      integer(kind = kint), intent(in) :: ndomain_2d(2)
!
!
      if(inner_r_flag) then
        set_rank_by_1b_rj_rank =  ip_r + ip_j*ndomain_2d(1)
      else
        set_rank_by_1b_rj_rank =  ip_r*ndomain_2d(2) + ip_j
      end if
!
      end function set_rank_by_1b_rj_rank
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_numnod                            &
     &         (id_rank, sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      use t_spheric_parameter
!
      integer, intent(in) :: id_rank
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rtm_grid), intent(in) :: sph_rtm
      type(sph_rlm_grid), intent(in) :: sph_rlm
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint) :: nnod_global_rtp, nnod_global_rtm
      integer(kind = kint) :: nnod_global_rlm, nnod_global_rj
!
!
      nnod_global_rtp                                                   &
     &  =  sph_rtp%nidx_global_rtp(1) * sph_rtp%nidx_global_rtp(2)      &
     &   * sph_rtp%nidx_global_rtp(3)
      nnod_global_rtm                                                   &
     &  =  sph_rtm%nidx_global_rtm(1) * sph_rtm%nidx_global_rtm(2)      &
     &   * sph_rtm%nidx_global_rtm(3)
      nnod_global_rlm                                                   &
     &  = sph_rlm%nidx_global_rlm(1) * (sph_rlm%nidx_global_rlm(2)+1)
      nnod_global_rj                                                    &
     &  = sph_rj%nidx_global_rj(1) * (sph_rj%nidx_global_rj(2) + 1)
!
      write(id_rank+50,*) 'nnod_global_rtp ', nnod_global_rtp
      write(id_rank+50,*) 'nnod_global_rtm ', nnod_global_rtm
      write(id_rank+50,*) 'nnod_global_rlm ', nnod_global_rlm
      write(id_rank+50,*) 'nnod_global_rj ',  nnod_global_rj
!
      end subroutine check_spheric_global_numnod
!
! -----------------------------------------------------------------------
!
      end module set_global_spherical_param
