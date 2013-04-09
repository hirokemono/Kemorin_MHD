!>@file   m_idx_4_sph_comm_test.f90
!!@brief  module m_idx_4_sph_comm_test
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2013 
!
!>@brief  global addresses for spherical transform
!!        communication test
!!
!!@verbatim
!!      subroutine allocate_idx_gl_rtp_compare
!!      subroutine allocate_idx_gl_rtm_out
!!      subroutine allocate_idx_gl_rlm_out
!!      subroutine allocate_idx_gl_rj_compare
!!
!!      subroutine deallocate_idx_gl_rtp_compare
!!      subroutine deallocate_idx_gl_rtm_out
!!      subroutine deallocate_idx_gl_rlm_out
!!      subroutine deallocate_idx_gl_rj_compare
!!
!!      subroutine set_comm_rtm_to_work_IO
!!      subroutine set_comm_rlm_to_work_IO
!!      subroutine set_comm_rtp_from_work_IO
!!      subroutine set_comm_rj_from_work_IO
!!
!!      subroutine set_global_id_4_comm_rtm
!!      subroutine set_global_id_4_comm_rlm
!!@endverbatim
!
      module m_idx_4_sph_comm_test
!
      use m_precision
      use m_constants
      use m_sph_trans_comm_table
!
      implicit none
!
!>      global point ID for  @f$ f(r,\theta,\phi) @f$
      integer(kind = kint), allocatable :: idx_gl_rtp_compare(:,:)
!>      global point ID for  @f$ f(r,\theta,m) @f$
      integer(kind = kint), allocatable :: idx_gl_rtm_out(:,:)
!>      global point ID for  @f$ f(r,l,m) @f$
      integer(kind = kint), allocatable :: idx_gl_rlm_out(:,:)
!>      global point ID for  @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: idx_gl_rj_compare(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rtp_compare
!
!
      allocate( idx_gl_rtp_compare(ntot_item_sr_rtp,3) )
      idx_gl_rtp_compare = 0
!
      end subroutine allocate_idx_gl_rtp_compare
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rtm_out
!
!
      allocate( idx_gl_rtm_out(ntot_item_sr_rtm,3) )
      idx_gl_rtm_out = 0
!
      end subroutine allocate_idx_gl_rtm_out
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rlm_out
!
!
      allocate( idx_gl_rlm_out(ntot_item_sr_rlm,2) )
      idx_gl_rlm_out = 0
!
      end subroutine allocate_idx_gl_rlm_out
!
! -----------------------------------------------------------------------
!
      subroutine allocate_idx_gl_rj_compare
!
!
      allocate( idx_gl_rj_compare(nneib_domain_rj,2) )
      idx_gl_rj_compare = 0
!
      end subroutine allocate_idx_gl_rj_compare
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rtp_compare
!
      deallocate( idx_gl_rtp_compare )
!
      end subroutine deallocate_idx_gl_rtp_compare
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rtm_out
!
      deallocate( idx_gl_rtm_out )
!
      end subroutine deallocate_idx_gl_rtm_out
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rlm_out
!
      deallocate( idx_gl_rlm_out )
!
      end subroutine deallocate_idx_gl_rlm_out
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_idx_gl_rj_compare
!
      deallocate( idx_gl_rj_compare )
!
      end subroutine deallocate_idx_gl_rj_compare
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_comm_rtm_to_work_IO
!
      use m_comm_data_IO
!
      integer(kind = kint) :: i
!
!
      nwork_import_IO =    ithree
      call allocate_import_work_IO
!
      do i = 1, ntot_item_sr_rtm
        iwork_import_IO(i,1) = idx_gl_rtm_out(i,1)
        iwork_import_IO(i,2) = idx_gl_rtm_out(i,2)
        iwork_import_IO(i,3) = idx_gl_rtm_out(i,3)
      end do
!
      call deallocate_idx_gl_rtm_out
!
      end subroutine set_comm_rtm_to_work_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_rlm_to_work_IO
!
      use m_comm_data_IO
!
      integer(kind = kint) :: i
!
!
      nwork_import_IO =    itwo
      call allocate_import_work_IO
!
      do i = 1, ntot_item_sr_rlm
        iwork_import_IO(i,1) = idx_gl_rlm_out(i,1)
        iwork_import_IO(i,2) = idx_gl_rlm_out(i,2)
      end do
!
      call deallocate_idx_gl_rlm_out
!
      end subroutine set_comm_rlm_to_work_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_rtp_from_work_IO
!
      use m_comm_data_IO
!
      integer(kind = kint) :: i
!
!
      call allocate_idx_gl_rtp_compare
!
      do i = 1, ntot_item_sr_rtp
        idx_gl_rtp_compare(i,1) = iwork_import_IO(i,1)
        idx_gl_rtp_compare(i,2) = iwork_import_IO(i,2)
        idx_gl_rtp_compare(i,3) = iwork_import_IO(i,3)
      end do
!
      call deallocate_import_work_IO
!
      end subroutine set_comm_rtp_from_work_IO
!
! -----------------------------------------------------------------------
!
      subroutine set_comm_rj_from_work_IO
!
      use m_comm_data_IO
!
      integer(kind = kint) :: i
!
!
      call allocate_idx_gl_rj_compare
!
      do i = 1, ntot_item_sr_rj
        idx_gl_rj_compare(i,1) = iwork_import_IO(i,1)
        idx_gl_rj_compare(i,2) = iwork_import_IO(i,2)
      end do
!
      call deallocate_import_work_IO
!
      end subroutine set_comm_rj_from_work_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_global_id_4_comm_rtm
!
      use m_spheric_parameter
!
      integer(kind = kint) :: i, inod
!
      do i = 1, ntot_item_sr_rtm
        inod = item_sr_rtm(i)
        idx_gl_rtm_out(i,1) = idx_global_rtm(inod,1)
        idx_gl_rtm_out(i,2) = idx_global_rtm(inod,2)
        idx_gl_rtm_out(i,3) = idx_global_rtm(inod,3)
      end do
!
      end subroutine set_global_id_4_comm_rtm
!
! -----------------------------------------------------------------------
!
      subroutine set_global_id_4_comm_rlm
!
      use m_spheric_parameter
!
      integer(kind = kint) :: i, inod
!
      do i = 1, ntot_item_sr_rlm
        inod = item_sr_rlm(i)
        idx_gl_rlm_out(i,1) = idx_global_rlm(inod,1)
        idx_gl_rlm_out(i,2) = idx_global_rlm(inod,2)
      end do
!
      end subroutine set_global_id_4_comm_rlm
!
! -----------------------------------------------------------------------
!
      end module m_idx_4_sph_comm_test
