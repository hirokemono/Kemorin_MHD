!
!      module m_interpolate_table_dest_IO
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!> @file  m_interpolate_table_dest_IO.f90
!!      module m_interpolate_table_dest_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief IO routines for target data of interpolation table
!!
!!@verbatim
!!      subroutine allocate_itp_num_dst_IO
!!      subroutine allocate_itp_nod_dst_IO
!!      subroutine allocate_itp_coefs_dst_IO
!!
!!      subroutine deallocate_itp_num_dst_IO
!!      subroutine deallocate_itp_nod_dst_IO
!!      subroutine deallocate_itp_coefs_dst_IO
!!@endverbatim
!
      module m_interpolate_table_dest_IO
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_org_domain_IO
      integer(kind = kint), allocatable :: id_org_domain_IO(:)
!
      integer(kind = kint), allocatable :: istack_table_dest_IO(:)
      integer(kind = kint), allocatable                                 &
     &              :: istack_table_wtype_dest_IO(:)
!
      integer(kind = kint) :: ntot_table_dest_IO
      integer(kind = kint), allocatable :: inod_dest_IO(:)
!
      integer(kind = kint), allocatable :: inod_global_dest_IO(:)
      integer(kind = kint), allocatable :: itype_inter_dest_IO(:)
      integer(kind = kint), allocatable :: iele_orgin_IO(:)
      real(kind = kreal), allocatable :: coef_inter_dest_IO(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_num_dst_IO
!
      allocate(id_org_domain_IO(num_org_domain_IO))
      allocate(istack_table_dest_IO(0:num_org_domain_IO))
      allocate(istack_table_wtype_dest_IO(0:4*num_org_domain_IO))
      id_org_domain_IO = 0
      istack_table_dest_IO = 0
      istack_table_wtype_dest_IO = 0
!
      end subroutine allocate_itp_num_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_nod_dst_IO
!
      allocate(inod_dest_IO(ntot_table_dest_IO))
      inod_dest_IO = 0
!
      end subroutine allocate_itp_nod_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_coefs_dst_IO
!
      allocate(inod_global_dest_IO(ntot_table_dest_IO))
      allocate(iele_orgin_IO(ntot_table_dest_IO))
      allocate(itype_inter_dest_IO(ntot_table_dest_IO))
      allocate(coef_inter_dest_IO(ntot_table_dest_IO,3))
!
      inod_global_dest_IO = 0
      iele_orgin_IO = 0
      itype_inter_dest_IO = -1
      coef_inter_dest_IO = 0.0d0
!
      end subroutine allocate_itp_coefs_dst_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_dst_IO
!
      deallocate(id_org_domain_IO)
      deallocate(istack_table_dest_IO)
      deallocate(istack_table_wtype_dest_IO)
!
      end subroutine deallocate_itp_num_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_nod_dst_IO
!
      deallocate(inod_dest_IO)
!
      end subroutine deallocate_itp_nod_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_coefs_dst_IO
!
      deallocate(inod_global_dest_IO)
      deallocate(iele_orgin_IO)
      deallocate(itype_inter_dest_IO)
      deallocate(coef_inter_dest_IO)
!
      end subroutine deallocate_itp_coefs_dst_IO
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_dest_IO
