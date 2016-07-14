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
      use t_interpolate_tbl_dest
!
      implicit none
!
!> Structure of interpolation table for target grid
      type(interpolate_table_dest), save :: IO_itp_dest
!IO_itp_dest%inod_dest_4_dest
!
!      integer(kind = kint) :: num_org_domain_IO
!      integer(kind = kint), allocatable :: id_org_domain_IO(:)
!
!      integer(kind = kint), allocatable :: istack_table_dest_IO(:)
      integer(kind = kint), allocatable                                 &
     &              :: istack_table_wtype_dest_IO(:)
!
!      integer(kind = kint) :: ntot_table_dest_IO
!      integer(kind = kint), allocatable :: inod_dest_IO(:)
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
      call alloc_itp_num_dest(IO_itp_dest)
!
      allocate(istack_table_wtype_dest_IO(0:4*IO_itp_dest%num_org_domain))
      istack_table_wtype_dest_IO = 0
!
      end subroutine allocate_itp_num_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_nod_dst_IO
!
      allocate(IO_itp_dest%inod_dest_4_dest(IO_itp_dest%ntot_table_dest))
      IO_itp_dest%inod_dest_4_dest = 0
!
      end subroutine allocate_itp_nod_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_coefs_dst_IO
!
      allocate(inod_global_dest_IO(IO_itp_dest%ntot_table_dest))
      allocate(iele_orgin_IO(IO_itp_dest%ntot_table_dest))
      allocate(itype_inter_dest_IO(IO_itp_dest%ntot_table_dest))
      allocate(coef_inter_dest_IO(IO_itp_dest%ntot_table_dest,3))
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
      call dealloc_itp_num_dest(IO_itp_dest)
      deallocate(istack_table_wtype_dest_IO)
!
      end subroutine deallocate_itp_num_dst_IO
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_nod_dst_IO
!
      deallocate(IO_itp_dest%inod_dest_4_dest)
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
