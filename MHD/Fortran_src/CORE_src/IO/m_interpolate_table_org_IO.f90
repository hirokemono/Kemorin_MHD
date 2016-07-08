!> @file  m_interpolate_table_org_IO.f90
!!      module m_interpolate_table_org_IO
!!
!! @author  H. Matsui
!! @date Written in Aug., 2006
!
!> @brief IO routines for origin data of interpolation table
!!
!!@verbatim
!!      subroutine allocate_itp_num_org_IO
!!      subroutine allocate_itp_table_org_IO
!!
!!      subroutine deallocate_itp_num_org_IO
!!      subroutine deallocate_itp_table_org_IO
!!@endverbatim
!
      module m_interpolate_table_org_IO
!
      use m_precision
      use t_interpolate_tbl_org
!
      implicit none
!
!> Structure of interpolation table for source grid
      type(interpolate_table_org), save :: IO_itp_org
!IO_itp_org%ntot_table_org
!
      integer(kind = kint) :: num_dest_domain_IO
      integer(kind = kint), allocatable :: id_dest_domain_IO(:)
      integer(kind = kint), allocatable :: istack_nod_table_org_IO(:)
      integer(kind = kint), allocatable :: istack_itp_type_org_IO(:)
!
!      integer(kind = kint) :: ntot_table_org_IO
!      integer(kind = kint), allocatable :: inod_itp_send_IO(:)
!      integer(kind = kint), allocatable :: inod_gl_dest_4_org_IO(:)
!      integer(kind = kint), allocatable :: iele_org_4_org_IO(:)
!      integer(kind = kint), allocatable :: itype_inter_org_IO(:)
!      real(kind = kreal), allocatable :: coef_inter_org_IO(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_num_org_IO
!
      allocate( id_dest_domain_IO(num_dest_domain_IO) )
      allocate( istack_nod_table_org_IO(0:num_dest_domain_IO) )
      allocate( istack_itp_type_org_IO(0:4) )
      id_dest_domain_IO = 0
      istack_nod_table_org_IO =   0
      istack_itp_type_org_IO =    0
!
      end subroutine allocate_itp_num_org_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_org_IO
!
      call alloc_itp_table_org(IO_itp_org)
!
      end subroutine allocate_itp_table_org_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_org_IO
!
      deallocate( id_dest_domain_IO )
      deallocate( istack_nod_table_org_IO )
      deallocate( istack_itp_type_org_IO )
!
      end subroutine deallocate_itp_num_org_IO
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_org_IO
