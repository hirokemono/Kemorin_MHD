!
!      module m_interpolate_table_org_IO
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_itp_num_org_IO
!      subroutine allocate_itp_table_org_IO
!
!      subroutine deallocate_itp_num_org_IO
!      subroutine deallocate_itp_table_org_IO
!
      module m_interpolate_table_org_IO
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_dest_domain_IO
      integer(kind = kint), allocatable :: id_dest_domain_IO(:)
      integer(kind = kint), allocatable :: istack_nod_table_org_IO(:)
      integer(kind = kint), allocatable                                 &
     &            :: istack_table_wtype_org_IO(:)
!
      integer(kind = kint) :: ntot_table_org_IO
      integer(kind = kint), allocatable :: inod_itp_send_IO(:)
      integer(kind = kint), allocatable :: inod_gl_dest_4_org_IO(:)
      integer(kind = kint), allocatable :: iele_org_4_org_IO(:)
      integer(kind = kint), allocatable :: itype_inter_org_IO(:)
      real(kind = kreal), allocatable :: coef_inter_org_IO(:,:)
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
      allocate( istack_table_wtype_org_IO(0:4*num_dest_domain_IO) )
      id_dest_domain_IO = 0
      istack_nod_table_org_IO = -1
      istack_table_wtype_org_IO = -1
!
      end subroutine allocate_itp_num_org_IO
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_org_IO
!
      allocate( inod_itp_send_IO(ntot_table_org_IO) )
      allocate( inod_gl_dest_4_org_IO(ntot_table_org_IO) )
      allocate( iele_org_4_org_IO(ntot_table_org_IO) )
      allocate( itype_inter_org_IO(ntot_table_org_IO) )
      allocate( coef_inter_org_IO(ntot_table_org_IO,3) )
!
      inod_itp_send_IO = 0
      inod_gl_dest_4_org_IO = 0
      iele_org_4_org_IO = 0
      itype_inter_org_IO = -1
      coef_inter_org_IO = 0.0d0
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
      deallocate( istack_table_wtype_org_IO )
!
      end subroutine deallocate_itp_num_org_IO
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_table_org_IO
!
      deallocate(inod_itp_send_IO)
      deallocate(inod_gl_dest_4_org_IO)
      deallocate(iele_org_4_org_IO)
      deallocate(itype_inter_org_IO)
      deallocate(coef_inter_org_IO)
!
      end subroutine deallocate_itp_table_org_IO
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_org_IO
