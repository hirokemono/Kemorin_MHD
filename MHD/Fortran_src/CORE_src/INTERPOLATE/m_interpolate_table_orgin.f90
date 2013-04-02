!
!      module m_interpolate_table_orgin
!
!> @brief Interpolation table for source grid
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine allocate_itp_num_org(num_dest_pe)
!      subroutine allocate_itp_table_org
!      subroutine allocate_istack_tbl_wtype_smp(np_smp)
!
!      subroutine deallocate_itp_num_org
!      subroutine deallocate_itp_table_org
!      subroutine deallocate_istack_tbl_wtype_smp
!
      module m_interpolate_table_orgin
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint) :: num_dest_domain
!<   number of subdomain to send interpolated data
      integer(kind = kint) :: iflag_self_itp_send
!<   flag if target nodes have same prosess
      integer(kind = kint), allocatable :: id_dest_domain(:)
!<   subdomain rank to send interpolated data
      integer(kind = kint), allocatable :: istack_nod_table_org(:)
!<   end address to send interpolated data
!
      integer(kind = kint), allocatable                                 &
     &            :: istack_nod_table_wtype_org(:)
!<   end address to send interpolated data including interpolate type
!
      integer(kind = kint) :: ntot_table_org
!<   total number of node to interpolate in original subdomain
      integer(kind = kint), allocatable :: inod_gl_dest_4_org(:)
!<   global node ID for target domain
      integer(kind = kint), allocatable :: iele_org_4_org(:)
!<   local element ID to make interpolation
      integer(kind = kint), allocatable :: itype_inter_org(:)
!<   interpolation type ID
      real(kind = kreal), allocatable :: coef_inter_org(:,:)
!<   Coordinate of target node in element coordinate
!
      integer(kind = kint), allocatable                                 &
     &            :: istack_table_wtype_org_smp(:)
!<   end address of table to interpolation at original elements
      integer(kind = kint) :: imax_table_wtype_org_smp
!<   maximum number of interpolation at original elements
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_num_org(num_dest_pe)
!
      integer(kind = kint), intent(in) :: num_dest_pe
!
      allocate( id_dest_domain(num_dest_pe) )
      allocate( istack_nod_table_org(0:num_dest_pe) )
      allocate( istack_nod_table_wtype_org(0:4*num_dest_pe) )
      id_dest_domain = 0
      istack_nod_table_org = -1
      istack_nod_table_wtype_org = -1
!
      end subroutine allocate_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_org
!
      allocate( inod_gl_dest_4_org(ntot_table_org) )
      allocate( iele_org_4_org(ntot_table_org) )
      allocate( itype_inter_org(ntot_table_org) )
      allocate( coef_inter_org(ntot_table_org,3) )
!
      inod_gl_dest_4_org = 0
      iele_org_4_org = 0
      itype_inter_org = -1
      coef_inter_org = 0.0d0
!
      end subroutine allocate_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine allocate_istack_tbl_wtype_smp(np_smp)
!
      integer(kind = kint), intent(in) :: np_smp
!
      allocate(istack_table_wtype_org_smp(0:4*np_smp*num_dest_domain))
      istack_table_wtype_org_smp = 0
!
      end subroutine allocate_istack_tbl_wtype_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_org
!
      deallocate( id_dest_domain )
      deallocate( istack_nod_table_org )
      deallocate( istack_nod_table_wtype_org )
!
      end subroutine deallocate_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_table_org
!
      deallocate( inod_gl_dest_4_org )
      deallocate( iele_org_4_org )
      deallocate( itype_inter_org )
      deallocate( coef_inter_org )
!
      end subroutine deallocate_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_istack_tbl_wtype_smp
!
      deallocate(istack_table_wtype_org_smp)
!
      end subroutine deallocate_istack_tbl_wtype_smp
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_orgin
