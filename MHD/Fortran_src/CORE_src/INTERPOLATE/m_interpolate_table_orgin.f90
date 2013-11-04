!>@file   m_interpolate_table_orgin.f90
!!@brief  module m_interpolate_table_orgin
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in  Aug., 2006
!
!> @brief Interpolation table for source grid
!!@verbatim
!!      subroutine allocate_itp_num_org(np_smp, num_dest_pe)
!!      subroutine allocate_itp_table_org
!!
!!      subroutine deallocate_itp_num_org
!!      subroutine deallocate_itp_table_org
!!@endverbatim
!
      module m_interpolate_table_orgin
!
      use m_precision
!
      implicit none
!
!
!>   number of subdomain to send interpolated data
      integer(kind = kint) :: num_dest_domain
!>   flag if target nodes have same prosess
      integer(kind = kint) :: iflag_self_itp_send
!>   subdomain rank to send interpolated data
      integer(kind = kint), allocatable :: id_dest_domain(:)
!>   end address to send interpolated data
      integer(kind = kint), allocatable :: istack_nod_tbl_org(:)
!>   end address for interplation modes
      integer(kind = kint), allocatable :: istack_itp_type_org(:)
!
!>   total number of node to interpolate in original subdomain
      integer(kind = kint) :: ntot_table_org
!>   export table for interpolation
      integer(kind = kint), allocatable :: inod_itp_send(:)
!>   global node ID for target domain
      integer(kind = kint), allocatable :: inod_gl_dest_4_org(:)
!>   local element ID to make interpolation
      integer(kind = kint), allocatable :: iele_org_4_org(:)
!>   interpolation type ID
      integer(kind = kint), allocatable :: itype_inter_org(:)
!>   Coordinate of target node in element coordinate
      real(kind = kreal), allocatable :: coef_inter_org(:,:)
!
!>   end address of table to interpolation at original elements
      integer(kind = kint), allocatable                                 &
     &            :: istack_table_type_org_smp(:)
!>   maximum number of interpolation at original elements
      integer(kind = kint) :: imax_table_type_org_smp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_num_org(np_smp, num_dest_pe)
!
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: num_dest_pe
!
      allocate( id_dest_domain(num_dest_pe) )
      allocate( istack_nod_tbl_org(0:num_dest_pe) )
      allocate( istack_itp_type_org(0:4) )
!
      allocate(istack_table_type_org_smp(0:4*np_smp))
!
      if(num_dest_pe .gt. 0) id_dest_domain = 0
      istack_nod_tbl_org =  0
      istack_itp_type_org = 0
      istack_table_type_org_smp = 0
!
      end subroutine allocate_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine allocate_itp_table_org
!
      allocate( inod_itp_send(ntot_table_org) )
      allocate( inod_gl_dest_4_org(ntot_table_org) )
      allocate( iele_org_4_org(ntot_table_org) )
      allocate( itype_inter_org(ntot_table_org) )
      allocate( coef_inter_org(ntot_table_org,3) )
!
      if(ntot_table_org .gt. 0) then
        inod_itp_send =      0
        inod_gl_dest_4_org = 0
        iele_org_4_org =     0
        itype_inter_org =   -1
        coef_inter_org = 0.0d0
      end if
!
      end subroutine allocate_itp_table_org
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_num_org
!
      deallocate( id_dest_domain )
      deallocate( istack_nod_tbl_org )
      deallocate(istack_table_type_org_smp)
!
      end subroutine deallocate_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_itp_table_org
!
      deallocate( inod_itp_send )
      deallocate( inod_gl_dest_4_org )
      deallocate( iele_org_4_org )
      deallocate( itype_inter_org )
      deallocate( coef_inter_org )
!
      end subroutine deallocate_itp_table_org
!
!-----------------------------------------------------------------------
!
      end module m_interpolate_table_orgin
