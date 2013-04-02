!t_interpolate_table_orgin.f90
!      module t_interpolate_table_orgin
!
!> @brief Structure of interpolation table for source mesh
!
!      Written by H.Matsui on Nov., 2008
!
!      subroutine alloc_type_itp_num_org(tbl_org)
!      subroutine alloc_type_itp_table_org(tbl_org)
!      subroutine alloc_type_istack_tbl_wtp_smp(np_smp, tbl_org)
!
!      subroutine dealloc_type_itp_num_org(tbl_org)
!      subroutine dealloc_type_itp_table_org(tbl_org)
!      subroutine dealloc_type_istack_tbl_wtp_smp(tbl_org)
!
      module t_interpolate_table_orgin
!
      use m_precision
!
      implicit none
!
!
!> Structure of interpolation table for source grid
      type interpolate_table_org
!
        integer(kind = kint) :: num_dest_domain
!<   number of subdomain to send interpolated data
        integer(kind = kint) :: iflag_self_itp_send
!<   flag if target nodes have same prosess
        integer(kind = kint), pointer :: id_dest_domain(:)
!<   subdomain rank to send interpolated data
        integer(kind = kint), pointer :: istack_nod_tbl_org(:)
!<   end address to send interpolated data
!
        integer(kind = kint), pointer  :: istack_nod_tbl_wtype_org(:)
!<   end address to send interpolated data including interpolate type
!
        integer(kind = kint) :: ntot_table_org
!<   total number of node to interpolate in original subdomain
        integer(kind = kint), pointer :: inod_gl_dest_4_org(:)
!<   global node ID for target domain
        integer(kind = kint), pointer :: iele_org_4_org(:)
!<   local element ID to make interpolation
        integer(kind = kint), pointer :: itype_inter_org(:)
!<   interpolation type ID
        real(kind = kreal), pointer :: coef_inter_org(:,:)
!<   Coordinate of target node in element coordinate
!
        integer(kind = kint), pointer :: istack_tbl_wtype_org_smp(:)
!<   end address of table to interpolation at original elements
        integer(kind = kint) :: imax_tbl_wtype_org_smp
!<   maximum number of interpolation at original elements
!
      end type interpolate_table_org
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_num_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
      integer(kind = kint) :: num
!
      allocate( tbl_org%id_dest_domain(tbl_org%num_dest_domain) )
      allocate( tbl_org%istack_nod_tbl_org(0:tbl_org%num_dest_domain) )
!
      num = 4*tbl_org%num_dest_domain
      allocate( tbl_org%istack_nod_tbl_wtype_org(0:num) )
!
      if (tbl_org%num_dest_domain .gt. 0) tbl_org%id_dest_domain = 0
      tbl_org%istack_nod_tbl_org =       -1
      tbl_org%istack_nod_tbl_wtype_org = -1
!
      end subroutine alloc_type_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_table_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
!
      allocate( tbl_org%inod_gl_dest_4_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%iele_org_4_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%itype_inter_org(tbl_org%ntot_table_org) )
      allocate( tbl_org%coef_inter_org(tbl_org%ntot_table_org,3) )
!
      if (tbl_org%ntot_table_org .gt. 0) then
        tbl_org%inod_gl_dest_4_org = 0
        tbl_org%iele_org_4_org =     0
        tbl_org%itype_inter_org =   -1
        tbl_org%coef_inter_org = 0.0d0
      end if
!
      end subroutine alloc_type_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_istack_tbl_wtp_smp(np_smp, tbl_org)
!
      integer(kind = kint), intent(in) :: np_smp
      type(interpolate_table_org), intent(inout) :: tbl_org
      integer(kind = kint) :: num
!
      num = 4*np_smp*tbl_org%num_dest_domain
      allocate(tbl_org%istack_tbl_wtype_org_smp(0:num))
      tbl_org%istack_tbl_wtype_org_smp = 0
!
      end subroutine alloc_type_istack_tbl_wtp_smp
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_itp_num_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      deallocate( tbl_org%id_dest_domain )
      deallocate( tbl_org%istack_nod_tbl_org )
      deallocate( tbl_org%istack_nod_tbl_wtype_org )
!
      end subroutine dealloc_type_itp_num_org
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_itp_table_org(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      deallocate( tbl_org%inod_gl_dest_4_org )
      deallocate( tbl_org%iele_org_4_org )
      deallocate( tbl_org%itype_inter_org )
      deallocate( tbl_org%coef_inter_org )
!
      end subroutine dealloc_type_itp_table_org
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_istack_tbl_wtp_smp(tbl_org)
!
      type(interpolate_table_org), intent(inout) :: tbl_org
!
      deallocate(tbl_org%istack_tbl_wtype_org_smp)
!
      end subroutine dealloc_type_istack_tbl_wtp_smp
!
!-----------------------------------------------------------------------
!
      end module t_interpolate_table_orgin
