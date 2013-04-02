!t_interpolate_tbl_dest.f90
!      module t_interpolate_tbl_dest
!
!> @brief Structure of interpolation table for target mesh
!
!      Written by H.Matsui on Nov., 2008
!
!      subroutine alloc_type_itp_num_dest(tbl_dest)
!      subroutine alloc_type_itp_table_dest(tbl_dest)
!      subroutine dealloc_type_itp_num_dest(tbl_dest)
!      subroutine dealloc_type_itp_table_dest(tbl_dest)
!
      module t_interpolate_tbl_dest
!
      use m_precision
!
      implicit none
!
!
!> Structure of interpolation table for target grid
      type interpolate_table_dest
!
        integer(kind = kint) :: num_org_domain
!<   number of subdomain to receive interpolated data
        integer(kind = kint) :: iflag_self_itp_recv
!<   flag if original nodes have same prosess
        integer(kind = kint), pointer :: id_org_domain(:)
!<   subdomain rank to receive interpolated data
        integer(kind = kint), pointer :: istack_nod_tbl_dest(:)
!<   end address to receive interpolated data
!
        integer(kind = kint), pointer :: istack_nod_tbl_wtype_dest(:)
!<   end address to receive interpolated data including interpolate type
!
        integer(kind = kint) :: ntot_table_dest
!<   total number of interpolated node in target subdomain
        integer(kind = kint), pointer :: inod_dest_4_dest(:)
!<   local node ID to set interpolated data
!
      end type interpolate_table_dest
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_num_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
      integer(kind = kint) :: num
!
!
      allocate(tbl_dest%id_org_domain(tbl_dest%num_org_domain))
      allocate(tbl_dest%istack_nod_tbl_dest(0:tbl_dest%num_org_domain))
!
      num = 4*tbl_dest%num_org_domain
      allocate(tbl_dest%istack_nod_tbl_wtype_dest(0:num))
!
      if (tbl_dest%num_org_domain .gt. 0) tbl_dest%id_org_domain = 0
      tbl_dest%istack_nod_tbl_dest =       -1
      tbl_dest%istack_nod_tbl_wtype_dest = -1
!
      end subroutine alloc_type_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine alloc_type_itp_table_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      allocate( tbl_dest%inod_dest_4_dest(tbl_dest%ntot_table_dest) )
      if (tbl_dest%ntot_table_dest .gt. 0) then
        tbl_dest%inod_dest_4_dest = 0
      end if
!
      end subroutine alloc_type_itp_table_dest
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_itp_num_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      deallocate( tbl_dest%id_org_domain )
      deallocate( tbl_dest%istack_nod_tbl_dest)
      deallocate( tbl_dest%istack_nod_tbl_wtype_dest)
!
      end subroutine dealloc_type_itp_num_dest
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_type_itp_table_dest(tbl_dest)
!
      type(interpolate_table_dest), intent(inout) :: tbl_dest
!
!
      deallocate( tbl_dest%inod_dest_4_dest )
!
      end subroutine dealloc_type_itp_table_dest
!
!-----------------------------------------------------------------------
!
      end module t_interpolate_tbl_dest
