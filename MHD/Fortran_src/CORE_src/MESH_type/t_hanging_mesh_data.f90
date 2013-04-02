!t_hanging_mesh_data.f90
!      module t_hanging_mesh_data
!
!> @brief list for hangning nodes
!
!      Written by H.Matsui on May., 2012
!
!      subroutine alloc_iflag_hanging_type(nnod, nsurf, nedge,          &
!     &          hang_tbl)
!      subroutine dealloc_iflag_hanging_type(hang_tbl)
!
!      subroutine alloc_inod_hang_type(hang_tbl)
!      subroutine alloc_isurf_hang_type(hang_tbl)
!      subroutine alloc_iedge_hang_type(hang_tbl)
!
!      subroutine dealloc_inod_hang_type(hang_tbl)
!      subroutine dealloc_isurf_hang_type(hang_tbl)
!      subroutine dealloc_iedge_hang_type(hang_tbl)
!        integer(kind = kint), intent(in) :: nnod, nedge, nsurf
!        type(hanging_mesh), intent(inout) :: hang_tbl
!
      module t_hanging_mesh_data
!
      use m_precision
!
      implicit none
!
!
!>     Structure for hanging nodes and edges
      type hangining_list
!>        flag list for hanging nodes
        integer(kind = kint), pointer :: iflag_hang(:,:)
!
!>        number of hanginig on surface
        integer(kind = kint) :: n_sf
!>        number of hanginig on edge
        integer(kind = kint) :: n_ed
!>        local id of hanginig on surface
        integer(kind = kint), pointer :: id_sf(:,:)
!>        local id of hanginig on edge
        integer(kind = kint), pointer :: id_ed(:,:)
      end type hangining_list
!
!>     Structure for hanging nodes on mesh
      type hanging_mesh
!>     Structure for hanging nodes
        type(hangining_list) :: nod_hang
!>     Structure for hanging edges
        type(hangining_list) :: edge_hang
!>     Structure for hanging surfaces
        type(hangining_list) :: surf_hang
      end type hanging_mesh
!
      private :: alloc_iflag_hanging, dealloc_iflag_hanging
      private :: alloc_id_hang, dealloc_id_hang
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iflag_hanging(num, hang)
!
      integer(kind = kint), intent(in) :: num
      type(hangining_list), intent(inout) :: hang
!
      allocate(hang%iflag_hang(2,num))
      if(num .gt.  0) hang%iflag_hang =  0
!
      end subroutine alloc_iflag_hanging
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iflag_hanging(hang)
!
      type(hangining_list), intent(inout) :: hang
!
      deallocate(hang%iflag_hang)
!
      end subroutine dealloc_iflag_hanging
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iflag_hanging_type(nnod, nsurf, nedge,           &
     &          hang_tbl)
!
      integer(kind = kint), intent(in) :: nnod, nedge, nsurf
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call alloc_iflag_hanging(nnod, hang_tbl%nod_hang)
      call alloc_iflag_hanging(nedge, hang_tbl%edge_hang)
      call alloc_iflag_hanging(nsurf, hang_tbl%surf_hang)
!
      end subroutine alloc_iflag_hanging_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iflag_hanging_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call dealloc_iflag_hanging(hang_tbl%nod_hang)
      call dealloc_iflag_hanging(hang_tbl%edge_hang)
      call dealloc_iflag_hanging(hang_tbl%surf_hang)
!
      end subroutine dealloc_iflag_hanging_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_id_hang(hang)
!
      type(hangining_list), intent(inout) :: hang
!
      allocate(hang%id_sf(5,hang%n_sf))
      allocate(hang%id_ed(3,hang%n_ed))
!
      if(hang%n_sf .gt. 0) hang%id_sf = 0
      if(hang%n_ed .gt. 0) hang%id_ed = 0
!
      end subroutine alloc_id_hang
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_id_hang(hang)
!
      type(hangining_list), intent(inout) :: hang
!
      deallocate(hang%id_sf, hang%id_ed)
!
      end subroutine dealloc_id_hang
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_inod_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call alloc_id_hang(hang_tbl%nod_hang)
!
      end subroutine alloc_inod_hang_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_isurf_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call alloc_id_hang(hang_tbl%surf_hang)
!
      end subroutine alloc_isurf_hang_type
!
!-----------------------------------------------------------------------
!
      subroutine alloc_iedge_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      hang_tbl%edge_hang%n_sf = 0
      call alloc_id_hang(hang_tbl%edge_hang)
!
      end subroutine alloc_iedge_hang_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_inod_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call dealloc_id_hang(hang_tbl%nod_hang)
!
      end subroutine dealloc_inod_hang_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_isurf_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call dealloc_id_hang(hang_tbl%surf_hang)
!
      end subroutine dealloc_isurf_hang_type
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_iedge_hang_type(hang_tbl)
!
      type(hanging_mesh), intent(inout) :: hang_tbl
!
!
      call dealloc_id_hang(hang_tbl%edge_hang)
!
      end subroutine dealloc_iedge_hang_type
!
!-----------------------------------------------------------------------
!
      end module t_hanging_mesh_data
