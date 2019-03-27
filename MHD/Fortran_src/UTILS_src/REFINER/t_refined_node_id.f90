!t_refined_node_id.f90
!      module t_refined_node_id
!
!     Written by H. Matsui on Oct., 2007
!
!!      subroutine alloc_refine_type_num(num, refine_list)
!!      subroutine alloc_refine_type_item(refine_list)
!!      subroutine dealloc_refine_type_num(refine_list)
!!      subroutine dealloc_refine_type_item(refine_list)
!!        type(table_4_refine), intent(inout) :: refine_list
!!      subroutine check_refine_item(num, refine_list)
!!        type(table_4_refine), intent(in) :: refine_list
!!
!!      subroutine alloc_num_refine_node(node, ele, surf, edge, ref_ids)
!!      subroutine alloc_item_refine_node(ref_ids)
!!      subroutine dealloc_refine_node_id(ref_ids)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(refined_node_id), intent(inout) :: ref_ids
!!      subroutine check_all_refine_items(ref_ids)
!!        type(refined_node_id), intent(in) :: ref_ids
!!
!
      module t_refined_node_id
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type table_4_refine
        integer(kind = kint) :: num_refine
        integer(kind = kint), allocatable :: num_nod_refine(:)
        integer(kind = kint), allocatable :: istack_nod_refine(:)
!
        integer(kind = kint) :: ntot_nod_refine
        integer(kind = kint) :: nmax_nod_refine
        integer(kind = kint) :: nmin_nod_refine
        integer(kind = kint), allocatable :: inod_refine(:)
        real(kind = kreal), allocatable :: xi_refine(:,:)
        real(kind = kreal), allocatable :: x_refine(:,:)
        real(kind = kreal), allocatable :: sph_refine(:,:)
      end type table_4_refine
!
      type refined_node_id
        type(table_4_refine) :: refine_nod
        type(table_4_refine) :: refine_ele
        type(table_4_refine) :: refine_surf
        type(table_4_refine) :: refine_edge
      end type refined_node_id
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_refine_type_num(num, refine_list)
!
      integer(kind = kint), intent(in) :: num
      type(table_4_refine), intent(inout) :: refine_list
!
!
      refine_list%num_refine = num
      allocate(refine_list%num_nod_refine(refine_list%num_refine))
      allocate(refine_list%istack_nod_refine(0:refine_list%num_refine))
!
      refine_list%istack_nod_refine = -1
      if(refine_list%num_refine .gt. 0) refine_list%num_nod_refine = 0
!
      end subroutine alloc_refine_type_num
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_refine_type_item(refine_list)
!
      type(table_4_refine), intent(inout) :: refine_list
!
      allocate(refine_list%inod_refine(refine_list%ntot_nod_refine) )
      allocate(refine_list%xi_refine(refine_list%ntot_nod_refine,3) )
      allocate(refine_list%x_refine(refine_list%ntot_nod_refine,3)  )
      allocate(refine_list%sph_refine(refine_list%ntot_nod_refine,3))
!
      if(refine_list%ntot_nod_refine .le. 0) return
      refine_list%inod_refine =    0
      refine_list%xi_refine =  0.0d0
      refine_list%x_refine =   0.0d0
      refine_list%sph_refine = 0.0d0
!
      end subroutine alloc_refine_type_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_refine_type_num(refine_list)
!
      type(table_4_refine), intent(inout) :: refine_list
!
      deallocate(refine_list%num_nod_refine)
      deallocate(refine_list%istack_nod_refine)
!
      end subroutine dealloc_refine_type_num
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_refine_type_item(refine_list)
!
      type(table_4_refine), intent(inout) :: refine_list
!
      deallocate(refine_list%inod_refine)
      deallocate(refine_list%xi_refine )
      deallocate(refine_list%x_refine  )
      deallocate(refine_list%sph_refine)
!
      end subroutine dealloc_refine_type_item
!
!  ---------------------------------------------------------------------
!
      subroutine cal_stacks_4_refine_list(num, refine_list)
!
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: num
      type(table_4_refine), intent(inout) :: refine_list
!
!
      call s_cal_minmax_and_stacks                                      &
     &   (num, refine_list%num_nod_refine, izero,                       &
     &    refine_list%istack_nod_refine, refine_list%ntot_nod_refine,   &
     &    refine_list%nmax_nod_refine, refine_list%nmin_nod_refine)
!
      end subroutine cal_stacks_4_refine_list
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_refine_item(refine_list)
!
      type(table_4_refine), intent(in) :: refine_list
!
!
      integer(kind = kint) :: i, j, ist, ied
!
!
      write(50,*) 'refine_list%ntot_nod_refine',                        &
     &           refine_list%num_refine, refine_list%ntot_nod_refine
      do i = 1, refine_list%num_refine
        ist = refine_list%istack_nod_refine(i-1) + 1
        ied = refine_list%istack_nod_refine(i)
        write(50,*) i, refine_list%istack_nod_refine(i),                &
     &                 refine_list%num_nod_refine(i)
        do j = ist, ied
          write(50,*) refine_list%inod_refine(j),                       &
     &                refine_list%xi_refine(j,1:3)
        end do
      end do
!
      end subroutine check_refine_item
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_num_refine_node(node, ele, surf, edge, ref_ids)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(refined_node_id), intent(inout) :: ref_ids
!
!
      call alloc_refine_type_num(node%numnod, ref_ids%refine_nod)
      call alloc_refine_type_num(ele%numele, ref_ids%refine_ele)
      call alloc_refine_type_num(surf%numsurf, ref_ids%refine_surf)
      call alloc_refine_type_num(edge%numedge, ref_ids%refine_edge)
!
      end subroutine alloc_num_refine_node
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_item_refine_node(ref_ids)
!
      type(refined_node_id), intent(inout) :: ref_ids
!
!
      call alloc_refine_type_item(ref_ids%refine_nod)
      call alloc_refine_type_item(ref_ids%refine_ele)
      call alloc_refine_type_item(ref_ids%refine_surf)
      call alloc_refine_type_item(ref_ids%refine_edge)
!
      end subroutine alloc_item_refine_node
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_refine_node_id(ref_ids)
!
      type(refined_node_id), intent(inout) :: ref_ids
!
!
      call dealloc_refine_type_item(ref_ids%refine_nod)
      call dealloc_refine_type_item(ref_ids%refine_ele)
      call dealloc_refine_type_item(ref_ids%refine_surf)
      call dealloc_refine_type_item(ref_ids%refine_edge)
!
      call dealloc_refine_type_num(ref_ids%refine_nod)
      call dealloc_refine_type_num(ref_ids%refine_ele)
      call dealloc_refine_type_num(ref_ids%refine_surf)
      call dealloc_refine_type_num(ref_ids%refine_edge)
!
      end subroutine dealloc_refine_node_id
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_all_refine_items(ref_ids)
!
      type(refined_node_id), intent(in) :: ref_ids
!
!
      write(50,*) 'node at node'
      call check_refine_item(ref_ids%refine_nod)
      write(50,*) 'node on elememt'
      call check_refine_item(ref_ids%refine_ele)
      write(50,*) 'node on surface'
      call check_refine_item(ref_ids%refine_surf)
      write(50,*) 'node on edge'
      call check_refine_item(ref_ids%refine_edge)
!
      end subroutine check_all_refine_items
!
!  ---------------------------------------------------------------------
!
      end module t_refined_node_id
