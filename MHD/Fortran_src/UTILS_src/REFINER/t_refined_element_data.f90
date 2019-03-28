!t_refined_element_data.f90
!      module t_refined_element_data
!
!      Written by H. Matsui
!
!!      subroutine alloc_refine_flags(ele, surf, edge, refine_tbl)
!!      subroutine alloc_refined_num_element(ele, refine_tbl)
!!      subroutine alloc_refined_ele_connect(refine_tbl)
!!      subroutine alloc_old_refine_level(ele, refine_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_refine_table), intent(inout) :: refine_tbl
!!
!!      subroutine dealloc_refine_flags(refine_tbl)
!!      subroutine dealloc_refined_num_element(refine_tbl)
!!      subroutine dealloc_refined_ele_connect(refine_tbl)
!!      subroutine dealloc_old_refine_level(refine_tbl)
!!        type(element_refine_table), intent(inout) :: refine_tbl
!!
!!      subroutine check_refine_flags(ele, surf, edge, refine_tbl)
!!      subroutine check_refine_stack(ele, refine_tbl)
!!      subroutine check_local_refine_flags(ele, refine_tbl)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(edge_data), intent(in) :: edge
!!        type(element_refine_table), intent(in) :: refine_tbl
!
      module t_refined_element_data
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      implicit none
!
!
      type element_refine_table
        integer(kind = kint) :: max_refine_level
        integer(kind = kint), allocatable :: ilevel_refine(:)
        integer(kind = kint), allocatable :: ilevel_refine_old(:)
!
        integer(kind = kint), allocatable :: iflag_refine_ele(:)
        integer(kind = kint), allocatable :: iflag_refine_surf(:)
        integer(kind = kint), allocatable :: iflag_refine_edge(:)
!
!
        integer(kind = kint), allocatable :: iflag_refine_sf_lcl(:,:)
        integer(kind = kint), allocatable :: iflag_refine_ed_lcl(:,:)
!
!
        integer(kind = kint) :: ntot_ele_refined
        integer(kind = kint) :: nnod_4_ele_refined
        integer(kind = kint), allocatable :: num_ele_refined(:)
        integer(kind = kint), allocatable :: istack_ele_refined(:)
        integer(kind = kint), allocatable :: ie_refined(:,:)
!
        integer(kind = kint) :: iflag_tmp_tri_refine = 0
!
!
        integer(kind = kint) :: inod_refine_local(64)
!
        integer(kind = kint) :: inod_refine_nod_local(8)
        integer(kind = kint) :: inod_refine_ele_local(8)
        integer(kind = kint) :: inod_refine_surf_local(6,4)
        integer(kind = kint) :: inod_refine_edge_local(12,2)
      end type element_refine_table
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_refine_flags(ele, surf, edge, refine_tbl)
!
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_edge_data
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
!
      allocate(refine_tbl%ilevel_refine(ele%numele))
      allocate(refine_tbl%iflag_refine_ele(ele%numele))
!
      allocate(refine_tbl%iflag_refine_surf(surf%numsurf))
      allocate(refine_tbl%iflag_refine_edge(edge%numedge))
!
      allocate(refine_tbl%iflag_refine_sf_lcl(nsurf_4_ele,ele%numele))
      allocate(refine_tbl%iflag_refine_ed_lcl(nedge_4_ele,ele%numele))
!
      if(ele%numele .gt. 0) then
        refine_tbl%ilevel_refine =     0
        refine_tbl%iflag_refine_ele =  0
!
        refine_tbl%iflag_refine_sf_lcl = 0
        refine_tbl%iflag_refine_ed_lcl = 0
      end if
!
      if(surf%numsurf .gt. 0) refine_tbl%iflag_refine_surf = 0
      if(edge%numedge .gt. 0) refine_tbl%iflag_refine_edge = 0
!
      end subroutine alloc_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_refined_num_element(ele, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(inout) :: refine_tbl
!
!
      allocate(refine_tbl%num_ele_refined(ele%numele))
      allocate(refine_tbl%istack_ele_refined(0:ele%numele))
!
      refine_tbl%num_ele_refined = 0
      if(ele%numele .gt. 0) refine_tbl%istack_ele_refined = -1
!
      end subroutine alloc_refined_num_element
!
! -----------------------------------------------------------------------
!
      subroutine alloc_refined_ele_connect(refine_tbl)
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
      integer(kind = kint) :: nele, nnod_ele
!
      nele = refine_tbl%ntot_ele_refined
      nnod_ele = refine_tbl%nnod_4_ele_refined
      allocate(refine_tbl%ie_refined(nele,nnod_ele))
!
      if(refine_tbl%ntot_ele_refined .gt. 0) refine_tbl%ie_refined = 0
!
      end subroutine alloc_refined_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine alloc_old_refine_level(ele, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(inout) :: refine_tbl
!
      refine_tbl%max_refine_level = 0
      allocate(refine_tbl%ilevel_refine_old(ele%numele))
      if(ele%numele .gt. 0) refine_tbl%ilevel_refine_old = 0
!
      end subroutine alloc_old_refine_level
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_refine_flags(refine_tbl)
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
      deallocate( refine_tbl%ilevel_refine )
      deallocate( refine_tbl%iflag_refine_ele )
!
      deallocate( refine_tbl%iflag_refine_surf )
      deallocate( refine_tbl%iflag_refine_edge )
!
      deallocate( refine_tbl%iflag_refine_sf_lcl )
      deallocate( refine_tbl%iflag_refine_ed_lcl )
!
      end subroutine dealloc_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_refined_num_element(refine_tbl)
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
      deallocate(refine_tbl%num_ele_refined)
      deallocate(refine_tbl%istack_ele_refined)
!
      end subroutine dealloc_refined_num_element
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_refined_ele_connect(refine_tbl)
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
      deallocate(refine_tbl%ie_refined)
!
      end subroutine dealloc_refined_ele_connect
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_old_refine_level(refine_tbl)
!
      type(element_refine_table), intent(inout) :: refine_tbl
!
      deallocate( refine_tbl%ilevel_refine_old )
!
      end subroutine dealloc_old_refine_level
!
! -----------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_refine_flags(ele, surf, edge, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
!
      type(element_refine_table), intent(in) :: refine_tbl
!
      integer(kind = kint) :: i
!
      write(50,*) 'i, iflag_refine_ele(i)'
      do i = 1, ele%numele
        write(50,*) i, refine_tbl%iflag_refine_ele(i)
      end do
!
      write(50,*) 'i, iflag_refine_surf(i)'
      do i = 1, surf%numsurf
        write(50,*) i, refine_tbl%iflag_refine_surf(i)
      end do
!
      write(50,*) 'i, iflag_refine_edge(i)'
      do i= 1, edge%numedge
        write(50,*) i, refine_tbl%iflag_refine_edge(i)
      end do
!
      end subroutine check_refine_flags
!
!  ---------------------------------------------------------------------
!
      subroutine check_refine_stack(ele, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(in) :: refine_tbl
!
      integer(kind = kint) :: i
!
      write(50,*) 'i, istack_ele_refined(i)'
      do i = 1, ele%numele
        write(50,*) i, refine_tbl%istack_ele_refined(i),                &
     &                 refine_tbl%num_ele_refined(i)
      end do
!
      end subroutine check_refine_stack
!
!  ---------------------------------------------------------------------
!
      subroutine check_local_refine_flags(ele, refine_tbl)
!
      type(element_data), intent(in) :: ele
      type(element_refine_table), intent(in) :: refine_tbl
!
      integer(kind = kint) :: i
!
      write(50,*) 'i, iflag_refine_sf_lcl(i)'
      do i = 1, ele%numele
        write(50,'(i15,6i5)')                                           &
     &         i, refine_tbl%iflag_refine_sf_lcl(1:nsurf_4_ele,i)
      end do
!
      write(50,*) 'i, iflag_refine_ed_lcl(i)'
      do i = 1, ele%numele
        write(50,'(i15,12i5)')                                          &
     &         i, refine_tbl%iflag_refine_ed_lcl(1:nedge_4_ele,i)
      end do
!
      end subroutine check_local_refine_flags
!
!  ---------------------------------------------------------------------
!
      end module t_refined_element_data
