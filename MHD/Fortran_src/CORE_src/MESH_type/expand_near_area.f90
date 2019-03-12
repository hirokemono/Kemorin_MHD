!>@file  expand_near_area.f90
!!       module expand_near_area
!!
!!@author H. Matsui
!!@date   Programmed in Oct., 2006
!
!> @brief Expand close node and element list
!!
!!@verbatim
!!      subroutine extend_near_area_no_flag(id_rank, iloop, mesh,       &
!!     &          next_ele_tbl, near_ele_tbl, near_node_tbl,            &
!!     &          near_ele_wide, near_node_wide)
!!        integer(kind = kint), intent(in) :: id_rank, iloop
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_around_node), intent(in) :: next_ele_tbl
!!        type(near_mesh), intent(inout) :: near_ele_tbl
!!        type(near_mesh), intent(inout) :: near_node_tbl
!!        type(near_mesh), intent(inout) :: near_ele_wide
!!        type(near_mesh), intent(inout) :: near_node_wide
!!
!!      subroutine extend_near_area_with_flag(id_rank, nref_neib,       &
!!     &          node, ele, next_ele_tbl, near_ele_tbl, near_node_tbl, &
!!     &          near_ele_wide, near_node_wide)
!!        integer(kind = kint), intent(in) :: id_rank, nref_neib
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_around_node), intent(in) :: next_ele_tbl
!!        type(near_mesh), intent(inout) :: near_ele_tbl
!!        type(near_mesh), intent(inout) :: near_node_tbl
!!        type(near_mesh), intent(inout) :: near_ele_wide
!!        type(near_mesh), intent(inout) :: near_node_wide
!!@endverbatim
!
      module expand_near_area
!
      use m_precision
!
      implicit none
!
      private :: expand_element_list, expand_node_list
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine extend_near_area_no_flag(id_rank, iloop, mesh,         &
     &          next_ele_tbl, near_ele_tbl, near_node_tbl,              &
     &          near_ele_wide, near_node_wide)
!
      use m_machine_parameter
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_near_mesh_id_4_node
!
      use expand_near_element
      use add_node_4_group
      use expand_near_flag
!
      integer(kind = kint), intent(in) :: id_rank, iloop
      type(mesh_geometry), intent(in) :: mesh
      type(element_around_node), intent(in) :: next_ele_tbl
!
      type(near_mesh), intent(inout) :: near_ele_tbl
      type(near_mesh), intent(inout) :: near_node_tbl
      type(near_mesh), intent(inout) :: near_ele_wide
      type(near_mesh), intent(inout) :: near_node_wide
!
      integer(kind = kint) :: i
!
!
      call allocate_mark_4_near_node(np_smp, mesh%node%numnod)
      call allocate_imark_4_ele(np_smp, mesh%ele%numele)
!
      call allocate_iflag_expand(mesh%node%numnod)
!
      do i = 1, iloop
!
        if(id_rank.eq. 0) write(*,*) 'expand loop ', i
!
        call expand_element_list(mesh%node, mesh%ele,                   &
     &      next_ele_tbl, near_node_tbl, near_ele_tbl, near_ele_wide)
        call expand_node_list(mesh%node, mesh%ele,                      &
     &      near_ele_tbl, near_node_tbl, near_node_wide)
      end do
!
      call deallocate_iflag_expand
      call deallocate_imark_4_ele
      call deallocate_mark_4_near_node
!
      end subroutine extend_near_area_no_flag
!
!-----------------------------------------------------------------------
!
      subroutine extend_near_area_with_flag(id_rank, nref_neib,         &
     &          node, ele, next_ele_tbl, near_ele_tbl, near_node_tbl,   &
     &          near_ele_wide, near_node_wide)
!
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_near_mesh_id_4_node
      use expand_near_element
      use add_node_4_group
      use expand_near_flag
!
      integer(kind = kint), intent(in) :: id_rank, nref_neib
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: next_ele_tbl
!
      type(near_mesh), intent(inout) :: near_ele_tbl
      type(near_mesh), intent(inout) :: near_node_tbl
      type(near_mesh), intent(inout) :: near_ele_wide
      type(near_mesh), intent(inout) :: near_node_wide
!
      integer(kind = kint) :: i, iflag_finish
!
!
      call allocate_mark_4_near_node(np_smp, node%numnod)
      call allocate_imark_4_ele(np_smp, ele%numele)
!
      call allocate_iflag_expand(node%numnod)
!
      call set_expand_flag(node%numnod, node%internal_node,             &
     &    nref_neib, near_node_tbl%num_nod, iflag_expand, &
     &    iflag_finish)
!
      i = 0
      do
        i = i + 1
        if(id_rank.eq. 0) write(*,*) 'expand loop ', i
!
        call expand_element_list(node, ele,                             &
     &      next_ele_tbl, near_node_tbl, near_ele_tbl, near_ele_wide)
        call expand_node_list(node, ele,                                &
     &      near_ele_tbl, near_node_tbl, near_node_wide)
!
        call set_expand_flag(node%numnod, node%internal_node,           &
     &      nref_neib, near_node_tbl%num_nod,                           &
     &      iflag_expand, iflag_finish)
!
        if (iflag_finish .eq. 0) exit
      end do
!
      call deallocate_iflag_expand
      call deallocate_imark_4_ele
      call deallocate_mark_4_near_node
!
      end subroutine extend_near_area_with_flag
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine expand_element_list(node, ele, next_ele_tbl,           &
     &          near_node_tbl, near_ele_tbl, near_ele_wide)
!
      use m_constants
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_near_mesh_id_4_node
!
      use copy_near_node_ele_type
      use expand_near_flag
      use expand_near_element
      use cal_minmax_and_stacks
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(element_around_node), intent(in) :: next_ele_tbl
      type(near_mesh), intent(in) :: near_node_tbl
      type(near_mesh), intent(inout) :: near_ele_tbl, near_ele_wide
!
!
      call alloc_num_4_near_nod(node%numnod, near_ele_wide)
!
      call count_expanded_near_element(np_smp,                          &
     &    node%numnod, ele%numele, node%istack_nod_smp, iflag_expand,   &
     &    next_ele_tbl%ntot, next_ele_tbl%istack_4_node,                &
     &    next_ele_tbl%iele_4_node, near_node_tbl%ntot,                 &
     &    near_node_tbl%istack_nod, near_node_tbl%id_near_nod,          &
     &    near_ele_tbl%ntot, near_ele_tbl%num_nod,                      &
     &    near_ele_tbl%istack_nod, near_ele_tbl%id_near_nod,            &
     &    near_ele_wide%num_nod)
!
      call s_cal_minmax_and_stacks(node%numnod, near_ele_wide%num_nod,  &
     &    izero, near_ele_wide%istack_nod, near_ele_wide%ntot,          &
     &    near_ele_wide%nmax, near_ele_wide%nmin)
!
      call alloc_near_element(near_ele_wide)
!
      call set_expanded_near_element(np_smp,                            &
     &    node%numnod, ele%numele, node%istack_nod_smp, iflag_expand,   &
     &    next_ele_tbl%ntot, next_ele_tbl%istack_4_node,                &
     &    next_ele_tbl%iele_4_node, near_node_tbl%ntot,                 &
     &    near_node_tbl%istack_nod, near_node_tbl%id_near_nod,          &
     &    near_ele_tbl%ntot, near_ele_tbl%num_nod,                      &
     &    near_ele_tbl%istack_nod, near_ele_tbl%id_near_nod,            &
     &    near_ele_wide%ntot, near_ele_wide%num_nod,                    &
     &    near_ele_wide%istack_nod, near_ele_wide%id_near_nod)
!
      call copy_extended_ele_id                                         &
     &   (node%numnod, near_ele_tbl, near_ele_wide)
!
      call dealloc_near_node(near_ele_wide)
      call dealloc_num_4_near_node(near_ele_wide)
!
!      write(*,*) 'nmax_ele_near_nod',near_ele_tbl%nmax
!      write(*,*) 'nmin_ele_near_nod',near_ele_tbl%nmin
!
      end subroutine expand_element_list
!
!-----------------------------------------------------------------------
!
      subroutine expand_node_list(node, ele,                            &
     &          near_ele_tbl, near_node_tbl, near_node_wide)
!
      use m_constants
      use m_machine_parameter
      use t_geometry_data
      use t_near_mesh_id_4_node
!
      use copy_near_node_ele_type
      use expand_near_flag
      use cal_minmax_and_stacks
      use add_node_4_group
      use set_distance_near_nod
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(near_mesh), intent(in) :: near_ele_tbl
      type(near_mesh), intent(inout) :: near_node_tbl, near_node_wide
!
!
      call alloc_num_4_near_nod(node%numnod, near_node_wide)
!
      call add_num_nod_4_group(np_smp, node%numnod,                     &
     &    ele%numele, ele%nnod_4_ele, ele%ie, node%istack_nod_smp,      &
     &    node%numnod, near_ele_tbl%ntot, near_ele_tbl%istack_nod,      &
     &    near_ele_tbl%id_near_nod, near_node_tbl%ntot,                 &
     &    near_node_tbl%istack_nod, near_node_tbl%id_near_nod,          &
     &    iflag_expand, near_node_wide%num_nod)
!
      call s_cal_minmax_and_stacks(node%numnod, near_node_wide%num_nod, &
     &    izero, near_node_wide%istack_nod, near_node_wide%ntot,        &
     &    near_node_wide%nmax, near_node_wide%nmin)
!
      call alloc_near_node(near_node_wide)
!
      call add_nod_id_4_group(np_smp, node%numnod,                      &
     &    ele%numele, ele%nnod_4_ele, ele%ie, node%istack_nod_smp,      &
     &    node%numnod, near_ele_tbl%ntot, near_ele_tbl%istack_nod,      &
     &    near_ele_tbl%id_near_nod, near_node_tbl%ntot,                 &
     &    near_node_tbl%istack_nod, near_node_tbl%id_near_nod,          &
     &    near_node_tbl%iweight, iflag_expand, near_node_wide%ntot,     &
     &    near_node_wide%istack_nod, near_node_wide%num_nod,            &
     &    near_node_wide%id_near_nod, near_node_wide%iweight)
!
      call add_distance_flag(np_smp, node%istack_nod_smp, node%numnod,  &
     &    near_node_tbl%ntot, near_node_tbl%istack_nod,                 &
     &    near_node_tbl%idist, near_node_wide%ntot,                     &
     &    near_node_wide%istack_nod, near_node_wide%idist)
!
      call sort_added_nod_4_group(node%numnod,                          &
     &    near_node_tbl%istack_nod, iflag_expand, near_node_wide%ntot,  &
     &    near_node_wide%istack_nod, near_node_wide%id_near_nod,        &
     &    near_node_wide%iweight)
!
      call copy_wider_id_2_near_type(node%numnod,                       &
     &    near_node_tbl, near_node_wide)
!
      call dealloc_near_node(near_node_wide)
      call dealloc_num_4_near_node(near_node_wide)
!
      end subroutine expand_node_list
!
!-----------------------------------------------------------------------
!
      end module expand_near_area
