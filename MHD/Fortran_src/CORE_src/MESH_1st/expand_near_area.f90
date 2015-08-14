!expand_near_area.f90
!      module expand_near_area
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine extend_near_area_no_flag(iloop)
!      subroutine extend_near_area_with_flag(my_rank, nref_neib)
!      subroutine expand_element_list
!      subroutine expand_node_list
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
      subroutine extend_near_area_no_flag(my_rank, iloop)
!
      use m_geometry_data
      use m_machine_parameter
      use m_near_node_id_4_node
      use expand_near_element
      use add_node_4_group
      use expand_near_flag
!
      integer(kind = kint), intent(in) :: my_rank, iloop
      integer(kind = kint) :: i
!
!
      call allocate_mark_4_near_node(np_smp, node1%numnod)
      call allocate_imark_4_ele(np_smp, ele1%numele)
!
      call allocate_iflag_expand(node1%numnod)
!
      do i = 1, iloop
!
        if(my_rank.eq. 0) write(*,*) 'expand loop ', i
!
        call expand_element_list
        call expand_node_list
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
      subroutine extend_near_area_with_flag(my_rank, nref_neib)
!
      use m_machine_parameter
      use m_geometry_data
      use m_near_node_id_4_node
      use m_near_element_id_4_node
      use expand_near_element
      use add_node_4_group
      use expand_near_flag
!
      integer(kind = kint), intent(in) :: my_rank, nref_neib
      integer(kind = kint) :: i, iflag_finish
!
!
      call allocate_mark_4_near_node(np_smp, node1%numnod)
      call allocate_imark_4_ele(np_smp, ele1%numele)
!
      call allocate_iflag_expand(node1%numnod)
!
      call set_expand_flag(node1%numnod, node1%internal_node,           &
     &    nref_neib, near_node1_tbl%num_nod, iflag_expand,              &
     &    iflag_finish)
!
!      call check_near_elements(my_rank, node1%numnod, near_ele1_tbl)
!
      i = 0
      do
        i = i + 1
        if(my_rank.eq. 0) write(*,*) 'expand loop ', i
!
        call expand_element_list
        call expand_node_list
!
        call set_expand_flag(node1%numnod, node1%internal_node,         &
     &      nref_neib, near_node1_tbl%num_nod, iflag_expand,            &
     &      iflag_finish)
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
      subroutine expand_element_list
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data
      use m_element_id_4_node
      use m_near_node_id_4_node
      use m_near_element_id_4_node
      use expand_near_flag
      use expand_near_element
      use cal_minmax_and_stacks
      use copy_near_node_ele_type
!
!
      call alloc_num_4_near_nod(node1%numnod, near_ele1_wide)
      call count_expanded_near_element                                  &
     &   (np_smp, node1%numnod, ele1%numele, node1%istack_nod_smp,      &
     &    iflag_expand, ele_4_nod1%ntot, ele_4_nod1%istack_4_node,      &
     &    ele_4_nod1%iele_4_node,                                       &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_ele1_tbl%ntot,               &
     &    near_ele1_tbl%num_nod, near_ele1_tbl%istack_nod,              &
     &    near_ele1_tbl%id_near_nod, near_ele1_wide%num_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, near_ele1_wide%num_nod, izero,                  &
     &    near_ele1_wide%istack_nod, near_ele1_wide%ntot,               &
     &    near_ele1_wide%nmax, near_ele1_wide%nmin)
!
      call alloc_near_element(near_ele1_wide)
!
      call set_expanded_near_element                                    &
     &   (np_smp, node1%numnod, ele1%numele, node1%istack_nod_smp,      &
     &    iflag_expand, ele_4_nod1%ntot, ele_4_nod1%istack_4_node,      &
     &    ele_4_nod1%iele_4_node,                                       &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_ele1_tbl%ntot,               &
     &    near_ele1_tbl%num_nod, near_ele1_tbl%istack_nod,              &
     &    near_ele1_tbl%id_near_nod, near_ele1_wide%ntot,               &
     &    near_ele1_wide%num_nod, near_ele1_wide%istack_nod,            &
     &    near_ele1_wide%id_near_nod)
!
      call copy_extended_ele_id                                         &
     &   (node1%numnod, near_ele1_tbl, near_ele1_wide)
!
      call dealloc_near_node(near_ele1_wide)
      call dealloc_num_4_near_node(near_ele1_wide)
!
!      write(*,*) 'nmax_ele_near_nod',near_ele1_tbl%nmax
!      write(*,*) 'nmin_ele_near_nod',near_ele1_tbl%nmin
!
      end subroutine expand_element_list
!
!-----------------------------------------------------------------------
!
      subroutine expand_node_list
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data
      use m_near_node_id_4_node
      use m_near_element_id_4_node
!
      use expand_near_flag
      use cal_minmax_and_stacks
      use copy_near_node_ele_type
      use add_node_4_group
      use set_distance_near_nod
!
!
      call alloc_num_4_near_nod(node1%numnod, near_node1_wide)
!
      call add_num_nod_4_group                                          &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    node1%istack_nod_smp, node1%numnod, near_ele1_tbl%ntot,       &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, iflag_expand,                     &
     &    near_node1_wide%num_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (node1%numnod, near_node1_wide%num_nod, izero,                 &
     &    near_node1_wide%istack_nod, near_node1_wide%ntot,             &
     &    near_node1_wide%nmax, near_node1_wide%nmin)
!
      call alloc_near_node(near_node1_wide)
!
      call add_nod_id_4_group                                           &
     &   (np_smp, node1%numnod, ele1%numele, ele1%nnod_4_ele, ele1%ie,  &
     &    node1%istack_nod_smp, node1%numnod, near_ele1_tbl%ntot,       &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_node1_tbl%iweight,           &
     &    iflag_expand, near_node1_wide%ntot,                           &
     &    near_node1_wide%istack_nod, near_node1_wide%num_nod,          &
     &    near_node1_wide%id_near_nod, near_node1_wide%iweight)
!
      call add_distance_flag                                            &
     &   (np_smp, node1%istack_nod_smp, node1%numnod,                   &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%idist, near_node1_wide%ntot,                   &
     &    near_node1_wide%istack_nod, near_node1_wide%idist)
!
      call sort_added_nod_4_group                                       &
     &   (node1%numnod, near_node1_tbl%istack_nod, iflag_expand,        &
     &    near_node1_wide%ntot, near_node1_wide%istack_nod,             &
     &    near_node1_wide%id_near_nod, near_node1_wide%iweight)
!
      call copy_wider_id_2_near_type                                    &
     &   (node1%numnod, near_node1_tbl, near_node1_wide)
!
      call dealloc_near_node(near_node1_wide)
      call dealloc_num_4_near_node(near_node1_wide)
!
      end subroutine expand_node_list
!
!-----------------------------------------------------------------------
!
      end module expand_near_area
