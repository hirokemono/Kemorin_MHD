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
      use m_geometry_parameter
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
      call allocate_mark_4_near_node(np_smp, numnod)
      call allocate_imark_4_ele(np_smp, numele)
!
      call allocate_iflag_expand(numnod)
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
      use m_geometry_parameter
      use m_machine_parameter
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
      call allocate_mark_4_near_node(np_smp, numnod)
      call allocate_imark_4_ele(np_smp, numele)
!
      call allocate_iflag_expand(numnod)
!
      call set_expand_flag(numnod, internal_node, nref_neib,            &
     &    near_node1_tbl%num_nod, iflag_expand, iflag_finish)
!
!      call check_near_elements(my_rank, numnod, near_ele1_tbl)
!
      i = 0
      do
        i = i + 1
        if(my_rank.eq. 0) write(*,*) 'expand loop ', i
!
        call expand_element_list
        call expand_node_list
!
        call set_expand_flag(numnod, internal_node, nref_neib,          &
     &      near_node1_tbl%num_nod, iflag_expand, iflag_finish)
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
      use m_geometry_parameter
      use m_element_id_4_node
      use m_near_node_id_4_node
      use m_near_element_id_4_node
      use expand_near_flag
      use expand_near_element
      use cal_minmax_and_stacks
      use copy_near_node_and_element
!
!
      call alloc_num_4_near_nod(numnod, near_ele1_wide)
      call count_expanded_near_element(np_smp,                          &
     &    numnod, numele, inod_smp_stack, iflag_expand,                 &
     &    ntot_ele_4_node, iele_stack_4_node, iele_4_node,              &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_ele1_tbl%ntot,               &
     &    near_ele1_tbl%num_nod, near_ele1_tbl%istack_nod,              &
     &    near_ele1_tbl%id_near_nod, near_ele1_wide%num_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (numnod, near_ele1_wide%num_nod, izero,                        &
     &    near_ele1_wide%istack_nod, near_ele1_wide%ntot,               &
     &    near_ele1_wide%nmax, near_ele1_wide%nmin)
!
      call alloc_near_element(near_ele1_wide)
!
      call set_expanded_near_element(np_smp,                            &
     &    numnod, numele, inod_smp_stack, iflag_expand,                 &
     &    ntot_ele_4_node, iele_stack_4_node, iele_4_node,              &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_ele1_tbl%ntot,               &
     &    near_ele1_tbl%num_nod, near_ele1_tbl%istack_nod,              &
     &    near_ele1_tbl%id_near_nod, near_ele1_wide%ntot,               &
     &    near_ele1_wide%num_nod, near_ele1_wide%istack_nod,            &
     &    near_ele1_wide%id_near_nod)
!
      call copy_extended_ele_id(numnod, near_ele1_tbl, near_ele1_wide)
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
      use m_geometry_parameter
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
      call alloc_num_4_near_nod(numnod, near_node1_wide)
!
      call add_num_nod_4_group(np_smp, numnod, numele, nnod_4_ele, ie,  &
     &    inod_smp_stack, numnod, near_ele1_tbl%ntot,                   &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, iflag_expand,                     &
     &    near_node1_wide%num_nod)
!
      call s_cal_minmax_and_stacks                                      &
     &   (numnod, near_node1_wide%num_nod, izero,                       &
     &    near_node1_wide%istack_nod, near_node1_wide%ntot,             &
     &    near_node1_wide%nmax, near_node1_wide%nmin)
!
      call alloc_near_node(near_node1_wide)
!
      call add_nod_id_4_group(np_smp, numnod, numele, nnod_4_ele, ie,   &
     &    inod_smp_stack, numnod, near_ele1_tbl%ntot,                   &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_node1_tbl%ntot, near_node1_tbl%istack_nod,               &
     &    near_node1_tbl%id_near_nod, near_node1_tbl%iweight,           &
     &    iflag_expand, near_node1_wide%ntot,                           &
     &    near_node1_wide%istack_nod, near_node1_wide%num_nod,          &
     &    near_node1_wide%id_near_nod, near_node1_wide%iweight)
!
      call add_distance_flag                                            &
     &   (np_smp, inod_smp_stack, numnod,  near_node1_tbl%ntot,         &
     &    near_node1_tbl%istack_nod, near_node1_tbl%idist,              &
     &    near_node1_wide%ntot, near_node1_wide%istack_nod,             &
     &    near_node1_wide%idist)
!
      call sort_added_nod_4_group                                       &
     &   (numnod, near_node1_tbl%istack_nod, iflag_expand,              &
     &    near_node1_wide%ntot, near_node1_wide%istack_nod,             &
     &    near_node1_wide%id_near_nod, near_node1_wide%iweight)
!
      call copy_wider_id_2_near_type                                    &
     &   (numnod, near_node1_tbl, near_node1_wide)
!
      call dealloc_near_node(near_node1_wide)
      call dealloc_num_4_near_node(near_node1_wide)
!
      end subroutine expand_node_list
!
!-----------------------------------------------------------------------
!
      end module expand_near_area
