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
     &      nnod_near_nod, iflag_expand, iflag_finish)
!
!      call check_near_4_nod_t(my_rank, numnod, near_ele1_tbl)
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
     &      nnod_near_nod, iflag_expand, iflag_finish)
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
     &    ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,        &
     &    near_ele1_tbl%ntot, near_ele1_tbl%num_nod,                    &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_ele1_wide%num_nod)
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
     &    ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,        &
     &    near_ele1_tbl%ntot, near_ele1_tbl%num_nod,                    &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    near_ele1_wide%ntot, near_ele1_wide%num_nod,                  &
     &    near_ele1_wide%istack_nod, near_ele1_wide%id_near_nod)
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
      use copy_near_node_and_element
      use add_node_4_group
      use set_distance_near_nod
!
!
      call allocate_num_4_near_nod_w(numnod)
!
      call add_num_nod_4_group(np_smp, numnod, numele, nnod_4_ele, ie,  &
     &    inod_smp_stack, numnod, near_ele1_tbl%ntot,                   &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,        &
     &    iflag_expand, nnod_near_nod_w)
!
      call s_cal_minmax_and_stacks(numnod, nnod_near_nod_w, izero,      &
     &      inod_stack_near_nod_w, ntot_nod_near_nod_w,                 &
     &      nmax_nod_near_nod_w, nmin_nod_near_nod_w)
!
      call allocate_near_node_w
!
      call add_nod_id_4_group(np_smp, numnod, numele, nnod_4_ele, ie,   &
     &    inod_smp_stack, numnod, near_ele1_tbl%ntot,                   &
     &    near_ele1_tbl%istack_nod, near_ele1_tbl%id_near_nod,          &
     &    ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,        &
     &    iweight_node, iflag_expand, ntot_nod_near_nod_w,              &
     &    inod_stack_near_nod_w, nnod_near_nod_w,                       &
     &    inod_near_nod_w, iweight_node_w)
!
      call add_distance_flag(np_smp, inod_smp_stack, numnod,            &
     &    ntot_nod_near_nod, inod_stack_near_nod, idist_from_center,    &
     &    ntot_nod_near_nod_w, inod_stack_near_nod_w,                   &
     &    idist_from_center_w)
!
      call sort_added_nod_4_group(numnod, inod_stack_near_nod,          &
     &    iflag_expand, ntot_nod_near_nod_w, inod_stack_near_nod_w,     &
     &    inod_near_nod_w, iweight_node_w)
!
      call copy_wider_node_id_2_near
!
      end subroutine expand_node_list
!
!-----------------------------------------------------------------------
!
      end module expand_near_area
