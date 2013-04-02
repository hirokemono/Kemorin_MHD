!
!      module ordering_rhs_assemble_type
!
!      Written by H.Matsui on Nov., 2008
!
!      subroutine s_sort_node_index_type(nod, neib_ele, rhs_tbl)
!      type(node_data),           intent(in) :: nod
!      type(element_around_node), intent(in) :: neib_ele
!      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
      module ordering_rhs_assemble_type
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_sort_node_index_type(nod, neib_ele, rhs_tbl)
!
      use m_constants
!
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      use ordering_4_rhs_assemble
      use cal_minmax_and_stacks
!
      type(node_data),           intent(in) :: nod
      type(element_around_node), intent(in) :: neib_ele
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
      rhs_tbl%inod_ele_max = neib_ele%nmax
      call alloc_type_sorted_node(np_smp, nod%numnod, rhs_tbl)
!
!    count number of node to RHS assemble
!
      call count_nele_4_RHS_assemble(nod%numnod,                        &
     &    neib_ele%nmax, neib_ele%nele_4_node, np_smp,                  &
     &    rhs_tbl%inod_ele_max, rhs_tbl%nnod_sort_smp)
!
!    stacking for SMP
!
      call s_cal_dbl_minmax_and_stacks(np_smp, neib_ele%nmax,           &
     &    rhs_tbl%nnod_sort_smp, izero, rhs_tbl%nod_stack_smp,          &
     &    rhs_tbl%num_sort_smp, rhs_tbl%nmax_sort_smp,                  &
     &    rhs_tbl%nmin_sort_smp)
!
!       write(*,*) 'num_sort_smp', rhs_tbl%num_sort_smp
!
      call alloc_type_sort_smp(rhs_tbl)
!
!       write(*,*) 'allocate smp end'
!
      call set_iele_4_RHS_assemble(nod%numnod, neib_ele%nmax,           &
     &    neib_ele%nele_4_node, neib_ele%istack_4_node, np_smp,         &
     &    rhs_tbl%inod_ele_max, rhs_tbl%nod_stack_smp, neib_ele%ntot,   &
     &    neib_ele%iele_4_node, neib_ele%iconn_4_node,                  &
     &    rhs_tbl%num_sort_smp, rhs_tbl%node_sort_list_smp,             &
     &    rhs_tbl%iele_sort_smp, rhs_tbl%iconn_sort_smp)
!
!       write(*,*) 'set smp for RHS end'
!
      end  subroutine s_sort_node_index_type
!
!-----------------------------------------------------------------------
!
      end module ordering_rhs_assemble_type
