!set_table_type_RHS_assemble.f90
!      module set_table_type_RHS_assemble
!
!       Written by H. Matsui on Dec., 2008
!
!      subroutine s_set_table_type_RHS_assemble(mesh, next_tbl, rhs_tbl)
!        type(mesh_geometry), intent(in) :: mesh
!        type(next_nod_ele_table),     intent(inout) :: next_tbl
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!      subroutine empty_table_type_RHS_assemble(mesh, rhs_tbl, next_tbl)
!        type(mesh_geometry), intent(in) :: mesh
!        type(next_nod_ele_table),     intent(inout) :: next_tbl
!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
      module set_table_type_RHS_assemble
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_table_type_RHS_assemble(mesh, rhs_tbl, next_tbl)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use set_ele_id_4_node_type
      use ordering_rhs_assemble_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_ele_table),     intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
!  found surrounding node and element
!
      call s_set_ele_id_4_node_type(mesh, next_tbl%neib_ele)
!
      call const_next_nod_id_4_node_type(mesh,                          &
     &    next_tbl%neib_ele, next_tbl%neib_nod)
!
!      set RHS assemble table
!
      call s_sort_node_index_type(mesh%node,                            &
     &    next_tbl%neib_ele, rhs_tbl)
!
      end subroutine s_set_table_type_RHS_assemble
!
!-----------------------------------------------------------------------
!
      subroutine empty_table_type_RHS_assemble(mesh, rhs_tbl, next_tbl)
!
      use m_machine_parameter
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_ele_table),     intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
!  found surrounding node and element
!
      call alloc_nele_belonged_type(mesh%node%numnod,                   &
     &    next_tbl%neib_ele)
!
      next_tbl%neib_ele%ntot = 0
      call alloc_iele_belonged_type(next_tbl%neib_ele)
!
!
      call alloc_num_next_node(mesh%node%numnod, next_tbl%neib_nod)
!
      next_tbl%neib_nod%ntot = 0
      call alloc_inod_next_node(next_tbl%neib_nod)
!
!      set RHS assemble table
!
      rhs_tbl%inod_ele_max = 0
      call alloc_type_sorted_node(np_smp, mesh%node%numnod, rhs_tbl)
!
      rhs_tbl%num_sort_smp = 0
      call alloc_type_sort_smp(rhs_tbl)
!
      end subroutine empty_table_type_RHS_assemble
!
!-----------------------------------------------------------------------
!
      end module set_table_type_RHS_assemble
      