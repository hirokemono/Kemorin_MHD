!set_table_4_RHS_assemble.f90
!      module set_table_4_RHS_assemble
!
!       Written by H. Matsui on Dec., 2008
!
!!      subroutine s_set_RHS_assemble_table(mesh, neib_tbl, rhs_tbl)
!!      subroutine set_belonged_ele_and_next_nod                        &
!!     &         (mesh, neib_ele, neib_nod)
!!      subroutine set_element_on_node_in_mesh(mesh, neib_ele)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(next_nod_ele_table), intent(inout) ::    neib_tbl
!!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!!
!!      subroutine empty_table_type_RHS_assemble(node, rhs_tbl, next_tbl)
!!        type(node_data),           intent(in) :: node
!!        type(next_nod_ele_table),     intent(inout) :: next_tbl
!!        type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
      module set_table_4_RHS_assemble
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_RHS_assemble_table(mesh, neib_tbl, rhs_tbl)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use set_ele_id_4_node_type
      use ordering_rhs_assemble_type
!
      type(mesh_geometry),       intent(in) :: mesh
      type(next_nod_ele_table), intent(inout) ::    neib_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
!  found surrounding node and element
      call set_belonged_ele_and_next_nod                                &
     &   (mesh, neib_tbl%neib_ele, neib_tbl%neib_nod)
!
!      set RHS assemble table
      call s_sort_node_index_type                                       &
     &   (mesh%node, neib_tbl%neib_ele, rhs_tbl)
!
      end subroutine s_set_RHS_assemble_table
!
!-----------------------------------------------------------------------
!
      subroutine set_belonged_ele_and_next_nod                          &
     &         (mesh, neib_ele, neib_nod)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use set_ele_id_4_node_type
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_around_node), intent(inout) :: neib_ele
      type(next_nod_id_4_nod), intent(inout) ::   neib_nod
!
!
!  found surrounding node and element
!
      call set_ele_id_4_node(mesh%node, mesh%ele, neib_ele)
      call const_next_nod_id_4_node                                     &
     &   (mesh%node, mesh%ele, neib_ele, neib_nod)
!
      end subroutine set_belonged_ele_and_next_nod
!
!-----------------------------------------------------------------------
!
      subroutine set_element_on_node_in_mesh(mesh, neib_ele)
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
      use set_ele_id_4_node_type
!
      type(mesh_geometry),       intent(in) :: mesh
      type(element_around_node), intent(inout) :: neib_ele
!
!
!  found surrounding node
      call set_ele_id_4_node(mesh%node, mesh%ele, neib_ele)
!
      end subroutine set_element_on_node_in_mesh
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_table_type_RHS_assemble(node, rhs_tbl, next_tbl)
!
      use m_machine_parameter
      use t_geometry_data
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      type(node_data),           intent(in) :: node
      type(next_nod_ele_table),     intent(inout) :: next_tbl
      type(tables_4_FEM_assembles), intent(inout) :: rhs_tbl
!
!
!  found surrounding node and element
!
      call alloc_numele_belonged(node%numnod, next_tbl%neib_ele)
!
      next_tbl%neib_ele%ntot = 0
      call alloc_iele_belonged(next_tbl%neib_ele)
!
!
      call alloc_num_next_node(node%numnod, next_tbl%neib_nod)
!
      next_tbl%neib_nod%ntot = 0
      call alloc_inod_next_node(next_tbl%neib_nod)
!
!      set RHS assemble table
!
      rhs_tbl%inod_ele_max = 0
      call alloc_type_sorted_node(np_smp, node%numnod, rhs_tbl)
!
      rhs_tbl%num_sort_smp = 0
      call alloc_type_sort_smp(rhs_tbl)
!
      end subroutine empty_table_type_RHS_assemble
!
!-----------------------------------------------------------------------
!
      end module set_table_4_RHS_assemble
      