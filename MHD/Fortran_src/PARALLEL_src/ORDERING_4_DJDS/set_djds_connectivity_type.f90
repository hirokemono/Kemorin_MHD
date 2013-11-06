!set_djds_connectivity_type.f90
!      module set_djds_connectivity_type
!
!     Written by H. Matsui on Dec., 2008
!
!!      subroutine s_set_djds_connectivity_type(mesh, next_tbl, djds_tbl)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(next_nod_ele_table),  intent(in) :: next_tbl
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!
!!      subroutine set_djds_layer_connect_type(nnod_1ele,               &
!!     &          iele_start, iele_end, mesh, layer_comm,               &
!!     &          next_tbl, djds_tbl)
!!        integer(kind = kint), intent(in) :: iele_start, iele_end
!!        type(mesh_geometry),           intent(in) :: mesh
!!        type(communication_table), intent(in) :: layer_comm
!!        type(next_nod_ele_table), intent(inout) :: next_tbl
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!
!!      subroutine empty_djds_connectivity_type(mesh, djds_tbl)
!!        type(mesh_geometry),           intent(in) :: mesh
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      module set_djds_connectivity_type
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_crs_connect
      use t_solver_djds
!
      use set_crs_connect_type
      use set_djds_smp_ordering_type
      use reordering_djds_smp_type
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_djds_connectivity_type(mesh, next_tbl, djds_tbl)
!
      type(mesh_geometry),      intent(in) :: mesh
      type(next_nod_ele_table), intent(in) :: next_tbl
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      type(CRS_matrix_connect) :: MHD_crs
!
!C +-------------------------------+
!C | set connectivity in CRS array |
!C +-------------------------------+
!C===
      call s_set_crs_connect_type(np_smp,                               &
     &    mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    next_tbl%neib_nod%ntot, next_tbl%neib_nod%istack_next,        &
     &    next_tbl%neib_nod%inod_next, MHD_crs)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      call s_reordering_djds_smp_type(np_smp,                           &
     &    mesh%node%numnod, mesh%node%internal_node,                    &
     &    mesh%node%istack_internal_smp, MHD_crs, djds_tbl)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_new_comm_table_type(mesh%node%numnod, mesh%nod_comm,     &
     &    djds_tbl)
!
      call dealloc_type_crs_connect(MHD_crs)
!
      end subroutine s_set_djds_connectivity_type
!
!-----------------------------------------------------------------------
!
      subroutine set_djds_layer_connect_type(nnod_1ele,                 &
     &          iele_start, iele_end, mesh, layer_comm,                 &
     &          next_tbl, djds_tbl)
!
      use t_comm_table
      use set_ele_id_4_node_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(mesh_geometry),       intent(in) :: mesh
      type(communication_table), intent(in) :: layer_comm
!
      type(next_nod_ele_table), intent(inout) :: next_tbl
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      type(CRS_matrix_connect) :: MHD_crs
!
!
      call set_layerd_ele_id_4_node_type(nnod_1ele,                     &
     &    iele_start, iele_end, mesh, next_tbl%neib_ele)
!
      call const_next_nod_id_4_node_type(mesh,                          &
     &    next_tbl%neib_ele, next_tbl%neib_nod)
!
      call s_set_crs_connect_type(np_smp,                               &
     &    mesh%node%numnod, mesh%node%istack_nod_smp,                   &
     &    next_tbl%neib_nod%ntot, next_tbl%neib_nod%istack_next,        &
     &    next_tbl%neib_nod%inod_next, MHD_crs)
!
      call s_reordering_djds_smp_type(np_smp,                           &
     &    mesh%node%numnod, mesh%node%internal_node,                    &
     &    mesh%node%istack_internal_smp, MHD_crs, djds_tbl)
!
      call set_new_comm_table_type(mesh%node%numnod,                    &
     &    layer_comm, djds_tbl)
!
      call dealloc_type_crs_connect(MHD_crs)
      call dealloc_iele_belonged_type(next_tbl%neib_ele)
      call dealloc_inod_next_node_type(next_tbl%neib_nod)
!
      end subroutine set_djds_layer_connect_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine empty_djds_connectivity_type(mesh, djds_tbl)
!
      use m_machine_parameter
!
      type(mesh_geometry),           intent(in) :: mesh
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      djds_tbl%NHYP =     0
      djds_tbl%itotal_l = 0
      djds_tbl%itotal_u = 0
      call alloc_type_4_RCM(mesh%node%numnod, djds_tbl)
      call alloc_type_number_4_djds(djds_tbl)
      call alloc_type_lists_4_DJDS(np_smp, mesh%node%numnod, djds_tbl)
      call alloc_type_address_4_DJDS(djds_tbl)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call alloc_type_new_comm_table(mesh%nod_comm%ntot_export, djds_tbl)
!
      end subroutine empty_djds_connectivity_type
!
!-----------------------------------------------------------------------
!
      end module set_djds_connectivity_type
