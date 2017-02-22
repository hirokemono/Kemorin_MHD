!>@file   set_djds_connectivity_type.f90
!!@brief  module set_djds_connectivity_type
!!
!!@author H. Matsui
!!@date        Written by H. Matsui in Dec., 2008
!!@n      modified by H. Matsui on Nov., 2013
!
!>@brief     Construct index table for DJDS solver
!!
!!@verbatim
!!      subroutine s_set_djds_connectivity_type                         &
!!     &         (mesh, solver_C, next_tbl, DJDS_param, djds_tbl)
!!        type(mesh_geometry),       intent(in) :: mesh
!!        type(mpi_4_solver),        intent(in) :: solver_C
!!        type(next_nod_ele_table),  intent(in) :: next_tbl
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!
!!      subroutine set_djds_layer_connect_type(nnod_1ele,               &
!!     &          iele_start, iele_end, mesh, layer_comm,               &
!!     &          solver_C, DJDS_param, djds_tbl)
!!        integer(kind = kint), intent(in) :: iele_start, iele_end
!!        type(mesh_geometry),           intent(in) :: mesh
!!        type(communication_table), intent(in) :: layer_comm
!!        type(DJDS_poarameter), intent(in) :: DJDS_param
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!
!!      subroutine empty_djds_connectivity_type(mesh, djds_tbl)
!!        type(mesh_geometry),           intent(in) :: mesh
!!        type(DJDS_ordering_table), intent(inout) :: djds_tbl
!!@endverbatim
!
      module set_djds_connectivity_type
!
      use m_precision
      use m_machine_parameter
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_iccg_parameter
      use t_crs_connect
      use t_solver_djds
      use t_vector_for_solver
!
      use DJDS_new_comm_table
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
      subroutine s_set_djds_connectivity_type                           &
     &         (mesh, solver_C, next_tbl, DJDS_param, djds_tbl)
!
      type(mesh_geometry),      intent(in) :: mesh
      type(mpi_4_solver),       intent(in) :: solver_C
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      type(CRS_matrix_connect) :: MHD_crs
!
!C +-------------------------------+
!C | set connectivity in CRS array |
!C +-------------------------------+
!C===
      call s_set_crs_connection(mesh%node, next_tbl%neib_nod, MHD_crs)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
      call s_reordering_djds_smp(np_smp, mesh%node%numnod,              &
     &    mesh%node%internal_node, mesh%node%istack_internal_smp,       &
     &    solver_C, MHD_crs, DJDS_param, djds_tbl)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_new_comm_table_type(mesh%node%numnod, mesh%nod_comm,     &
     &    djds_tbl)
!
      call dealloc_crs_connect(MHD_crs)
!
      end subroutine s_set_djds_connectivity_type
!
!-----------------------------------------------------------------------
!
      subroutine set_djds_layer_connect_type(nnod_1ele,                 &
     &          iele_start, iele_end, mesh, layer_comm,                 &
     &          solver_C, DJDS_param, djds_tbl)
!
      use t_comm_table
      use set_ele_id_4_node_type
!
      integer(kind = kint), intent(in) :: nnod_1ele
      integer(kind = kint), intent(in) :: iele_start, iele_end
      type(mesh_geometry),       intent(in) :: mesh
      type(communication_table), intent(in) :: layer_comm
      type(mpi_4_solver),        intent(in) :: solver_C
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
      type(next_nod_ele_table) :: FEM_next
      type(CRS_matrix_connect) :: CRS_table
!
!
      call set_layerd_ele_id_4_node(nnod_1ele, iele_start, iele_end,    &
     &     mesh%node, mesh%ele, FEM_next%neib_ele)
!
      call const_next_nod_id_4_node(mesh%node, mesh%ele,                &
     &    FEM_next%neib_ele, FEM_next%neib_nod)
!
      call s_set_crs_connection(mesh%node, FEM_next%neib_nod, CRS_table)
!
      call s_reordering_djds_smp(np_smp, mesh%node%numnod,              &
     &    mesh%node%internal_node, mesh%node%istack_internal_smp,       &
     &    solver_C, CRS_table, DJDS_param, djds_tbl)
!
      call set_new_comm_table_type(mesh%node%numnod,                    &
     &    layer_comm, djds_tbl)
!
      call dealloc_crs_connect(CRS_table)
      call dealloc_iele_belonged(FEM_next%neib_ele)
      call dealloc_inod_next_node(FEM_next%neib_nod)
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
