!>@file   const_repart_nod_and_comm.f90
!!@brief  module const_repart_nod_and_comm
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make Construct re-partitioned node and communication table
!!
!!@verbatim
!!      subroutine s_const_repart_nod_and_comm                          &
!!     &         (mesh, neib_nod, part_param, part_grp, new_ids_on_org, &
!!     &          new_comm, new_node, part_tbl, ext_tbl)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(volume_partioning_param), intent(in) :: part_param
!!        type(group_data), intent(in) :: part_grp
!!        type(communication_table), intent(inout) :: new_comm
!!        type(node_data), intent(inout) :: new_node
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!        type(calypso_comm_table), intent(inout) :: ext_tbl
!!        type(node_ele_double_number), intent(inout) :: new_ids_on_org
!!@endverbatim
!
      module const_repart_nod_and_comm
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_mesh_data
      use t_calypso_comm_table
      use t_control_param_vol_grping
      use t_sorting_for_repartition
      use t_para_double_numbering
      use t_repart_double_numberings
      use m_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_repart_nod_and_comm                            &
     &         (mesh, neib_nod, part_param, part_grp, new_ids_on_org,   &
     &          new_comm, new_node, part_tbl, ext_tbl)
!
      use external_group_4_new_part
      use ext_of_int_grp_4_new_part
      use const_comm_tbl_to_new_mesh
      use const_repart_mesh_data
      use const_repart_comm_tbl
      use check_data_for_repartition
!
      type(mesh_geometry), intent(in) :: mesh
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(volume_partioning_param), intent(in) :: part_param
      type(group_data), intent(in) :: part_grp
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(calypso_comm_table), intent(inout) :: part_tbl
      type(calypso_comm_table), intent(inout) :: ext_tbl
      type(node_ele_double_number), intent(inout) :: new_ids_on_org
!
      type(group_data) :: ext_int_grp
      type(group_data) :: ext_grp
      type(sorting_data_for_repartition) :: sort_nod
      type(node_ele_double_number) :: recieved_new_nod_ids
!
      integer(kind = kint) :: numnod, internal_node
!
!
      call const_int_comm_tbl_to_new_part(part_grp, part_tbl)
!    Set new_ids_on_org in internal node
      call node_dbl_numbering_to_repart(mesh%nod_comm, mesh%node,       &
     &    part_tbl, new_ids_on_org, SR_sig1, SR_i1)
!
      call const_external_grp_4_new_part(new_ids_on_org%irank,          &
     &    mesh%node, part_param, part_grp, ext_grp)
!       Re-partitioning for external node
      call const_ext_of_int_grp_new_part(mesh%node, neib_nod,           &
     &    part_param, part_grp, ext_grp, ext_int_grp)
      call const_ext_comm_tbl_to_new_part(ext_int_grp, ext_tbl)
      call dealloc_group(ext_int_grp)
      call dealloc_group(ext_grp)
!
!      Set local recieved_new_nod_ids in internal node
      internal_node =                part_tbl%ntot_import
      numnod = ext_tbl%ntot_import + part_tbl%ntot_import
!
      call alloc_double_numbering(numnod, recieved_new_nod_ids)
      call ext_node_dbl_numbering_by_SR(mesh%node, ext_tbl,             &
     &    new_ids_on_org, internal_node, recieved_new_nod_ids,          &
     &    SR_sig1, SR_i1)
!
      call alloc_sorting_data(ext_tbl%ntot_import, sort_nod)
      call sort_node_by_domain_and_index                                &
     &   (internal_node, recieved_new_nod_ids, ext_tbl, sort_nod)
      call dealloc_double_numbering(recieved_new_nod_ids)
!
      call const_repartitioned_comm_tbl                                 &
     &   (internal_node, sort_nod%num_recv, sort_nod%nrecv_trim,        &
     &    ext_tbl%ntot_import, sort_nod%irank_sorted,                   &
     &    sort_nod%id_sorted, sort_nod%iflag_dup, new_comm)
!
!      call check_num_of_neighbourings                                  &
!     &   (new_comm, ext_tbl, sort_nod%nrecv_trim)
!      call check_new_node_comm_table(my_rank, new_comm)
      call dealloc_sorting_data(sort_nod)
!
      call set_repart_node_position                                     &
     &   (part_tbl, mesh%node, new_comm, new_node)
      call check_repart_node_transfer                                   &
     &   (mesh%nod_comm, mesh%node, new_comm, new_node,                 &
     &    part_tbl, new_ids_on_org, SR_sig1, SR_i1)
!
      end subroutine s_const_repart_nod_and_comm
!
! ----------------------------------------------------------------------
!
      end module const_repart_nod_and_comm
