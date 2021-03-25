!>@file   mesh_repartition_by_volume.f90
!!@brief  module mesh_repartition_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_mesh_repartition_by_volume                         &
!!     &         (org_fem, ele_comm, neib_nod, part_param,              &
!!     &          new_mesh, new_groups, new_ele_comm, repart_nod_tbl)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: org_fem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!        type(mesh_groups), intent(inout) :: new_groups
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!@endverbatim
!
      module mesh_repartition_by_volume
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_comm_table
      use t_calypso_comm_table
      use t_next_node_ele_4_node
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_mesh_repartition_by_volume                           &
     &         (org_fem, ele_comm, neib_nod, part_param,                &
     &          new_mesh, new_groups, new_ele_comm, repart_nod_tbl)
!
      use t_para_double_numbering
      use t_control_param_vol_grping
      use t_repart_double_numberings
      use t_repartition_by_volume
!
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use redistribute_groups
      use const_element_comm_tables
      use nod_and_ele_derived_info
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: org_fem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(mesh_geometry), intent(inout) :: new_mesh
      type(mesh_groups), intent(inout) :: new_groups
      type(communication_table), intent(inout) :: new_ele_comm
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!
      type(group_data) :: part_grp
      type(calypso_comm_table) :: ext_tbl
      type(calypso_comm_table) :: repart_ele_tbl
      type(node_ele_double_number) :: new_ids_on_org
!
!
!       Re-partitioning
      call grouping_by_volume(org_fem%mesh, part_param, part_grp)
!
      call alloc_double_numbering                                       &
     &   (org_fem%mesh%node%numnod, new_ids_on_org)
      call s_const_repart_nod_and_comm                                  &
     &   (org_fem%mesh, neib_nod, part_param, part_grp,                 &
     &    new_ids_on_org, new_mesh%nod_comm,                            &
     &    new_mesh%node, repart_nod_tbl, ext_tbl)
      call alloc_sph_node_geometry(new_mesh%node)
!
      call dealloc_group(part_grp)
      call dealloc_calypso_comm_table(ext_tbl)
!
      call s_const_repart_ele_connect                                   &
     &   (org_fem%mesh, ele_comm, repart_nod_tbl,                       &
     &    new_ids_on_org, new_mesh%nod_comm, new_mesh%node,             &
     &    new_mesh%ele, repart_ele_tbl, new_mesh%surf, new_mesh%edge)
      call dealloc_double_numbering(new_ids_on_org)
!
       call set_nod_and_ele_infos(new_mesh%node, new_mesh%ele)
       call const_ele_comm_table(new_mesh%node, new_mesh%nod_comm,      &
      &                          new_ele_comm, new_mesh%ele)
!
      call s_redistribute_groups                                        &
     &   (org_fem%mesh, org_fem%group, new_mesh, new_ele_comm,          &
     &    repart_nod_tbl, repart_ele_tbl, new_groups)
!
      end subroutine s_mesh_repartition_by_volume
!
! ----------------------------------------------------------------------
!
      end module mesh_repartition_by_volume
