!>@file   mesh_repartition_by_volume.f90
!!@brief  module mesh_repartition_by_volume
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_mesh_repartition_by_volume(org_fem, ele_comm,      &
!!     &          neib_nod, part_param, new_fem, org_to_new_tbl)
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_data), intent(in) :: org_fem
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(mesh_data), intent(inout) :: new_fem
!!        type(calypso_comm_table) intent(inout) :: org_to_new_tbl
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
      use m_work_time
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_mesh_repartition_by_volume(org_fem, ele_comm,        &
     &          neib_nod, part_param, new_fem, org_to_new_tbl)
!
      use t_control_param_vol_grping
      use t_repart_double_numberings
      use t_repartition_by_volume
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use redistribute_groups
      use parallel_sleeve_extension
!
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_data), intent(in) :: org_fem
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
!
      type(mesh_data), intent(inout) :: new_fem
      type(calypso_comm_table), intent(inout) :: org_to_new_tbl
!
      type(group_data) :: part_grp
!
      type(calypso_comm_table) :: ext_tbl
      type(calypso_comm_table) :: ele_tbl
!
      type(double_numbering_data) :: new_ids_on_org
      integer(kind = kint) :: i_level
!
!
!       Re-partitioning
      call grouping_by_volume(org_fem%mesh, part_param, part_grp)
!
      call alloc_double_numbering_data                                  &
     &   (org_fem%mesh%node%numnod, new_ids_on_org)
      call s_const_repart_nod_and_comm                                  &
     &   (org_fem%mesh, neib_nod, part_param, part_grp,                 &
     &    new_ids_on_org, new_fem%mesh%nod_comm,                        &
     &    new_fem%mesh%node, org_to_new_tbl, ext_tbl)
      call alloc_sph_node_geometry(new_fem%mesh%node)
!
      call dealloc_group(part_grp)
      call dealloc_calypso_comm_table(ext_tbl)
!
      call s_const_repart_ele_connect                                   &
     &   (org_fem%mesh, ele_comm, org_to_new_tbl,                       &
     &    new_ids_on_org, ele_tbl, new_fem%mesh)
      call dealloc_double_numbering_data(new_ids_on_org)
!
      call s_redistribute_groups(org_fem%mesh, org_fem%group, ele_comm, &
     &    new_fem%mesh, org_to_new_tbl, ele_tbl, new_fem%group)
      call dealloc_calypso_comm_table(ele_tbl)
!
! Increase sleeve size
      do i_level = 2, part_param%num_FEM_sleeve
        if(my_rank .eq. 0) write(*,*) 'extend sleeve:', i_level
        call para_sleeve_extension(new_fem%mesh, new_fem%group)
      end do
!
      end subroutine s_mesh_repartition_by_volume
!
! ----------------------------------------------------------------------
!
      end module mesh_repartition_by_volume
