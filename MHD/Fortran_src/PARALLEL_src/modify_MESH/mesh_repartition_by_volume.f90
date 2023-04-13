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
!!     &         (mesh, group, ele_comm, neib_nod, part_param,          &
!!     &          num_mask, masking, ref_repart, d_mask,                &
!!     &          new_mesh, new_group, repart_nod_tbl, repart_ele_tbl,  &
!!     &          m_SR)
!!        integer(kind = kint), intent(in) :: num_mask
!!        type(volume_partioning_param), intent(in) ::  part_param
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: groups
!!        type(communication_table), intent(in) :: ele_comm
!!        type(next_nod_id_4_nod), intent(in) :: neib_nod
!!        type(masking_parameter), intent(in) :: masking(num_mask)
!!        real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: d_mask(mesh%node%numnod,num_mask)
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!        type(mesh_groups), intent(inout) :: new_group
!!        type(calypso_comm_table), intent(inout) :: repart_nod_tbl
!!        type(calypso_comm_table), intent(inout) :: repart_ele_tbl
!!        type(mesh_SR), intent(inout) :: m_SR
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
      use t_mesh_SR
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
     &         (mesh, group, ele_comm, neib_nod, part_param,            &
     &          num_mask, masking, ref_repart, d_mask,                  &
     &          new_mesh, new_group, repart_nod_tbl, repart_ele_tbl,    &
     &          m_SR)
!
      use t_para_double_numbering
      use t_control_param_vol_grping
      use t_repart_double_numberings
      use t_repartition_by_volume
!
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use redistribute_groups
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: num_mask
      type(volume_partioning_param), intent(in) ::  part_param
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(communication_table), intent(in) :: ele_comm
      type(next_nod_id_4_nod), intent(in) :: neib_nod
      type(masking_parameter), intent(in) :: masking(num_mask)
      real(kind = kreal), intent(in) :: ref_repart(mesh%node%numnod)
      real(kind = kreal), intent(in)                                    &
     &                   :: d_mask(mesh%node%numnod,num_mask)
!
      type(mesh_geometry), intent(inout) :: new_mesh
      type(mesh_groups), intent(inout) :: new_group
      type(calypso_comm_table), intent(inout) :: repart_nod_tbl
      type(calypso_comm_table), intent(inout) :: repart_ele_tbl
      type(mesh_SR), intent(inout) :: m_SR
!
      type(group_data) :: part_grp
      type(calypso_comm_table) :: ext_tbl
      type(node_ele_double_number) :: new_ids_on_org
!
!
!       Re-partitioning
      call grouping_by_volume(mesh, part_param, num_mask, masking,      &
     &    ref_repart, d_mask, part_grp, m_SR%SR_sig, m_SR%SR_r)
!
      call alloc_double_numbering(mesh%node%numnod, new_ids_on_org)
      call s_const_repart_nod_and_comm                                  &
     &   (mesh, neib_nod, part_param, part_grp,                         &
     &    new_ids_on_org, new_mesh%nod_comm, new_mesh%node,             &
     &    repart_nod_tbl, ext_tbl, m_SR)
      call alloc_sph_node_geometry(new_mesh%node)
!
      call dealloc_group(part_grp)
      call dealloc_calypso_comm_table(ext_tbl)
!
      call s_const_repart_ele_connect                                   &
     &   (mesh, ele_comm, repart_nod_tbl, new_ids_on_org,               &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele,               &
     &    repart_ele_tbl, m_SR%SR_sig, m_SR%SR_i, m_SR%SR_il)
      call dealloc_double_numbering(new_ids_on_org)
      call set_3D_nnod_4_sfed_by_ele(new_mesh%ele%nnod_4_ele,           &
     &    new_mesh%surf%nnod_4_surf, new_mesh%edge%nnod_4_edge)
!
      call s_redistribute_groups(mesh, group, ele_comm, new_mesh,       &
     &    repart_nod_tbl, repart_ele_tbl, new_group,                    &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      end subroutine s_mesh_repartition_by_volume
!
! ----------------------------------------------------------------------
!
      end module mesh_repartition_by_volume
