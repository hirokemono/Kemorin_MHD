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
!!     &          new_mesh, new_group, new_ele_comm,                    &
!!     &          repart_nod_tbl, repart_ele_tbl,  m_SR)
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
!!        type(communication_table), intent(in) :: new_ele_comm
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
      use t_para_double_numbering
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
     &          new_mesh, new_group, new_ele_comm,                      &
     &          repart_nod_tbl, repart_ele_tbl,  m_SR)
!
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
      type(communication_table), intent(in) :: new_ele_comm
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
      call s_redistribute_groups                                        &
     &   ((.FALSE.), mesh, group, ele_comm, new_mesh, new_ele_comm,     &
     &    repart_nod_tbl, repart_ele_tbl, new_group,                    &
     &    m_SR%SR_sig, m_SR%SR_i)
!
      end subroutine s_mesh_repartition_by_volume
!
! ----------------------------------------------------------------------
!
      subroutine const_repart_mesh_by_table                             &
     &         (mesh, group, part_nod_tbl, part_ele_tbl, new_ele_comm,  &
     &          new_numele, new_mesh, new_group, m_SR)
!
      use const_element_comm_tables
      use const_repart_mesh_data
      use const_repart_nod_and_comm
      use const_repart_ele_connect
      use redistribute_groups
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(calypso_comm_table), intent(in) :: part_nod_tbl
      type(calypso_comm_table), intent(in) :: part_ele_tbl
      type(communication_table), intent(in) :: new_ele_comm
      integer(kind = kint), intent(in) :: new_numele
!
      type(mesh_geometry), intent(inout) :: new_mesh
      type(mesh_groups), intent(inout) ::   new_group
      type(mesh_SR), intent(inout) :: m_SR
!
      type(communication_table), save :: ele_comm
      type(node_ele_double_number), save :: new_ids_on_org
      type(node_ele_double_number), save :: new_iele_dbl
!
!
      if(iflag_debug.gt.0) write(*,*)' const_ele_comm_table'
!      call const_global_numele_list(mesh%ele)
      call const_ele_comm_table(mesh%node,                              &
     &    mesh%nod_comm, mesh%ele, ele_comm, m_SR)
!
      call alloc_double_numbering(mesh%node%numnod, new_ids_on_org)
      call node_dbl_numbering_to_repart                                 &
     &   (mesh%nod_comm, mesh%node, part_nod_tbl,                       &
     &    new_ids_on_org, m_SR%SR_sig, m_SR%SR_i)
!
      call alloc_double_numbering(new_mesh%ele%numele,                  &
     &                            new_iele_dbl)
      call double_numbering_4_element(new_mesh%ele, new_ele_comm,       &
     &    new_iele_dbl, m_SR%SR_sig, m_SR%SR_i)
!
!
      call set_repart_node_position(part_nod_tbl, mesh%node,            &
     &    new_mesh%nod_comm, new_mesh%node,                             &
     &    m_SR%SR_sig, m_SR%SR_r, m_SR%SR_il)
      call alloc_sph_node_geometry(new_mesh%node)
!
!
      call const_repart_ele_connect_by_tbl(new_numele,                  &
     &   mesh, ele_comm, part_ele_tbl, new_ids_on_org,                  &
     &   new_mesh%nod_comm, new_mesh%node, new_ele_comm, new_iele_dbl,  &
     &   new_mesh%ele, m_SR%SR_sig, m_SR%SR_i, m_SR%SR_il)
      call set_3D_nnod_4_sfed_by_ele(new_mesh%ele%nnod_4_ele,           &
     &    new_mesh%surf%nnod_4_surf, new_mesh%edge%nnod_4_edge)
      call dealloc_double_numbering(new_iele_dbl)
      call dealloc_double_numbering(new_ids_on_org)
!
!
      call s_redistribute_groups                                        &
     &   ((.TRUE.), mesh, group, ele_comm,                              &
     &    new_mesh, new_ele_comm, part_nod_tbl, part_ele_tbl,           &
     &    new_group, m_SR%SR_sig, m_SR%SR_i)
      call dealloc_comm_table(ele_comm)
!
      end subroutine const_repart_mesh_by_table
!
! ----------------------------------------------------------------------
!
      end module mesh_repartition_by_volume
