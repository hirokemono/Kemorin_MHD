!>@file   const_internal_mesh_data.f90
!!       module const_internal_mesh_data
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine s_const_internal_mesh_data                           &
!!     &         (mesh, group, new_mesh, new_group)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!        type(mesh_groups), intent(inout) :: new_group
!!@endverbatim
!
      module const_internal_mesh_data
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_mesh_data
!
      implicit none
!
      private :: const_internal_mesh_geometry
      private :: const_internal_group_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_const_internal_mesh_data                             &
     &         (mesh, group, new_mesh, new_group)
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
!
      type(mesh_geometry), intent(inout) :: new_mesh
      type(mesh_groups), intent(inout) :: new_group
!
      integer(kind = kint), allocatable :: iele_to_new(:)
      integer(kind = kint), allocatable :: iele_to_org(:)
!
!
      allocate(iele_to_new(mesh%ele%numele))
      allocate(iele_to_org(mesh%ele%numele))
!$omp parallel workshare
      iele_to_new(1:mesh%ele%numele) = 0
      iele_to_org(1:mesh%ele%numele) = 0
!$omp end parallel workshare
!
      call const_internal_mesh_geometry                                 &
     &   (mesh%node, mesh%ele, iele_to_new, iele_to_org,                &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele)
!
      call const_internal_group_data                                    &
     &   (mesh%node, mesh%ele, group, iele_to_new, new_group)
!
      deallocate(iele_to_new, iele_to_org)
!
      end subroutine s_const_internal_mesh_data
!
! -----------------------------------------------------------------------
!
      subroutine const_internal_mesh_geometry                           &
     &         (node, ele, iele_to_new, iele_to_org,                    &
     &          new_comm, new_node, new_ele)
!
      use set_internal_mesh_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: new_node
      type(element_data), intent(inout) :: new_ele
!
      integer(kind = kint), intent(inout) :: iele_to_new(ele%numele)
      integer(kind = kint), intent(inout) :: iele_to_org(ele%numele)
!
!
      call find_internal_element                                        &
     &   (node%internal_node, ele%numele, ele%ie(1,1),                  &
     &    new_ele%numele, iele_to_new, iele_to_org)
!
!  Communication table
      new_comm%num_neib =    izero
      new_comm%ntot_import = izero
      new_comm%ntot_export = izero
      call alloc_comm_table_num(new_comm)
      call alloc_comm_table_item(new_comm)
!
!  Node data
      new_node%numnod =        node%internal_node
      new_node%internal_node = node%internal_node
      call alloc_node_geometry_base(new_node)
      call copy_internal_node_position(my_rank, node, new_node)
!
!  Element data
      call alloc_numnod_stack(nprocs, new_node)
      new_node%istack_numnod =   node%istack_internod
      new_node%istack_internod = node%istack_internod
!
      new_ele%nnod_4_ele = ele%nnod_4_ele
      call alloc_ele_connect(new_ele)
!
      call set_internal_element_connent                                 &
     &   (ele, iele_to_org, new_ele)
!
      call alloc_numele_stack(nprocs, new_ele)
      new_ele%istack_numele =   ele%istack_interele
      new_ele%istack_interele = ele%istack_interele
!
      end subroutine const_internal_mesh_geometry
!
! -----------------------------------------------------------------------
!
      subroutine const_internal_group_data                              &
     &         (node, ele, group, iele_to_new, new_group)
!
      use set_internal_mesh_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(mesh_groups), intent(in) :: group
      integer(kind = kint), intent(in) :: iele_to_new(ele%numele)
!
      type(mesh_groups), intent(inout) :: new_group
!
!
!  Node greoup
      new_group%nod_grp%num_grp = group%nod_grp%num_grp
      call alloc_group_num(new_group%nod_grp)
!
      call count_internal_node_grp                                      &
     &   (node%internal_node, group%nod_grp, new_group%nod_grp)
!
      call alloc_group_item(new_group%nod_grp)
      call set_internal_node_grp                                        &
     &   (node%internal_node, group%nod_grp, new_group%nod_grp)
!
!
!  element greoup
      new_group%ele_grp%num_grp = group%ele_grp%num_grp
      call alloc_group_num(new_group%ele_grp)
      call count_internal_element_grp                                   &
     &   (ele%numele, iele_to_new, group%ele_grp, new_group%ele_grp)
!
      call alloc_group_item(new_group%ele_grp)
      call set_internal_element_grp(ele%numele, iele_to_new,            &
     &    group%ele_grp, new_group%ele_grp)
!
!
!  surface greoup
      new_group%surf_grp%num_grp = group%surf_grp%num_grp
      call alloc_sf_group_num(new_group%surf_grp)
      call count_internal_surface_grp                                   &
     &   (ele%numele, iele_to_new, group%surf_grp, new_group%surf_grp)
!
      call alloc_sf_group_item(new_group%surf_grp)
      call set_internal_surface_grp(ele%numele, iele_to_new,            &
     &    group%surf_grp, new_group%surf_grp)
!
      end subroutine const_internal_group_data
!
! -----------------------------------------------------------------------
!
      end module const_internal_mesh_data

