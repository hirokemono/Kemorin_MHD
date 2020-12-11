!>@file   const_repart_ele_connect.f90
!!@brief  module const_repart_ele_connect
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine s_const_repart_ele_connect(mesh, ele_comm, part_tbl, &
!!     &          new_ids_on_org, ele_tbl, new_mesh)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(communication_table), intent(in) :: ele_comm
!!        type(calypso_comm_table), intent(in) :: part_tbl
!!        type(double_numbering_data), intent(in) :: new_ids_on_org
!!        type(calypso_comm_table), intent(inout) :: ele_tbl
!!        type(mesh_geometry), intent(inout) :: new_mesh
!!        type(double_numbering_data) :: element_ids
!!@endverbatim
!
      module const_repart_ele_connect
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_mesh_data
      use t_next_node_ele_4_node
      use t_calypso_comm_table
      use t_sorting_for_repartition
      use t_repart_double_numberings
!
      implicit none
!
      private :: const_reparition_ele_connect
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_const_repart_ele_connect(mesh, ele_comm, part_tbl,   &
     &          new_ids_on_org, ele_tbl, new_mesh)
!
      use ele_trans_tbl_4_repart
      use set_nnod_4_ele_by_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(communication_table), intent(in) :: ele_comm
      type(calypso_comm_table), intent(in) :: part_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
!
      type(calypso_comm_table), intent(inout) :: ele_tbl
      type(mesh_geometry), intent(inout) :: new_mesh
!
      type(double_numbering_data) :: element_ids
!
      integer(kind = kint) :: new_numele
!
!
      call alloc_double_numbering_data(mesh%ele%numele, element_ids)
      call double_numbering_4_element(mesh%ele, ele_comm, element_ids)
!
      call const_ele_trans_tbl_for_repart                               &
     &   (mesh%node, mesh%ele, part_tbl, new_ids_on_org%irank, ele_tbl)
!      call check_element_transfer_tbl(mesh%ele, ele_tbl)
! 
      call trim_overlapped_ele_by_repart                                &
     &   (mesh, element_ids, ele_tbl, new_numele)
!
      call const_reparition_ele_connect                                 &
     &   (mesh%node, mesh%ele, ele_tbl, new_ids_on_org,                 &
     &    element_ids, new_numele, new_mesh)
      call dealloc_double_numbering_data(element_ids)
!
      call set_3D_nnod_4_sfed_by_ele(new_mesh%ele%nnod_4_ele,           &
     &    new_mesh%surf%nnod_4_surf, new_mesh%edge%nnod_4_edge)
!
      end subroutine s_const_repart_ele_connect
!
! ----------------------------------------------------------------------
!
      subroutine const_reparition_ele_connect                           &
     &         (node, ele, ele_tbl, new_ids_on_org,                     &
     &          element_ids, new_numele, new_mesh)
!
      use search_ext_node_repartition
      use const_repart_mesh_data
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(calypso_comm_table), intent(in) :: ele_tbl
      type(double_numbering_data), intent(in) :: new_ids_on_org
      type(double_numbering_data), intent(in) :: element_ids
!
      integer(kind = kint), intent(in) :: new_numele
!
      type(mesh_geometry), intent(inout) :: new_mesh
!
      integer(kind = kint), allocatable :: ie_newnod(:,:)
      integer(kind = kint), allocatable :: ie_newdomain(:,:)
!
!
      allocate(ie_newnod(ele%numele,ele%nnod_4_ele))
      allocate(ie_newdomain(ele%numele,ele%nnod_4_ele))
!$omp parallel workshare
      ie_newnod(1:ele%numele,1:ele%nnod_4_ele) =    0
      ie_newdomain(1:ele%numele,1:ele%nnod_4_ele) = 0
!$omp end parallel workshare
!
      call set_repart_element_connect(new_numele, node, ele, ele_tbl,   &
     &    new_ids_on_org, ie_newdomain, ie_newnod, new_mesh%ele)
!
      call s_search_ext_node_repartition                                &
     &   (ele, ele_tbl, element_ids, ie_newdomain,                      &
     &    new_mesh%nod_comm, new_mesh%node, new_mesh%ele)
      deallocate(ie_newnod, ie_newdomain)
!
      end subroutine const_reparition_ele_connect
!
! ----------------------------------------------------------------------
!
      end module const_repart_ele_connect
