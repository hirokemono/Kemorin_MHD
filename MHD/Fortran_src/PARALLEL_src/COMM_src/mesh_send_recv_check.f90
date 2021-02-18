!>@file   mesh_send_recv_check.f90
!!@brief  module mesh_send_recv_check
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief test routine for mesh communication
!!
!!@verbatim
!!      subroutine node_send_recv_test                                  &
!!     &         (node, nod_comm, nod_check, v_sol)
!!      subroutine ele_send_recv_test                                   &
!!     &         (node, ele, ele_comm, ele_check, v_sol)
!!      subroutine surf_send_recv_test                                  &
!!     &         (node, surf, surf_comm, surf_check, v_sol)
!!      subroutine edge_send_recv_test                                  &
!!     &         (node, edge, edge_comm, edge_check, v_sol)
!!        type(node_data), intent(in) :: node
!!        type(communication_table), intent(in) :: nod_comm
!!        type(work_for_comm_check), intent(inout) :: nod_check
!!        type(element_data), intent(in) :: ele
!!        type(communication_table), intent(in) :: ele_comm
!!        type(work_for_comm_check), intent(inout) :: ele_check
!!        type(surface_data), intent(in) :: surf
!!        type(communication_table), intent(in) :: surf_comm
!!        type(work_for_comm_check), intent(inout) :: surf_check
!!        type(edge_data), intent(in) :: edge
!!        type(communication_table), intent(in) :: edge_comm
!!        type(work_for_comm_check), intent(inout) :: edge_check
!!
!!      subroutine node_transfer_test                                   &
!!     &         (node, new_node, new_comm, trans_tbl, nod_check, v_sol)
!!        type(node_data), intent(in) :: node, new_node
!!        type(communication_table), intent(in) :: new_comm
!!        type(calypso_comm_table), intent(in) :: trans_tbl
!!        type(work_for_comm_check), intent(inout) :: nod_check
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!
!!      subroutine collect_failed_comm(wk_check, SR_sig)
!!        type(work_for_comm_check), intent(inout) :: wk_check
!!        type(send_recv_status), intent(inout) :: SR_sig
!!@endverbatim
!
      module mesh_send_recv_check
!
      use m_work_time
      use m_precision
      use m_constants
!
      use calypso_mpi
!
      use t_solver_SR
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_work_for_comm_check
      use t_vector_for_solver
      use t_calypso_comm_table
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine node_send_recv_test                                    &
     &         (node, nod_comm, nod_check, v_sol)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(work_for_comm_check), intent(inout) :: nod_check
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call alloc_geom_4_comm_test(node%numnod, nod_check)
      call set_node_4_comm_test                                         &
     &   (node%numnod, node%internal_node, node%inod_global, node%xx,   &
     &    nod_check%i_gl_test, nod_check%xx_test)
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (node%numnod, nod_comm, nod_check%i_gl_test)
      call nod_vector_send_recv(node%numnod, nod_comm,                  &
     &                          nod_check%xx_test, v_sol)
!
      if (iflag_debug.gt.0) write(*,*) 'count_diff_node_comm_test'
      nod_check%num_diff                                                &
     &   = count_node_comm_test(node%numnod, node%inod_global, node%xx, &
     &                          nod_check%i_gl_test, nod_check%xx_test)
!
      call alloc_diff_ele_comm_test(nod_check)
      call compare_nod_comm_test                                        &
     &   (node%numnod, node%inod_global, node%xx,                       &
     &    nod_check%i_gl_test, nod_check%xx_test, nod_check%num_diff,   &
     &    nod_check%i_diff, nod_check%x_diff)
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test                                     &
     &         (node, ele, ele_comm, ele_check, v_sol)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(communication_table), intent(in) :: ele_comm
      type(work_for_comm_check), intent(inout) :: ele_check
      type(vectors_4_solver), intent(inout) :: v_sol
!
      call alloc_geom_4_comm_test(ele%numele, ele_check)
      call set_element_4_comm_test                                      &
     &   (node%internal_node, ele%numele, ele%ie(1,1), ele%iele_global, &
     &    ele%x_ele, ele_check%i_gl_test, ele_check%xx_test)
      call SOLVER_SEND_RECV_int8_type(ele%numele, ele_comm,             &
     &                                ele_check%i_gl_test)
      call nod_vector_send_recv(ele%numele, ele_comm,                   &
     &                          ele_check%xx_test, v_sol)
!
      ele_check%num_diff = count_ele_comm_test                          &
     &                   (ele%numele, ele%x_ele, ele_comm%ntot_import,  &
     &                    ele_comm%item_import, ele_check%xx_test)
      call alloc_diff_ele_comm_test(ele_check)
      call compare_ele_comm_test(ele%numele, ele%x_ele,                 &
     &    ele_comm%ntot_import, ele_comm%item_import,                   &
     &    ele_check%xx_test, ele_check%num_diff,                        &
     &    ele_check%i_diff, ele_check%x_diff)
      call dealloc_ele_4_comm_test(ele_check)
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine surf_send_recv_test                                    &
     &         (node, surf, surf_comm, surf_check, v_sol)
!
      use nod_phys_send_recv
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(surface_data), intent(in) :: surf
      type(communication_table), intent(in) :: surf_comm
      type(work_for_comm_check), intent(inout) :: surf_check
      type(vectors_4_solver), intent(inout) :: v_sol
!
      call alloc_geom_4_comm_test(surf%numsurf, surf_check)
      call set_element_4_comm_test                                      &
     &   (node%internal_node, surf%numsurf, surf%ie_surf(1,1),          &
     &    surf%isurf_global, surf%x_surf,                               &
     &    surf_check%i_gl_test, surf_check%xx_test)
      call SOLVER_SEND_RECV_int8_type(surf%numsurf, surf_comm,          &
     &                                surf_check%i_gl_test)
      call nod_vector_send_recv(surf%numsurf, surf_comm,                &
     &                          surf_check%xx_test, v_sol)
!
      surf_check%num_diff =  count_ele_comm_test                        &
     &               (surf%numsurf, surf%x_surf, surf_comm%ntot_import, &
     &                surf_comm%item_import, surf_check%xx_test)
      call alloc_diff_ele_comm_test(surf_check)
      call compare_ele_comm_test(surf%numsurf, surf%x_surf,             &
     &    surf_comm%ntot_import, surf_comm%item_import,                 &
     &    surf_check%xx_test, surf_check%num_diff,                      &
     &    surf_check%i_diff, surf_check%x_diff)
      call dealloc_ele_4_comm_test(surf_check)
!
      end subroutine surf_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine edge_send_recv_test                                    &
     &         (node, edge, edge_comm, edge_check, v_sol)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: edge_comm
      type(work_for_comm_check), intent(inout) :: edge_check
      type(vectors_4_solver), intent(inout) :: v_sol
!
      call alloc_geom_4_comm_test(edge%numedge, edge_check)
      call set_element_4_comm_test                                      &
     &   (node%internal_node, edge%numedge ,edge%ie_edge(1,1),          &
     &    edge%iedge_global, edge%x_edge,                               &
     &    edge_check%i_gl_test, edge_check%xx_test)
      call SOLVER_SEND_RECV_int8_type(edge%numedge, edge_comm,          &
     &                                edge_check%i_gl_test)
      call nod_vector_send_recv(edge%numedge, edge_comm,                &
     &                          edge_check%xx_test, v_sol)
!
      edge_check%num_diff =  count_ele_comm_test                        &
     &               (edge%numedge, edge%x_edge, edge_comm%ntot_import, &
     &                edge_comm%item_import, edge_check%xx_test)
      call alloc_diff_ele_comm_test(edge_check)
      call compare_ele_comm_test(edge%numedge, edge%x_edge,             &
     &    edge_comm%ntot_import, edge_comm%item_import,                 &
     &    edge_check%xx_test, edge_check%num_diff,                      &
     &    edge_check%i_diff, edge_check%x_diff)
      call dealloc_ele_4_comm_test(edge_check)
!
      end subroutine edge_send_recv_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine node_transfer_test                                     &
     &         (node, new_node, new_comm, trans_tbl, nod_check, v_sol)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
      use calypso_SR_type
      use select_copy_from_recv
!
      type(node_data), intent(in) :: node, new_node
      type(communication_table), intent(in) :: new_comm
      type(calypso_comm_table), intent(in) :: trans_tbl
      type(work_for_comm_check), intent(inout) :: nod_check
      type(vectors_4_solver), intent(inout) :: v_sol
!
!
      call alloc_geom_4_comm_test(new_node%numnod, nod_check)
      call calypso_SR_type_int8(iflag_import_item, trans_tbl,           &
     &    node%numnod, new_node%numnod,                                 &
     &    node%inod_global(1), nod_check%i_gl_test)
      call calypso_SR_type_3(iflag_import_item, trans_tbl,              &
     &    node%numnod, new_node%numnod,                                 &
     &    node%xx, nod_check%xx_test)
!
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (new_node%numnod, new_comm, nod_check%i_gl_test)
      call nod_vector_send_recv(new_node%numnod, new_comm,              &
     &                          nod_check%xx_test, v_sol)
!
      if (iflag_debug.gt.0) write(*,*) 'count_diff_node_comm_test'
      nod_check%num_diff                                                &
     &   = count_node_comm_test                                         &
     &             (new_node%numnod, new_node%inod_global, new_node%xx, &
     &              nod_check%i_gl_test, nod_check%xx_test)
!
      call alloc_diff_ele_comm_test(nod_check)
      call compare_nod_comm_test                                        &
     &   (new_node%numnod, new_node%inod_global, new_node%xx,           &
     &    nod_check%i_gl_test, nod_check%xx_test, nod_check%num_diff,   &
     &    nod_check%i_diff, nod_check%x_diff)
!
      end subroutine node_transfer_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine collect_failed_comm(wk_check, SR_sig)
!
      use diff_geometory_comm_test
      use nod_phys_send_recv
      use solver_SR_type
      use collect_SR_int
      use collect_SR_N
!
      type(work_for_comm_check), intent(inout) :: wk_check
      type(send_recv_status), intent(inout) :: SR_sig
!
!
      write(*,*) my_rank, ' wk_check%num_diff', wk_check%num_diff
      call alloc_comm_stack_ctest_IO(wk_check)
      call count_collect_SR_num                                         &
     &   (wk_check%num_diff,  wk_check%istack_diff_pe,  SR_sig)
!
      call alloc_ele_comm_test_IO(wk_check)
      call collect_send_recv_int                                        &
     &   (0, wk_check%num_diff, wk_check%i_diff,                        &
     &    wk_check%istack_diff_pe, wk_check%i_diff_IO, SR_sig)
      call collect_send_recv_N                                          &
     &   (0, isix, wk_check%num_diff, wk_check%x_diff,                  &
     &    wk_check%istack_diff_pe, wk_check%x_diff_IO, SR_sig)
      call dealloc_diff_ele_comm_test(wk_check)
!
      end subroutine collect_failed_comm
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_check
