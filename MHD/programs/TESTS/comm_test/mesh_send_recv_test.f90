!>@file   mesh_send_recv_test.f90
!!@brief  module mesh_send_recv_test
!!
!!@author H. Matsui
!!@date Programmed in April, 2013
!
!>@brief test routine for mesh communication
!!
!!@verbatim
!!      subroutine elpsed_label_4_comm_test
!!
!!      subroutine s_mesh_send_recv_test
!!      subroutine node_send_recv4_test(node, nod_comm, N12, v_sol)
!!
!!      subroutine node_send_recv_test(node, nod_comm)
!!@endverbatim
!
      module mesh_send_recv_test
!
      use m_work_time
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_geometry_4_comm_test
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_vector_for_solver
!
      implicit  none
!
      logical, save, private :: iflag_elapsd = .FALSE.
      integer(kind = kint), save, private :: ist_elapsed =   0
      integer(kind = kint), save, private :: ied_elapsed =   0
!
      private :: ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine elpsed_label_4_comm_test
!
      integer(kind = kint), parameter :: num_append = 8
!
!
      call append_elapsed_times                                         &
     &   (num_append, ist_elapsed, ied_elapsed)
!
      write(elps1%labels(ist_elapsed+1),'(a)')                          &
     &                             'copy_from_recv_by_comm_table1'
      write(elps1%labels(ist_elapsed+2),'(a)')                          &
     &                             'copy_from_recv_by_rev_table1'
      write(elps1%labels(ist_elapsed+3),'(a)')                          &
     &                             'copy_from_recv_by_comm_lgloop1'
      write(elps1%labels(ist_elapsed+4),'(a)')                          &
     &                             'copy_from_recv_by_rev_lgloop1'
!
      write(elps1%labels(ist_elapsed+5),'(a)')                          &
     &                             'copy_to_send_by_comm_table2'
      write(elps1%labels(ist_elapsed+6),'(a)')                          &
     &                             'copy_from_recv_by_rev_table2'
      write(elps1%labels(ist_elapsed+7),'(a)')                          &
     &                             'copy_from_recv_by_comm_lgloop2'
      write(elps1%labels(ist_elapsed+8),'(a)')                          &
     &                             'copy_from_recv_by_rev_lgloop2'
!
      iflag_elapsd = .TRUE.
!
      end subroutine elpsed_label_4_comm_test
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine s_mesh_send_recv_test(ele, surf, edge,                 &
     &          ele_comm, surf_comm, edge_comm, v_sol)
!
      use t_vector_for_solver
      use nod_phys_send_recv
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(edge_data), intent(in) :: edge
      type(communication_table), intent(in) :: ele_comm
      type(communication_table), intent(in) :: surf_comm
      type(communication_table), intent(in) :: edge_comm
      type(vectors_4_solver), intent(inout) :: v_sol
!
      call ele_send_recv_test(ele%numele, ele%x_ele,                    &
     &                        ele_comm, ele_check)
      call nod_vector_send_recv(ele%numele, ele_comm,                   &
     &                          ele_check%xx_test, v_sol)
!
      call ele_send_recv_test(surf%numsurf, surf%x_surf,                &
     &                        surf_comm, surf_check)
      call nod_vector_send_recv(surf%numsurf, surf_comm,                &
     &                          surf_check%xx_test, v_sol)
!
      call ele_send_recv_test(edge%numedge, edge%x_edge,                &
     &                        edge_comm, edge_check)
      call nod_vector_send_recv(edge%numedge, edge_comm,                &
     &                          edge_check%xx_test, v_sol)
!
      end subroutine s_mesh_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv_test(node, nod_comm, v_sol)
!
      use solver_SR_type
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, node%internal_node
        v_sol%i8x_vec(inod) =   node%inod_global(inod)
        v_sol%x_vec(3*inod-2) = node%xx(inod,1)
        v_sol%x_vec(3*inod-1) = node%xx(inod,2)
        v_sol%x_vec(3*inod  ) = node%xx(inod,3)
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (node%numnod, nod_comm, v_sol%i8x_vec(1))
!
      call SOLVER_SEND_RECV_3_type(node%numnod, nod_comm,               &
     &                             v_sol%x_vec(1))
!
      end subroutine node_send_recv_test
!
! ----------------------------------------------------------------------
!
      subroutine node_send_recv4_test(node, nod_comm, N12, v_sol)
!
      use m_solver_SR
      use solver_SR_type
      use send_recv_loop_tests
!
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
!
      integer(kind = kint), intent(in) :: N12
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint), allocatable :: irev_import(:)
!
      integer(kind = kint) :: inod, k, ii
!
      allocate(irev_import(nod_comm%istack_import(nod_comm%num_neib)))
!
!$omp parallel do
      do inod = 1, node%internal_node
        v_sol%i8x_vec(inod) = node%inod_global(inod)
        v_sol%x_vec(12*inod-11) = node%xx(inod,1)
        v_sol%x_vec(12*inod-10) = node%xx(inod,2)
        v_sol%x_vec(12*inod- 9) = node%xx(inod,3)
        v_sol%x_vec(12*inod- 8) = node%xx(inod,1) + 100.0
        v_sol%x_vec(12*inod- 7) = node%xx(inod,2) + 100.0
        v_sol%x_vec(12*inod- 6) = node%xx(inod,3) + 100.0
        v_sol%x_vec(12*inod- 5) = node%xx(inod,1) + 200.0
        v_sol%x_vec(12*inod- 4) = node%xx(inod,2) + 200.0
        v_sol%x_vec(12*inod- 3) = node%xx(inod,3) + 200.0
        v_sol%x_vec(12*inod- 2) = node%xx(inod,1) + 300.0
        v_sol%x_vec(12*inod- 1) = node%xx(inod,2) + 300.0
        v_sol%x_vec(12*inod   ) = node%xx(inod,3) + 300.0
      end do
!$omp end parallel do
!
      call SOLVER_SEND_RECV_int8_type                                   &
     &   (node%numnod, nod_comm, v_sol%i8x_vec(1))
!
      call SOLVER_SEND_RECV_N_type                                      &
     &   (node%numnod, N12, nod_comm, v_sol%x_vec(1))
!
      do ii = 1, nod_comm%istack_import(nod_comm%num_neib)
        k = nod_comm%item_import(ii) - node%internal_node
        irev_import(k) = ii
      end do
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+1)
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+2)
      call copy_to_send_by_comm_table1(node, nod_comm,                  &
     &                                 N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+2)

      call copy_from_recv_by_comm_table1                                &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+1)
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+2)
      call copy_from_recv_by_rev_table1                                 &
     &   (node, nod_comm, N12, irev_import, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+2)
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+3)
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+4)
      call copy_to_send_by_comm_lgloop1                                 &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+4)

      call copy_from_recv_by_comm_lgloop1                               &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+3)
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+4)
      call copy_from_recv_by_rev_lgloop1                                &
     &   (node, nod_comm, N12, irev_import, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+4)
!
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+5)
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+6)
      call copy_to_send_by_comm_table2(node, nod_comm,                  &
     &                                 N12, v_sol%x_vec)
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+6)

      call copy_from_recv_by_comm_table2                                &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+5)
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+6)
      call copy_from_recv_by_rev_table2                                 &
     &   (node, nod_comm, N12, irev_import, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+6)
!
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+7)
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+8)
      call copy_to_send_by_comm_lgloop2                                 &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+8)

      call copy_from_recv_by_comm_lgloop2                               &
     &   (node, nod_comm, N12, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+7)
!
      if(iflag_elapsd) call start_elapsed_time(ist_elapsed+8)
      call copy_from_recv_by_rev_lgloop2                                &
     &   (node, nod_comm, N12, irev_import, v_sol%x_vec(1))
      if(iflag_elapsd) call end_elapsed_time(ist_elapsed+8)
!
      end subroutine node_send_recv4_test
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine ele_send_recv_test(numele, x_ele, ele_comm, wk_check)
!
      integer(kind = kint), intent(in) :: numele
      real(kind = kreal), intent(in) :: x_ele(numele,3)
      type(communication_table), intent(in) :: ele_comm
!
      type(work_for_comm_check), intent(inout) :: wk_check
!
      integer(kind = kint) :: iele, inum
!
!
      do iele = 1, numele
        wk_check%xx_test(iele,1) = x_ele(iele,1)
        wk_check%xx_test(iele,2) = x_ele(iele,2)
        wk_check%xx_test(iele,3) = x_ele(iele,3)
      end do
      do inum = 1, ele_comm%ntot_import
        iele = ele_comm%item_import(inum)
        wk_check%xx_test(iele,1) = 0.0d0
        wk_check%xx_test(iele,2) = 0.0d0
        wk_check%xx_test(iele,3) = 0.0d0
      end do
!
      end subroutine ele_send_recv_test
!
! ----------------------------------------------------------------------
!
      end module mesh_send_recv_test
