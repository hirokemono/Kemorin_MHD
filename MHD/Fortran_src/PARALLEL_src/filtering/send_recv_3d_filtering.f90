!
!      module send_recv_3d_filtering
!
!      Written by H. Matsui on July, 2005
!      Written by H. Matsui on Apr., 2008
!
!      subroutine scalar_send_recv_3d_filter(i_filter)
!      subroutine vector_send_recv_3d_filter(i_filter)
!      subroutine tensor_send_recv_3d_filter(i_filter)
!
      module send_recv_3d_filtering
!
      use m_precision
!
      use m_parallel_var_dof
      use m_nod_comm_table
      use m_geometry_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv_3d_filter(i_filter)
!
      use m_node_phys_data
      use solver_SR
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV(numnod, num_neib, id_neib,                  &
     &                      istack_import, item_import,                 &
     &                      istack_export, item_export,                 &
     &                      x_vec(1),SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, numnod
        d_nod(inod,i_filter) = x_vec(inod)
      end do
!cdir end parallel do
!
      end subroutine scalar_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv_3d_filter(i_filter)
!
      use m_node_phys_data
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3 (numnod, num_neib, id_neib,               &
     &                         istack_import, item_import,              &
     &                         istack_export, item_export,              &
     &                         x_vec(1), SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, numnod
        d_nod(inod,i_filter  ) = x_vec(3*inod-2)
        d_nod(inod,i_filter+1) = x_vec(3*inod-1)
        d_nod(inod,i_filter+2) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine tensor_send_recv_3d_filter(i_filter)
!
      use m_node_phys_data
      use solver_SR_6
!
      integer(kind = kint), intent(in) :: i_filter
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_6                                           &
     &      (numnod, num_neib, id_neib,                                 &
     &       istack_import, item_import,                                &
     &       istack_export, item_export,                                &
     &       x_vec(1), SOLVER_COMM, my_rank )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod =1, numnod
        d_nod(inod,i_filter  ) = x_vec(6*inod-5)
        d_nod(inod,i_filter+1) = x_vec(6*inod-4)
        d_nod(inod,i_filter+2) = x_vec(6*inod-3)
        d_nod(inod,i_filter+3) = x_vec(6*inod-2)
        d_nod(inod,i_filter+4) = x_vec(6*inod-1)
        d_nod(inod,i_filter+5) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine tensor_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      end module send_recv_3d_filtering
