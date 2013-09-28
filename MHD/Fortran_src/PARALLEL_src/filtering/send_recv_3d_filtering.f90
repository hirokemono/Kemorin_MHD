!
!      module send_recv_3d_filtering
!
!      Written by H. Matsui on July, 2005
!      Written by H. Matsui on Apr., 2008
!
!      subroutine scalar_send_recv_3d_filter(nnod, x_vec, d_nod)
!      subroutine vector_send_recv_3d_filter(nnod, x_vec, d_nod)
!      subroutine tensor_send_recv_3d_filter(nnod, x_vec, d_nod)
!
      module send_recv_3d_filtering
!
      use m_precision
      use m_parallel_var_dof
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv_3d_filter(nnod, x_vec, d_nod)
!
      use m_nod_comm_table
      use solver_SR
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod)
!
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV(nnod, num_neib, id_neib,                    &
     &                      istack_import, item_import,                 &
     &                      istack_export, item_export, x_vec(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod) = x_vec(inod  )
      end do
!$omp end parallel do
!
      end subroutine scalar_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv_3d_filter(nnod, x_vec, d_nod)
!
      use m_nod_comm_table
      use solver_SR_3
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(3*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,3)
!
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_3 (nnod, num_neib, id_neib,                 &
     &                         istack_import, item_import,              &
     &                         istack_export, item_export, x_vec(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod,1) = x_vec(3*inod-2)
        d_nod(inod,2) = x_vec(3*inod-1)
        d_nod(inod,3) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine tensor_send_recv_3d_filter(nnod, x_vec, d_nod)
!
      use m_nod_comm_table
      use solver_SR_6
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(inout) :: x_vec(6*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,6)
!
      integer(kind = kint) :: inod
!
!
      START_TIME= MPI_WTIME()
      call SOLVER_SEND_RECV_6(nnod, num_neib, id_neib,                  &
     &       istack_import, item_import, istack_export, item_export,    &
     &       x_vec(1) )
      END_TIME= MPI_WTIME()
      COMMtime = COMMtime + END_TIME - START_TIME
!
!$omp parallel do
      do inod =1, nnod
        d_nod(inod,1) = x_vec(6*inod-5)
        d_nod(inod,2) = x_vec(6*inod-4)
        d_nod(inod,3) = x_vec(6*inod-3)
        d_nod(inod,4) = x_vec(6*inod-2)
        d_nod(inod,5) = x_vec(6*inod-1)
        d_nod(inod,6) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine tensor_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      end module send_recv_3d_filtering
