!
!      module send_recv_3d_filtering
!
!      Written by H. Matsui on July, 2005
!      Written by H. Matsui on Apr., 2008
!
!!      subroutine scalar_send_recv_3d_filter                           &
!!     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!!      subroutine vector_send_recv_3d_filter                           &
!!     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!!      subroutine tensor_send_recv_3d_filter                           &
!!     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!
      module send_recv_3d_filtering
!
      use m_precision
      use m_work_time
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine scalar_send_recv_3d_filter                             &
     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!
      use m_nod_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
      real(kind = kreal), intent(inout) :: x_vec(nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod, ncomp_nod)
!
      integer(kind = kint) :: inod
!
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_type(nnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod,i_fld) = x_vec(inod  )
      end do
!$omp end parallel do
!
      end subroutine scalar_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine vector_send_recv_3d_filter                             &
     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!
      use m_nod_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
      real(kind = kreal), intent(inout) :: x_vec(3*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
!
      integer(kind = kint) :: inod
!
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_3_type(nnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod=1, nnod
        d_nod(inod,i_fld  ) = x_vec(3*inod-2)
        d_nod(inod,i_fld+1) = x_vec(3*inod-1)
        d_nod(inod,i_fld+2) = x_vec(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      subroutine tensor_send_recv_3d_filter                             &
     &         (nnod, x_vec, ncomp_nod, i_fld, d_nod)
!
      use m_nod_comm_table
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: nnod, ncomp_nod, i_fld
      real(kind = kreal), intent(inout) :: x_vec(6*nnod)
      real(kind = kreal), intent(inout) :: d_nod(nnod,ncomp_nod)
!
      integer(kind = kint) :: inod
!
!
      START_SRtime= MPI_WTIME()
      call SOLVER_SEND_RECV_6_type(nnod, nod_comm, x_vec(1))
      SendRecvtime = MPI_WTIME() - START_SRtime + SendRecvtime
!
!$omp parallel do
      do inod =1, nnod
        d_nod(inod,i_fld  ) = x_vec(6*inod-5)
        d_nod(inod,i_fld+1) = x_vec(6*inod-4)
        d_nod(inod,i_fld+2) = x_vec(6*inod-3)
        d_nod(inod,i_fld+3) = x_vec(6*inod-2)
        d_nod(inod,i_fld+4) = x_vec(6*inod-1)
        d_nod(inod,i_fld+5) = x_vec(6*inod  )
      end do
!$omp end parallel do
!
      end subroutine tensor_send_recv_3d_filter
!
! ----------------------------------------------------------------------
!
      end module send_recv_3d_filtering
