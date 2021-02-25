!> @file  transfer_to_new_partition.f90
!!      module transfer_to_new_partition
!!
!! @author  H. Matsui
!! @date Programmed in Jan., 2009
!
!> @brief Data communication to new partitioned mesh
!!
!!@verbatim
!!      subroutine init_vector_for_repart(NB, nnod_new)
!!      subroutine deallocate_vector_for_repart
!!
!!      subroutine scalar_to_new_partition                              &
!!     &         (iflag_recv, transfer_tbl, new_nod_comm,               &
!!     &          nnod_org, nnod_new, vec_org, vec_new, v_sol)
!!      subroutine vector_to_new_partition                              &
!!     &         (iflag_recv, transfer_tbl, new_nod_comm,               &
!!     &          nnod_org, nnod_new, vec_org, vec_new, v_sol)
!!      subroutine tensor_to_new_partition                              &
!!     &         (iflag_recv, transfer_tbl, new_nod_comm,               &
!!     &          NB, nnod_org, nnod_new, vec_org, vec_new, v_sol)
!!        type(calypso_comm_table), intent(in) :: transfer_tbl
!!        type(communication_table), intent(in) :: new_nod_comm
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!@endverbatim
!
      module transfer_to_new_partition
!
      use m_precision
      use m_phys_constants
      use calypso_mpi
      use t_comm_table
      use t_calypso_comm_table
      use t_vector_for_solver
!
      implicit  none
!
!>      Vector for solution vector
      real(kind=kreal), allocatable :: x_new(:)
!>      Size of allocated vectors
      integer(kind = kint) :: isize_repart_vect = -1
!
      private :: isize_repart_vect, x_new
      private :: verify_vector_for_repart, allocate_vector_for_repart
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_vector_for_repart(NB, nnod_new)
!
      integer(kind = kint), intent(in) :: NB, nnod_new
!
!
      call allocate_vector_for_repart(NB, nnod_new)
!
      end subroutine init_vector_for_repart
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_vector_for_repart
!
      deallocate(x_new)
      isize_repart_vect = 0
!
      end subroutine deallocate_vector_for_repart
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine scalar_to_new_partition                                &
     &         (iflag_recv, transfer_tbl, new_nod_comm,                 &
     &          nnod_org, nnod_new, vec_org, vec_new, v_sol)
!
      use calypso_SR_type
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(calypso_comm_table), intent(in) :: transfer_tbl
      type(communication_table), intent(in) :: new_nod_comm
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real(kind = kreal), intent(in) :: vec_org(nnod_org)
!
      type(vectors_4_solver), intent(inout) :: v_sol
      real(kind = kreal), intent(inout) :: vec_new(nnod_new)
!
!
      call verify_iccgN_vec_type(n_scalar, nnod_org, v_sol)
!
      call calypso_SR_type_1(iflag_recv, transfer_tbl,                  &
     &    nnod_org, nnod_new, vec_org(1), vec_new(1))
      call SOLVER_SEND_RECV_type(nnod_new, new_nod_comm, vec_new(1))
!
      end subroutine scalar_to_new_partition
!
! -----------------------------------------------------------------------
!
      subroutine vector_to_new_partition                                &
     &         (iflag_recv, transfer_tbl, new_nod_comm,                 &
     &          nnod_org, nnod_new, vec_org, vec_new, v_sol)
!
      use calypso_SR_type
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(calypso_comm_table), intent(in) :: transfer_tbl
      type(communication_table), intent(in) :: new_nod_comm
      integer(kind = kint), intent(in) :: nnod_org, nnod_new
      real(kind = kreal), intent(in) :: vec_org(nnod_org,3)
!
      real(kind = kreal), intent(inout) :: vec_new(nnod_new,3)
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: inod
!
!
      call verify_iccgN_vec_type(n_vector, nnod_org, v_sol)
      call verify_vector_for_repart(n_vector, nnod_new)
!
!$omp parallel do private(inod)
      do inod = 1, nnod_org
        v_sol%x_vec(3*inod-2) = vec_org(inod,1)
        v_sol%x_vec(3*inod-1) = vec_org(inod,2)
        v_sol%x_vec(3*inod  ) = vec_org(inod,3)
      end do
!$omp end parallel do
!
      call calypso_SR_type_3(iflag_recv, transfer_tbl,                  &
     &    nnod_org, nnod_new, v_sol%x_vec(1), x_new(1))
      call SOLVER_SEND_RECV_3_type(nnod_new, new_nod_comm, x_new(1))
!
!$omp parallel do private(inod)
      do inod = 1, nnod_new
        vec_new(inod,1) = x_new(3*inod-2)
        vec_new(inod,2) = x_new(3*inod-1)
        vec_new(inod,3) = x_new(3*inod  )
      end do
!$omp end parallel do
!
      end subroutine vector_to_new_partition
!
! -----------------------------------------------------------------------
!
      subroutine tensor_to_new_partition                                &
     &         (iflag_recv, transfer_tbl, new_nod_comm,                 &
     &          NB, nnod_org, nnod_new, vec_org, vec_new, v_sol)
!
      use calypso_SR_type
      use solver_SR_type
!
      integer(kind = kint), intent(in) :: iflag_recv
      type(calypso_comm_table), intent(in) :: transfer_tbl
      type(communication_table), intent(in) :: new_nod_comm
      integer(kind = kint), intent(in) :: NB, nnod_org, nnod_new
      real(kind = kreal), intent(in) :: vec_org(nnod_org,NB)
!
      real(kind = kreal), intent(inout) :: vec_new(nnod_new,NB)
      type(vectors_4_solver), intent(inout) :: v_sol
!
      integer(kind = kint) :: inod, nd
!
!
      call verify_iccgN_vec_type(NB, nnod_org, v_sol)
      call verify_vector_for_repart(NB, nnod_new)
!
!$omp parallel do private(inod,nd)
      do inod = 1, nnod_org
        do nd = 1, NB
          v_sol%x_vec(NB*inod-NB+nd) = vec_org(inod,nd)
        end do
      end do
!$omp end parallel do
!
      call calypso_SR_type_N(iflag_recv, NB, transfer_tbl,              &
     &    nnod_org, nnod_new, v_sol%x_vec(1), x_new(1))
      call SOLVER_SEND_RECV_N_type                                      &
     &   (nnod_new, NB, new_nod_comm, x_new(1))
!
!$omp parallel private(nd)
      do nd = 1, NB
!$omp do private(inod)
        do inod = 1, nnod_new
          vec_new(inod,nd) = x_new(NB*inod-NB+nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine tensor_to_new_partition
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine verify_vector_for_repart(NB, nnod_new)
!
      integer(kind = kint), intent(in) :: NB, nnod_new
!
!
      if (isize_repart_vect .lt. 0) then
        call allocate_vector_for_repart(NB,nnod_new)
      else
        if (isize_repart_vect .lt. (NB*nnod_new)) then
          call deallocate_vector_for_repart
          call allocate_vector_for_repart(NB,nnod_new)
        end if
      end if
!
      end subroutine verify_vector_for_repart
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_vector_for_repart(NB, nnod_new)
!
      integer(kind = kint), intent(in) :: NB, nnod_new
!
!
      if(allocated(x_new)) return
!
      allocate(x_new(NB*nnod_new))
      isize_repart_vect = NB*nnod_new
!
      if(nnod_new*NB .gt. 0) x_new = 0.0d00
!
      end subroutine allocate_vector_for_repart
!
!  ---------------------------------------------------------------------
!
      end module transfer_to_new_partition
