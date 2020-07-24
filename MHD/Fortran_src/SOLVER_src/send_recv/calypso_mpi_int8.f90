!>@file   calypso_mpi_int8.f90
!!@brief  module calypso_mpi_int8
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI communication routines for 8-byte integer in Calypso
!!
!!@verbatim
!!      subroutine calypso_mpi_bcast_int8(buffer, count, root)
!!        integer, intent(in) :: root
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint_gl), intent(inout) :: buffer(count)
!!      subroutine calypso_mpi_allreduce_int8                           &
!!     &         (r_local, r_global, count, operation)
!!        integer, intent(in) :: operation
!!        integer(kind = kint_gl), intent(in) :: count
!!        integer(kind = kint_gl), intent(in) ::    r_local(count)
!!        integer(kind = kint_gl), intent(inout) :: r_global(count)
!!      subroutine calypso_mpi_allgather_int8                           &
!!     &         (i8sendbuf, n_send, i8recvbuf, n_recv)
!!        integer(kind = kint), intent(in) :: n_send, n_recv
!!        integer(kind = kint_gl), intent(in) ::    i8sendbuf(n_send)
!!        integer(kind = kint_gl), intent(inout)                        &
!!     &                        :: i8recvbuf(nprocs*n_recv)
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi_int8
!
      use m_precision
      use m_constants
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
      subroutine calypso_mpi_bcast_int8(buffer, count, root)
!
      integer, intent(in) :: root
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(inout) :: buffer(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_BCAST(buffer(ist+1), ilen_in, CALYPSO_GLOBAL_INT,      &
     &      root, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_bcast_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allreduce_int8                             &
     &         (r_local, r_global, count, operation)
!
      integer, intent(in) :: operation
      integer(kind = kint_gl), intent(in) :: count
      integer(kind = kint_gl), intent(in) ::    r_local(count)
      integer(kind = kint_gl), intent(inout) :: r_global(count)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      ist = 0
      do
        ilen_in = int(min(count-ist, huge_20))
        call MPI_allREDUCE(r_local(ist+1), r_global(ist+1), ilen_in,    &
     &      CALYPSO_GLOBAL_INT, operation, CALYPSO_COMM, ierr_MPI)
        ist = ist + ilen_in
        if(ist .ge. count) exit
      end do
!
      end subroutine calypso_mpi_allreduce_int8
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_mpi_allgather_int8                             &
     &         (i8sendbuf, n_send, i8recvbuf, n_recv)
!
      integer(kind = kint), intent(in) :: n_send, n_recv
      integer(kind = kint_gl), intent(in) ::    i8sendbuf(n_send)
      integer(kind = kint_gl), intent(inout)                            &
     &                        :: i8recvbuf(nprocs*n_recv)
!
      integer(kind = kint_gl) :: ist
      integer :: ilen_in
!
!
      call MPI_AllGather(i8sendbuf, int(n_send), CALYPSO_GLOBAL_INT,    &
     &    i8recvbuf, int(n_recv), CALYPSO_GLOBAL_INT, CALYPSO_COMM,     &
     &    ierr_MPI)
!
      end subroutine calypso_mpi_allgather_int8
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi_int8
