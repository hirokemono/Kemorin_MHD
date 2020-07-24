!>@file   calypso_mpi.f90
!!@brief  module calypso_mpi
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI wrapper for Calypso
!!
!!@verbatim
!!      subroutine calypso_MPI_init
!!      subroutine calypso_MPI_finalize
!!      subroutine calypso_MPI_abort(icode, message)
!!
!!      subroutine calypso_MPI_barrier
!!@endverbatim
!!
!!@n @param  icode       error code
!!@n @param  message    message to output
!
      module calypso_mpi
!
!      use mpi
      use m_precision
      use m_constants
!
      implicit none
!
      include 'mpif.h'
!
!>     MPI communicator
      integer :: CALYPSO_COMM
!
!>     integer size for MPI
      integer :: CALYPSO_INTEGER
!>     real size for MPI
      integer :: CALYPSO_REAL
!>     character size for MPI
      integer :: CALYPSO_CHARACTER
!
!>     integer size for MPI
      integer :: CALYPSO_GLOBAL_INT
!>     4-byte integer size for MPI
      integer :: CALYPSO_FOUR_INT
!
!>     logical size for MPI
      integer :: CALYPSO_LOGICAL
!
!>      process ID (start from 0)
      integer :: my_rank
!>      total number of processes
      integer :: nprocs
!
!>      error flag for MPI
      integer :: ierr_MPI
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine calypso_MPI_init
!
!
      call  MPI_INIT(ierr_MPI)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, CALYPSO_COMM, ierr_MPI)
      call  MPI_COMM_SIZE(CALYPSO_COMM, nprocs, ierr_MPI)
      call  MPI_COMM_RANK(CALYPSO_COMM, my_rank, ierr_MPI)
!
      CALYPSO_CHARACTER = MPI_CHARACTER
      CALYPSO_FOUR_INT =  MPI_INTEGER 
      CALYPSO_LOGICAL =   MPI_LOGICAL
!
      if(kint .eq. 4) then
        CALYPSO_INTEGER = MPI_INTEGER
      else if(kint .eq. 8) then
        CALYPSO_INTEGER = MPI_INTEGER8
      else if(kint .eq. 2) then
        CALYPSO_INTEGER = MPI_INTEGER2
      else if(kint .eq. 1) then
        CALYPSO_INTEGER = MPI_INTEGER1
      else
        CALYPSO_INTEGER = MPI_INTEGER
      end if
!
      if(kreal .eq. 8) then
        CALYPSO_REAL = MPI_DOUBLE_PRECISION
      else if(kint .eq. 4) then
        CALYPSO_REAL = MPI_REAL
      else if(kint .eq. 16) then
        CALYPSO_REAL = MPI_REAL16
      else
        CALYPSO_REAL = MPI_DOUBLE_PRECISION
      end if
!
      if(kint_gl .eq. 4) then
        CALYPSO_GLOBAL_INT = MPI_INTEGER
      else if(kint_gl .eq. 8) then
        CALYPSO_GLOBAL_INT = MPI_INTEGER8
      else
        CALYPSO_GLOBAL_INT = MPI_INTEGER
      end if
!
      end subroutine calypso_MPI_init
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_finalize
!
!
      call  MPI_FINALIZE(ierr_MPI)
!
      end subroutine calypso_MPI_finalize
!
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_abort(icode, message)
!
      integer(kind = kint), intent(in)  ::  icode
      character(len=*), intent(in)  ::  message
!
!
      write(*,*) ' ///// abnormal termination ///// ', icode,           &
     &                                            ' ', message
!
      call  MPI_ABORT(CALYPSO_COMM, 999, ierr_MPI)
!
      stop
      end subroutine calypso_MPI_abort
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine calypso_MPI_barrier
!
!
      call MPI_BARRIER(CALYPSO_COMM, ierr_MPI)
!
      end subroutine  calypso_MPI_barrier
!
!  ---------------------------------------------------------------------
!
      end module calypso_mpi
