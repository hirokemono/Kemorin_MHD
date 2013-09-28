!>@file   m_parallel_var_dof.f90
!!@brief      module m_parallel_var_dof
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n    Modified on Apr., 2008
!!@n    Modified on Dec., 2012
!
!> @brief  Basic parameters for MPI parallelization
!!
!!@verbatim
!!      subroutine parallel_cal_init
!!@endverbatim
!
      module   m_parallel_var_dof
!
      use m_precision
      use calypso_mpi
!
!
      implicit  none
!
!>      MPI communicator for CALYPSO
      integer(kind=kint) :: SOLVER_COMM
! 
!>      process ID (start from 0)
      integer(kind=kint) :: my_rank
!>      error flag
      integer(kind=kint) :: ierr
!
      real(kind=kreal) :: START_TIME, END_TIME, COMMtime
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine parallel_cal_init
!
!
      call calypso_MPI_init
      call  MPI_COMM_DUP (MPI_COMM_WORLD, SOLVER_COMM, ierr)
      call  MPI_COMM_RANK(SOLVER_COMM, my_rank  , ierr)
!
      end subroutine parallel_cal_init
!
!  ---------------------------------------------------------------------
!
      end module   m_parallel_var_dof
