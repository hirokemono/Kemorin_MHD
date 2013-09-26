!>@file   calypso_mpi.f90
!!@brief  module calypso_mpi
!!
!!@author  H. Matsui
!!@date Programmed on Feb., 2012
!
!
!> @brief MPI wrapper
!
      module calypso_mpi
!
!      use mpi
      use m_precision
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
!
      end module calypso_mpi
!
