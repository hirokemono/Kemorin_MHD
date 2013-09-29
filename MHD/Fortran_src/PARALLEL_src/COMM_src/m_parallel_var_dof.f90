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
!
      end subroutine parallel_cal_init
!
!  ---------------------------------------------------------------------
!
      end module   m_parallel_var_dof
