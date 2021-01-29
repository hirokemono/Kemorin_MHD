!
!     module   m_2nd_pallalel_vector
!.......................................................................
!
!     Writen by H. Matsui on Aug., 2006
!
      module   m_2nd_pallalel_vector
!
      use m_precision
!
      implicit  none
!
      integer :: nprocs_2nd
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_num_processes_to_2nd
!
      use calypso_mpi
!
!
      nprocs_2nd = nprocs
!
      end subroutine copy_num_processes_to_2nd
!
!  ---------------------------------------------------------------------
!
      end module   m_2nd_pallalel_vector

