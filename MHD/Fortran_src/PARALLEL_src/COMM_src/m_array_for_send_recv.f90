!>@file   m_array_for_send_recv.f90
!!@brief      module m_array_for_send_recv
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n    Modified on Apr., 2008
!!@n    Modified on Dec., 2012
!!@n    Modified on sep., 2013
!
!>@brief Work array for data communication of FEM data
!!
!!@verbatim
!!      subroutine verify_vector_for_solver(NB, NP)
!!      subroutine allocate_vector_for_solver(NB, NP)
!!      subroutine deallocate_vector_for_solver
!!@endverbatim
!!
!!@param  N    length of vector
!!@param  NB   number of vectors to solve
!
      module   m_array_for_send_recv
!
      use m_precision
      use calypso_mpi
!
      implicit  none
!
!
!>      Vector for solution vector
      real(kind=kreal), allocatable :: x_vec(:)
!>      Vector for right hand side vector
      real(kind=kreal), allocatable :: b_vec(:)
!>      Size of allocated vectors
      integer(kind = kint) :: isize_solver_vect = -1
!
!
      private :: isize_solver_vect
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
       subroutine verify_vector_for_solver(NB, NP)
!
       integer(kind = kint), intent(in) :: NB, NP
       integer(kind = kint) :: ncomp
!
!
       ncomp = NB*NP
       if (isize_solver_vect .lt. 0) then
         call allocate_vector_for_solver(NB,NP)
       else
         if (isize_solver_vect .lt. ncomp) then
           call deallocate_vector_for_solver
           call allocate_vector_for_solver(NB,NP)
         end if
       end if
!
       end subroutine verify_vector_for_solver
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_vector_for_solver(NB, NP)
!
       integer(kind = kint), intent(in) :: NB, NP
!
!
       if(allocated(x_vec)) return
!
       allocate(x_vec(NB*NP))
       allocate(b_vec(NB*NP))
       isize_solver_vect = NB*NP
!
       if(NP*NB .gt. 0) then
         b_vec  = 0.0d00
         x_vec  =0.0d00
       end if
!
       end subroutine allocate_vector_for_solver
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_vector_for_solver
!
!
       deallocate(x_vec, b_vec)
       isize_solver_vect = 0
!
       end subroutine deallocate_vector_for_solver
!
!  ---------------------------------------------------------------------
!
      end module   m_array_for_send_recv
