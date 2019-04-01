!
!     module   m_2nd_pallalel_vector
!.......................................................................
!
!     Writen by H. Matsui on Aug., 2006
!
!!       subroutine verify_2nd_iccg_matrix(NB, numnod)
!!       subroutine allocate_2nd_iccg_matrix(NB, numnod)
!!       subroutine deallocate_2nd_iccg_matrix
!!
!!       subroutine verify_2nd_iccg_int_mat(numnod)
!!       subroutine allocate_2nd_iccg_int_mat(numnod)
!!       subroutine deallocate_2nd_iccg_int_mat
!!
!!       subroutine verify_2nd_iccg_int8_mat(numnod)
!!       subroutine allocate_2nd_iccg_int8_mat(numnod)
!!       subroutine deallocate_2nd_iccg_int8_mat
!
      module   m_2nd_pallalel_vector
!
      use m_precision
!
      implicit  none
!
      integer :: nprocs_2nd
!     total pe count (1 to petot)
! 
!
      real(kind=kreal), allocatable :: xvec_2nd(:)
      real(kind=kreal), allocatable :: bb_2nd(:)
      integer(kind = kint) :: isize_solver_vect2 = -1
!
      integer(kind = kint), allocatable :: ivec_2nd(:)
      integer(kind = kint) :: isize_solver_int2 = -1
! 
      integer(kind = kint_gl), allocatable :: ivec8_2nd(:)
      integer(kind = kint) :: isize_solver_i8_2 = -1
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
! ----------------------------------------------------------------------
!
       subroutine verify_2nd_iccg_matrix(NB, numnod)
!
       integer(kind = kint), intent(in) :: NB, numnod
       integer(kind = kint) :: ncomp
!
       ncomp = NB*numnod
       if (isize_solver_vect2 .lt. 0) then
         call allocate_2nd_iccg_matrix(NB, numnod)
       else
         if (isize_solver_vect2 .lt. ncomp) then
           call deallocate_2nd_iccg_matrix
           call allocate_2nd_iccg_matrix(NB,numnod)
         end if
       end if
!
       end subroutine verify_2nd_iccg_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine verify_2nd_iccg_int_mat(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       if (isize_solver_int2 .lt. 0) then
         call allocate_2nd_iccg_int_mat(numnod)
       else
         if (isize_solver_int2 .lt. numnod) then
           call deallocate_2nd_iccg_int_mat
         call allocate_2nd_iccg_int_mat(numnod)
         end if
       end if
!
       end subroutine verify_2nd_iccg_int_mat
!
!  ---------------------------------------------------------------------
!
       subroutine verify_2nd_iccg_int8_mat(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
       if (isize_solver_int2 .lt. 0) then
         call allocate_2nd_iccg_int8_mat(numnod)
       else
         if (isize_solver_int2 .lt. numnod) then
           call deallocate_2nd_iccg_int8_mat
         call allocate_2nd_iccg_int8_mat(numnod)
         end if
       end if
!
       end subroutine verify_2nd_iccg_int8_mat
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_2nd_iccg_matrix(NB, numnod)
!
       integer(kind = kint), intent(in) :: NB, numnod
!
!
       allocate(xvec_2nd(NB*numnod))
       allocate(bb_2nd(NB*numnod))
       xvec_2nd  =0.0d00
       bb_2nd  =0.0d00
!
       isize_solver_vect2 = NB*numnod
!
       end subroutine allocate_2nd_iccg_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_2nd_iccg_matrix
!
!
       deallocate(xvec_2nd)
       deallocate(bb_2nd)
       isize_solver_vect2 = 0
!
       end subroutine deallocate_2nd_iccg_matrix
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_2nd_iccg_int_mat(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(ivec_2nd(numnod))
       ivec_2nd  = 0
!
       isize_solver_int2 = numnod
!
       end subroutine allocate_2nd_iccg_int_mat
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_2nd_iccg_int_mat
!
!
       deallocate(ivec_2nd)
       isize_solver_int2 = 0
!
       end subroutine deallocate_2nd_iccg_int_mat
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_2nd_iccg_int8_mat(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(ivec8_2nd(numnod))
       ivec8_2nd  = 0
!
       isize_solver_i8_2 = numnod
!
       end subroutine allocate_2nd_iccg_int8_mat
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_2nd_iccg_int8_mat
!
!
       deallocate(ivec8_2nd)
       isize_solver_i8_2 = 0
!
       end subroutine deallocate_2nd_iccg_int8_mat
!
!  ---------------------------------------------------------------------
!
      end module   m_2nd_pallalel_vector

