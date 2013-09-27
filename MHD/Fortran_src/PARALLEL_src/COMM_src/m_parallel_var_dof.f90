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
!!
!!      subroutine parallel_abort(code , message)
!!
!!      subroutine verify_iccgN_matrix(NB, numnod)
!!      subroutine allocate_iccgN_matrix(NB, numnod)
!!      subroutine deallocate_iccgN_matrix
!!
!!      subroutine allocate_iccg_int_matrix(numnod)
!!      subroutine deallocate_iccg_int_matrix
!!@endverbatim
!
      module   m_parallel_var_dof
!
      use m_precision
      use calypso_mpi
!
      implicit  none
!
!>      MPI communicator for CALYPSO
      integer(kind=kint) :: SOLVER_COMM
!>      total number of processes
      integer(kind=kint) :: nprocs
! 
!>      process ID (start from 0)
      integer(kind=kint) :: my_rank
!>      error flag
      integer(kind=kint) :: ierr
!
!>    file type parameter (0: ascii, 1: binary)
      integer(kind = kint) :: ifile_type = 0
!
      real(kind=kreal), allocatable :: x_vec(:)
      real(kind=kreal), allocatable :: bb(:)
      integer(kind = kint) :: isize_solver_vect = -1
! 
      integer(kind=kint), allocatable :: ix_vec(:)
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
      use m_machine_parameter
!
!
      call  MPI_INIT(ierr)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, SOLVER_COMM, ierr)
      call  MPI_COMM_SIZE(SOLVER_COMM, nprocs, ierr)
      call  MPI_COMM_RANK(SOLVER_COMM, my_rank  , ierr)
!
      if(my_rank .gt. 0) return
      iflag_debug = i_debug
!
      end subroutine parallel_cal_init
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine parallel_abort(code, message)
!
      integer,       intent(in)  ::  code
      character*(*), intent(in)  ::  message
!
      write(*,*) ' ///// abnormal termination ///// ', code,            &
     &                                            ' ', message
!
      call  MPI_ABORT (SOLVER_COMM, ierr)
!
      stop
      end subroutine  parallel_abort
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine verify_iccgN_matrix(NB, numnod)
!
       integer(kind = kint), intent(in) :: NB, numnod
       integer(kind = kint) :: ncomp
!
!
       ncomp = NB*numnod
       if (isize_solver_vect .lt. 0) then
         call allocate_iccgN_matrix(NB,numnod)
       else
         if (isize_solver_vect .lt. ncomp) then
           call deallocate_iccgN_matrix
           call allocate_iccgN_matrix(NB,numnod)
         end if
       end if
!
       end subroutine verify_iccgN_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_iccgN_matrix(NB, numnod)
!
       integer(kind = kint), intent(in) :: NB, numnod
!
!
       allocate(x_vec(NB*numnod))
       allocate(bb(NB*numnod))
       x_vec  =0.0d00
       bb  =0.0d00
!
       isize_solver_vect = NB*numnod
!
       end subroutine allocate_iccgN_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_iccgN_matrix
!
!
       deallocate(x_vec, bb)
!
       isize_solver_vect = 0
!
       end subroutine deallocate_iccgN_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_iccg_int_matrix(numnod)
!
       integer(kind = kint), intent(in) :: numnod
!
!
       allocate(ix_vec(numnod))
       ix_vec  = 0
!
       end subroutine allocate_iccg_int_matrix
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_iccg_int_matrix
!
       deallocate(ix_vec)
!
       end subroutine deallocate_iccg_int_matrix
!
!  ---------------------------------------------------------------------
!
      end module   m_parallel_var_dof
