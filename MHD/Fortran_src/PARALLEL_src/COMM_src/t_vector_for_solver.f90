!t_vector_for_solver.f90
!     module   t_vector_for_solver
!
!     Writen by H. Matsui  on Dec., 2008
!
! --------------------------------------------------------
!
!!      subroutine copy_communicator_4_solver(S_COMM)
!!        type(mpi_4_solver), intent(inout) :: S_COMM
!!      subroutine verify_iccgN_vec_type(NB, nnod, vect)
!!         integer(kind = kint), intent(in) :: NB, nnod
!!         type(vectors_4_solver), intent(inout) :: vect
!!      subroutine alloc_iccgN_vec_type(NB, nnod, vect)
!!      subroutine dealloc_iccgN_vec_type(vect)
!!         type(vectors_4_solver), intent(inout) :: vect
!!
!!      subroutine alloc_iccg_int_vector(nnod, vect)
!!      subroutine dealloc_iccg_int_vector(vect)
!!         type(vectors_4_solver), intent(inout) :: vect
!!
!!       subroutine alloc_iccg_int8_vector(nnod, vect)
!!       subroutine dealloc_iccg_int8_vector(vect)
!!         type(vectors_4_solver), intent(inout) :: vect
!
! --------------------------------------------------------
!
      module   t_vector_for_solver
!
      use m_precision
!      use calypso_mpi
!
      implicit  none
!
!
!>      Structure for communicatiors for solver
      type mpi_4_solver
!>        Communicator for each level
        integer :: SOLVER_COMM
!>        MPI rank for each level
        integer :: MG_rank
!>        Total process count (1 to petot)
        integer :: nprocs
!>        Lavel for communicator
        integer(kind=kint) :: icolor_MG
      end type mpi_4_solver
!
!>      Structure for vectors for solver
      type vectors_4_solver
!>        Vector for solution vector
        real(kind=kreal), allocatable :: x_vec(:)
!>        Vector for right hand side vector
        real(kind=kreal), allocatable :: b_vec(:)
!>        Size of allocated vectors
        integer(kind = kint) :: isize_solver_vect = -1
! 
        integer(kind=kint), allocatable :: ix_vec(:)
! 
        integer(kind=kint_gl), allocatable :: i8x_vec(:)
      end type vectors_4_solver
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_communicator_4_solver(S_COMM)
!
      use calypso_mpi
!
      type(mpi_4_solver), intent(inout) :: S_COMM
!
!
      S_COMM%icolor_MG = 0
!
      call MPI_COMM_DUP(CALYPSO_COMM, S_COMM%SOLVER_COMM, ierr_MPI)
      call MPI_COMM_RANK(S_COMM%SOLVER_COMM, S_COMM%MG_rank, ierr_MPI)
      S_COMM%nprocs =  nprocs
!
      end subroutine copy_communicator_4_solver
!
!  ---------------------------------------------------------------------
!
       subroutine verify_iccgN_vec_type(NB, nnod, vect)
!
       integer(kind = kint), intent(in) :: NB, nnod
       type(vectors_4_solver), intent(inout) :: vect
       integer(kind = kint) :: ncomp
!
!
       ncomp = NB*nnod
       if (vect%isize_solver_vect .lt. 0) then
         call alloc_iccgN_vec_type(NB, nnod, vect)
       else
         if (vect%isize_solver_vect .lt. ncomp) then
           call dealloc_iccgN_vec_type(vect)
           call alloc_iccgN_vec_type(NB,nnod, vect)
         end if
       end if
!
       end subroutine verify_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccgN_vec_type(NB, nnod, vect)
!
       integer(kind = kint), intent(in) :: NB, nnod
       type(vectors_4_solver), intent(inout) :: vect
!
!
       if(allocated(vect%x_vec)) return
!
       allocate(vect%x_vec(NB*nnod))
       allocate(vect%b_vec(NB*nnod))
!
       if(nnod .gt. 0) then
!$omp parallel workshare
         vect%x_vec(1:NB*nnod) = 0.0d00
         vect%b_vec(1:NB*nnod) = 0.0d00
!$omp end parallel workshare
       end if
!
       vect%isize_solver_vect = NB*nnod
!
       end subroutine alloc_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccgN_vec_type(vect)
!
       type(vectors_4_solver), intent(inout) :: vect
!
!
       if(allocated(vect%x_vec) .eqv. .FALSE.) return
       deallocate(vect%x_vec, vect%b_vec)
       vect%isize_solver_vect = 0
!
       end subroutine dealloc_iccgN_vec_type
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccg_int_vector(nnod, vect)
!
       integer(kind = kint), intent(in) :: nnod
       type(vectors_4_solver), intent(inout) :: vect
!
!
       if(allocated(vect%ix_vec)) return
       allocate(vect%ix_vec(nnod))
       if(nnod .gt. 0) then
!$omp parallel workshare
         vect%ix_vec(1:nnod) = 0
!$omp end parallel workshare
       end if
!
       end subroutine alloc_iccg_int_vector
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccg_int_vector(vect)
!
       type(vectors_4_solver), intent(inout) :: vect
!
       if(allocated(vect%ix_vec) .eqv. .FALSE.) return
       deallocate(vect%ix_vec)
!
       end subroutine dealloc_iccg_int_vector
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine alloc_iccg_int8_vector(nnod, vect)
!
       integer(kind = kint), intent(in) :: nnod
       type(vectors_4_solver), intent(inout) :: vect
!
!
       if(allocated(vect%i8x_vec)) return
       allocate(vect%i8x_vec(nnod))
       if(nnod .gt. 0) then
!$omp parallel workshare
         vect%i8x_vec(1:nnod)  = 0
!$omp end parallel workshare
       end if
!
       end subroutine alloc_iccg_int8_vector
!
!  ---------------------------------------------------------------------
!
       subroutine dealloc_iccg_int8_vector(vect)
!
       type(vectors_4_solver), intent(inout) :: vect
!
       if(allocated(vect%i8x_vec) .eqv. .FALSE.) return
       deallocate(vect%i8x_vec)
!
       end subroutine dealloc_iccg_int8_vector
!
!  ---------------------------------------------------------------------
!
      end module   t_vector_for_solver
