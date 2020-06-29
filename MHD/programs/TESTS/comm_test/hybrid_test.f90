!
      program hybrid_test
!
!>     MPI communicator
      integer :: CALYPSO_COMM
!
!>      process ID (start from 0)
      integer :: my_rank
!>      total number of processes
      integer :: nprocs
!
!>      error flag for MPI
      integer :: ierr_MPI
!
      integer :: i
      integer allocatable :: itmp(:)
      real :: start_time, end_time
!                                                                               
!
!
      call  MPI_INIT(ierr_MPI)
      call  MPI_COMM_DUP (MPI_COMM_WORLD, CALYPSO_COMM, ierr_MPI)
      call  MPI_COMM_SIZE(CALYPSO_COMM, nprocs, ierr_MPI)
      call  MPI_COMM_RANK(CALYPSO_COMM, my_rank, ierr_MPI)
!
!     
      start_time = MPI_WTIME()                                                                          
      allocate(itmp(nprocs))
!$omp parallel do                                                               
      do i = 1, nprocs
        itmp(i) = i
      end do
!$omp end parallel do                                                           
      write(*,*) 'itmp', my_rank, itmp(my_rank+1)
!
      end_time = MPI_WTIME()                                                                                                                                                        
!
      call  MPI_FINALIZE(ierr_MPI)
      write(*,*) 'time: ' (end_time - start_time)
!
      end program hybrid_test
