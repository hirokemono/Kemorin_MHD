!>@file   t_calypso_mpi_IO_param.f90
!!@brief  module t_calypso_mpi_IO_param
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in Apr., 2006
!
!> @brief Base parameter structure for MPI-IO
!!
!!@verbatim
!!      subroutine alloc_istack_merge(id_rank_IO, nprocs_IO, IO_param)
!!      subroutine dealloc_istack_merge(IO_param)
!!@endverbatim
!
      module t_calypso_mpi_IO_param
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      implicit none
!
!>      Structure for parameters of MPI-IO
      type calypso_MPI_IO_params
!>        File ID for MPI-IO
        integer ::  id_file
!>        process ID for MPI-IO
        integer(kind=kint) ::  id_rank
!>        number of subdomains (not equal to number of processes)
        integer(kind=kint) ::  nprocs_in
!
!>        global file IO point
        integer(kind = kint_gl) :: ioff_gl
!
!>        Stack of data lengh in each domain
        integer(kind = kint_gl), pointer :: istack_merged(:)
      end type calypso_MPI_IO_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_istack_merge(id_rank_IO, nprocs_IO, IO_param)
!
      integer(kind = kint), intent(in) :: nprocs_IO, id_rank_IO
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      IO_param%id_rank =   id_rank_IO
      IO_param%nprocs_in = nprocs_IO
!
      allocate(IO_param%istack_merged(0:IO_param%nprocs_in))
      IO_param%istack_merged = 0
!
      end subroutine alloc_istack_merge
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_istack_merge(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      deallocate(IO_param%istack_merged)
!
      end subroutine dealloc_istack_merge
!
!  ---------------------------------------------------------------------
!
      subroutine copy_istack_4_parallell_data(istack8, IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      integer(kind = kint_gl), intent(in)                               &
     &                        :: istack8(0:IO_param%nprocs_in)
!
!
!$omp parallel workshare
      IO_param%istack_merged(0:IO_param%nprocs_in)                      &
     &      = istack8(0:IO_param%nprocs_in)
!$omp end parallel workshare
!
      end subroutine copy_istack_4_parallell_data
!
!  ---------------------------------------------------------------------
!
      subroutine mul_istack_4_parallell_vect(nvect, IO_param)
!
      integer(kind = kint), intent(in)  :: nvect
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
!$omp parallel workshare
      IO_param%istack_merged(0:IO_param%nprocs_in)                      &
     &      = nvect * IO_param%istack_merged(0:IO_param%nprocs_in)
!$omp end parallel workshare
!
      end subroutine mul_istack_4_parallell_vect
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_4_parallell_data(num_local, IO_param)
!
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: num_global(nprocs)
      integer(kind = kint) :: ip
!
!
      call MPI_Allgather(num_local, ione, CALYPSO_INTEGER,              &
     &    num_global, ione, CALYPSO_INTEGER, CALYPSO_COMM,              &
     &    ierr_MPI)
!
      IO_param%istack_merged(0) = 0
      do ip = 1, nprocs
        IO_param%istack_merged(ip) = IO_param%istack_merged(ip-1)       &
     &                              + num_global(ip)
      end do
!
      end subroutine set_istack_4_parallell_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_istack_4_fixed_num(num_local, IO_param)
!
      integer(kind = kint), intent(in) :: num_local
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
      integer(kind = kint) :: ip
!
!
      do ip = 0, nprocs
        IO_param%istack_merged(ip) = ip * num_local
      end do
!
      end subroutine set_istack_4_fixed_num
!
!  ---------------------------------------------------------------------
!
      end module t_calypso_mpi_IO_param
