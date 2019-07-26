!>@file   t_rayleigh_field_IO.f90
!!@brief  module t_rayleigh_field_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine alloc_rayleigh_component(nnod_r, istart_pe, ra_fld)
!!      subroutine alloc_rayleigh_1d_grids(ra_fld)
!!        type(rayleigh_field), intent(inout) :: ra_fld
!!      subroutine dealloc_rayleigh_component(ra_fld)
!!      subroutine dealloc_rayleigh_1d_grids(ra_fld)
!!        type(rayleigh_field), intent(inout) :: ra_fld
!!      subroutine check_rayleigh_1d_grids(ra_fld)
!!        type(rayleigh_field), intent(in) :: ra_fld
!!
!!      subroutine read_rayleigh_field_param(file_name, ra_fld)
!!      subroutine read_each_rayleigh_component(file_name, ra_fld)
!!        type(rayleigh_field), intent(inout) :: ra_fld
!!
!!      subroutine load_local_field_from_rayleigh(ra_fld, t_IO, fld_IO)
!!        type(rayleigh_field), intent(in) :: ra_fld
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module t_rayleigh_field_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use calypso_mpi
      use t_field_data_IO
      use set_parallel_file_name
!
      implicit  none
!
!>      Structure for Rayleigh restart data
      type rayleigh_field
!>        Endian swap flag
        integer :: iflag_swap = 0
!
!>        global radial resolution
        integer :: nri_gl
!>        global meridional resolution
        integer :: nth_gl
!>        global horizontal resolution
        integer :: nphi_gl
!>        radial grid
        real(kind = kreal), allocatable :: radius_gl(:)
!>        meridional grid
        real(kind = kreal), allocatable :: theta_gl(:)
!
        integer(kind = kint_gl) :: nnod_rayleigh_in
!
        integer(kind = kint_gl) :: istart_rayleigh_in
!>        Single component from Rayleigh data
        real(kind = kreal), allocatable :: rayleigh_in(:)
      end type rayleigh_field
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_component(nnod_r, istart_pe, ra_fld)
!
      integer(kind = kint_gl), intent(in) :: nnod_r, istart_pe
      type(rayleigh_field), intent(inout) :: ra_fld
!
!
      ra_fld%istart_rayleigh_in = istart_pe
      ra_fld%nnod_rayleigh_in = nnod_r
      allocate(ra_fld%rayleigh_in(ra_fld%nnod_rayleigh_in))
!
!$omp parallel workshare
      ra_fld%rayleigh_in(1:ra_fld%nnod_rayleigh_in) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_rayleigh_component
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_1d_grids(ra_fld)
!
      type(rayleigh_field), intent(inout) :: ra_fld
!
      allocate(ra_fld%radius_gl(ra_fld%nri_gl))
      allocate(ra_fld%theta_gl(ra_fld%nth_gl))
!
      ra_fld%radius_gl(1:ra_fld%nri_gl) = 0.0d0
      ra_fld%theta_gl(1:ra_fld%nth_gl) = 0.0d0
!
      end subroutine alloc_rayleigh_1d_grids
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_component(ra_fld)
!
      type(rayleigh_field), intent(inout) :: ra_fld
!
!
      deallocate(ra_fld%rayleigh_in)
!
      end subroutine dealloc_rayleigh_component
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_1d_grids(ra_fld)
!
      type(rayleigh_field), intent(inout) :: ra_fld
!
      deallocate(ra_fld%radius_gl, ra_fld%theta_gl)
!
      end subroutine dealloc_rayleigh_1d_grids
!
!-----------------------------------------------------------------------
!
      subroutine check_rayleigh_1d_grids(ra_fld)
!
      type(rayleigh_field), intent(in) :: ra_fld
!
      integer(kind = kint) :: i
!
!
      write(50+my_rank,*) 'iflag_swap: ', ra_fld%iflag_swap
      write(50+my_rank,*) 'global_grid',                                &
     &                   ra_fld%nri_gl, ra_fld%nth_gl, ra_fld%nphi_gl
      do i = 1, ra_fld%nri_gl
        write(50+my_rank,*) i, ra_fld%radius_gl(i)
      end do
      do i = 1, ra_fld%nth_gl
        write(50+my_rank,*) i, ra_fld%theta_gl(i)
      end do
!
      end subroutine check_rayleigh_1d_grids
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_field_param(file_name, ra_fld)
!
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len = kchara), intent(in) :: file_name
      type(rayleigh_field), intent(inout) :: ra_fld
!
      integer :: b_flag
!
      type(calypso_MPI_IO_params), save :: IO_param
!
      integer(kind = kint_gl) :: num64
!
!
      write(*,*) 'Read parameter file: ', trim(file_name)
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!
      ra_fld%iflag_swap  = iendian_KEEP
      call mpi_read_int4head_b(IO_param, b_flag)
      if(b_flag .ne. 314) ra_fld%iflag_swap = iendian_FLIP
!
      call mpi_read_int4head_b(IO_param, ra_fld%nri_gl)
      call mpi_read_int4head_b(IO_param, ra_fld%nth_gl)
      call mpi_read_int4head_b(IO_param, ra_fld%nphi_gl)
!
      call alloc_rayleigh_1d_grids(ra_fld)
!
      num64 = ra_fld%nri_gl
      call mpi_read_mul_realhead_b(IO_param, num64, ra_fld%radius_gl)
      num64 = ra_fld%nth_gl
      call mpi_read_mul_realhead_b(IO_param, num64, ra_fld%theta_gl)
!
      call close_mpi_file(IO_param)
!
      if(i_debug .gt. 0) then
        call check_rayleigh_1d_grids(ra_fld)
      end if
!
      call dealloc_rayleigh_1d_grids(ra_fld)
!
      end subroutine read_rayleigh_field_param
!
! ----------------------------------------------------------------------
!
      subroutine read_each_rayleigh_component(file_name, ra_fld)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_binary_data_IO
!
      character(len = kchara), intent(in) :: file_name
!
      type(rayleigh_field), intent(inout) :: ra_fld
!
      type(calypso_MPI_IO_params), save :: IO_param
!
!
      if(my_rank .eq. 0) write(*,*) 'Read Rayleigh field data: ',       &
     &                  trim(file_name)
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!
      IO_param%ioff_gl = kreal * ra_fld%istart_rayleigh_in
      call calypso_mpi_seek_read_real                                   &
     &   (IO_param%id_file, ra_fld%iflag_swap, IO_param%ioff_gl,        &
     &    ra_fld%nnod_rayleigh_in, ra_fld%rayleigh_in(1))
      call close_mpi_file(IO_param)
!
      end subroutine read_each_rayleigh_component
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine load_local_field_from_rayleigh(ra_fld, t_IO, fld_IO)
!
      use t_time_data
!
      type(rayleigh_field), intent(in) :: ra_fld
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
!
      t_IO%i_time_step = 7000
      t_IO%time = 0.7
      t_IO%dt = 0.0d0
!
      fld_IO%nnod_IO = int(ra_fld%nnod_rayleigh_in)
      fld_IO%num_field_IO = 1
      call alloc_phys_name_IO(fld_IO)
!
      fld_IO%fld_name = 'temperature'
      fld_IO%num_comp_IO(1) = 1
      call cal_istack_phys_comp_IO(fld_IO)
!
      call alloc_phys_data_IO(fld_IO)
      fld_IO%d_IO(1:fld_IO%nnod_IO,1)                                   &
     &             = ra_fld%rayleigh_in(1:fld_IO%nnod_IO)
!
      end subroutine load_local_field_from_rayleigh
!
! -----------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
