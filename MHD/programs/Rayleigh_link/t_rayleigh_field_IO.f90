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
!!      subroutine set_ctl_params_rayleigh_domains                      &
!!     &         (sdctl, ra_fld, e_msg, ierr)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(rayleigh_field), intent(inout) :: ra_fld
!!@endverbatim
!
      module t_rayleigh_field_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use calypso_mpi
      use t_field_data_IO
!
      implicit  none
!
!>      Structure for Rayleigh field data
      type rayleigh_field
!>        Endian swap flag
        integer :: iflag_swap = 0
!
!>        Number of subdomains for grid data
        integer(kind = kint) :: ndomain_rtp(3)
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
!
      subroutine set_ctl_params_rayleigh_domains                        &
     &         (sdctl, ra_fld, e_msg, ierr)
!
      use m_error_IDs
      use t_ctl_data_4_divide_sphere
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(rayleigh_field), intent(inout) :: ra_fld
!
      character(len = kchara), intent(inout) :: e_msg
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(      sdctl%num_radial_domain_ctl%iflag .le. 0                 &
     &   .and. sdctl%num_radial_domain_ctl%iflag .le. 0) then
        e_msg = 'Set Parallelization information'
        ierr = ierr_file
        return
      end if
!
      ra_fld%ndomain_rtp(1) = sdctl%num_radial_domain_ctl%intvalue
      ra_fld%ndomain_rtp(2) = sdctl%num_horiz_domain_ctl%intvalue
      ra_fld%ndomain_rtp(3) = 1
!
      if(ra_fld%ndomain_rtp(1)*ra_fld%ndomain_rtp(2) .ne. nprocs) then
        e_msg = 'Set correct horizontal and vertical pallelization'
        ierr = ierr_file
      end if
!
      end subroutine set_ctl_params_rayleigh_domains
!
!-----------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
