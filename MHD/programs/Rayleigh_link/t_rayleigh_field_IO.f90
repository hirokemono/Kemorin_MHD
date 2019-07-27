!>@file   t_rayleigh_field_IO.f90
!!@brief  module t_rayleigh_field_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine alloc_rayleigh_component(nnod_r, istart_pe, r_reso)
!!      subroutine dealloc_rayleigh_component(r_reso)
!!        type(Rayleigh_grid_param), intent(inout) :: r_reso
!!
!!      subroutine read_rayleigh_field_param(file_name, r_reso)
!!      subroutine read_each_rayleigh_component(file_name, r_reso)
!!        type(Rayleigh_grid_param), intent(inout) :: r_reso
!!
!!      subroutine set_ctl_params_rayleigh_domains                      &
!!     &         (sdctl, r_reso, e_msg, ierr)
!!      subroutine set_rayleigh_parallel_param(r_reso)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(Rayleigh_grid_param), intent(inout) :: r_reso
!!@endverbatim
!
      module t_rayleigh_field_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use calypso_mpi
      use t_field_data_IO
      use t_rayleigh_resolution
!
      implicit  none
!
!>      Structure for Rayleigh field data
!      type rayleigh_field
!      end type rayleigh_field
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_component(nnod_r, istart_pe, r_reso)
!
      integer(kind = kint_gl), intent(in) :: nnod_r, istart_pe
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
!
      r_reso%istart_rayleigh_in = istart_pe
      r_reso%nnod_rayleigh_in = nnod_r
      allocate(r_reso%field_rtp(r_reso%nnod_rayleigh_in))
!
!$omp parallel workshare
      r_reso%field_rtp(1:r_reso%nnod_rayleigh_in) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_rayleigh_component
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_component(r_reso)
!
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
!
      deallocate(r_reso%field_rtp)
!
      end subroutine dealloc_rayleigh_component
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_field_param(file_name, r_reso)
!
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len = kchara), intent(in) :: file_name
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
      integer :: b_flag
!
      type(calypso_MPI_IO_params), save :: IO_param
!
      integer(kind = kint_gl) :: num64
      integer(kind = kint) :: i
!
!
      write(*,*) 'Read parameter file: ', trim(file_name)
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!
      r_reso%iflag_swap  = iendian_KEEP
      call mpi_read_int4head_b(IO_param, b_flag)
      if(b_flag .ne. 314) r_reso%iflag_swap = iendian_FLIP
!
      call mpi_read_int4head_b(IO_param, r_reso%nri_gl)
      call mpi_read_int4head_b(IO_param, r_reso%nth_gl)
      call mpi_read_int4head_b(IO_param, r_reso%nphi_gl)
!
      allocate(r_reso%radius_gl(r_reso%nri_gl))
      allocate(r_reso%theta_gl(r_reso%nth_gl))
      allocate(r_reso%cos_theta(r_reso%nth_gl))
!
      num64 = r_reso%nri_gl
      call mpi_read_mul_realhead_b(IO_param, num64, r_reso%radius_gl)
      num64 = r_reso%nth_gl
      call mpi_read_mul_realhead_b(IO_param, num64, r_reso%theta_gl)
!
      call close_mpi_file(IO_param)
!
!$omp parallel do private(i)
      do i = 1, r_reso%nth_gl
        r_reso%cos_theta(i) = cos(r_reso%theta_gl(i))
      end do
!$omp end parallel do
!
      end subroutine read_rayleigh_field_param
!
! ----------------------------------------------------------------------
!
      subroutine read_each_rayleigh_component(file_name, r_reso)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_binary_data_IO
!
      character(len = kchara), intent(in) :: file_name
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
      type(calypso_MPI_IO_params), save :: IO_param
!
!
      if(my_rank .eq. 0) write(*,*) 'Read Rayleigh field data: ',       &
     &                  trim(file_name)
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!
      IO_param%ioff_gl = kreal * r_reso%istart_rayleigh_in
      call calypso_mpi_seek_read_real                                   &
     &   (IO_param%id_file, r_reso%iflag_swap, IO_param%ioff_gl,        &
     &    r_reso%nnod_rayleigh_in, r_reso%field_rtp(1))
      call close_mpi_file(IO_param)
!
      end subroutine read_each_rayleigh_component
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_domains                        &
     &         (sdctl, r_reso, e_msg, ierr)
!
      use m_error_IDs
      use t_ctl_data_4_divide_sphere
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(Rayleigh_grid_param), intent(inout) :: r_reso
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
      r_reso%ndomain_rtp(1) = sdctl%num_radial_domain_ctl%intvalue
      r_reso%ndomain_rtp(2) = sdctl%num_horiz_domain_ctl%intvalue
      r_reso%ndomain_rtp(3) = 1
!
      if(r_reso%ndomain_rtp(1)*r_reso%ndomain_rtp(2) .ne. nprocs) then
        e_msg = 'Set correct horizontal and vertical pallelization'
        ierr = ierr_file
      end if
!
      end subroutine set_ctl_params_rayleigh_domains
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_parallel_param(r_reso)
!
      use cal_minmax_and_stacks
!
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
      integer(kind = kint) :: ndivideed, irest
      integer(kind = kint), allocatable :: istack_r(:), istack_h(:)
!
!
      r_reso%irank_h = mod(my_rank,r_reso%ndomain_rtp(2))
      r_reso%irank_r = (my_rank - r_reso%irank_h)                       &
     &                      / r_reso%ndomain_rtp(2)
!
      allocate(istack_r(0:r_reso%ndomain_rtp(1)))
      allocate(istack_h(0:r_reso%ndomain_rtp(2)))
!
      call cal_divide_and_rest(ndivideed, irest, r_reso%nri_gl,         &
     &    r_reso%ndomain_rtp(1))
      call set_stack_of_segments(r_reso%ndomain_rtp(1),                 &
     &    ndivideed, irest, ione, istack_r)
      r_reso%kst = istack_r(r_reso%irank_r  ) + 1
      r_reso%ked = istack_r(r_reso%irank_r+1)
!
      call cal_divide_and_rest(ndivideed, irest, r_reso%nth_gl,         &
     &    r_reso%ndomain_rtp(2))
      call set_stack_of_segments(r_reso%ndomain_rtp(2),                 &
     &    ndivideed, irest, ione, istack_h)
      r_reso%lst = istack_h(r_reso%irank_h  ) + 1
      r_reso%led = istack_h(r_reso%irank_h+1)
      deallocate(istack_r, istack_h)
!
      end subroutine set_rayleigh_parallel_param
!
! ----------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
