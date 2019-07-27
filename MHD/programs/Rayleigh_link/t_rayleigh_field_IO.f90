!>@file   t_rayleigh_field_IO.f90
!!@brief  module t_rayleigh_field_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine alloc_rayleigh_component                             &
!!     &         (nnod_r, istart_pe, rayleigh_rtp)
!!      subroutine dealloc_rayleigh_component(rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!
!!      subroutine read_rayleigh_field_param(file_name, rayleigh_rtp)
!!      subroutine read_each_rayleigh_component(file_name, rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!
!!      subroutine set_ctl_params_rayleigh_domains                      &
!!     &         (sdctl, rayleigh_rtp, e_msg, ierr)
!!      subroutine set_rayleigh_parallel_param(rayleigh_rtp)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_component                               &
     &         (nnod_r, istart_pe, rayleigh_rtp)
!
      integer(kind = kint_gl), intent(in) :: nnod_r, istart_pe
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      rayleigh_rtp%istart_rayleigh_in = istart_pe
      rayleigh_rtp%nnod_rayleigh_in = nnod_r
      allocate(rayleigh_rtp%field_rtp(rayleigh_rtp%nnod_rayleigh_in))
!
!$omp parallel workshare
      rayleigh_rtp%field_rtp(1:rayleigh_rtp%nnod_rayleigh_in) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_rayleigh_component
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_rayleigh_component(rayleigh_rtp)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      deallocate(rayleigh_rtp%field_rtp)
!
      end subroutine dealloc_rayleigh_component
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_field_param(file_name, rayleigh_rtp)
!
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len = kchara), intent(in) :: file_name
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
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
      rayleigh_rtp%iflag_swap  = iendian_KEEP
      call mpi_read_int4head_b(IO_param, b_flag)
      if(b_flag .ne. 314) rayleigh_rtp%iflag_swap = iendian_FLIP
!
      call mpi_read_int4head_b(IO_param, rayleigh_rtp%nri_gl)
      call mpi_read_int4head_b(IO_param, rayleigh_rtp%nth_gl)
      call mpi_read_int4head_b(IO_param, rayleigh_rtp%nphi_gl)
!
      allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
      allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
      allocate(rayleigh_rtp%cos_theta(rayleigh_rtp%nth_gl))
!
      num64 = rayleigh_rtp%nri_gl
      call mpi_read_mul_realhead_b                                      &
     &   (IO_param, num64, rayleigh_rtp%radius_gl)
      num64 = rayleigh_rtp%nth_gl
      call mpi_read_mul_realhead_b                                      &
     &   (IO_param, num64, rayleigh_rtp%theta_gl)
!
      call close_mpi_file(IO_param)
!
!$omp parallel do private(i)
      do i = 1, rayleigh_rtp%nth_gl
        rayleigh_rtp%cos_theta(i) = cos(rayleigh_rtp%theta_gl(i))
      end do
!$omp end parallel do
!
      end subroutine read_rayleigh_field_param
!
! ----------------------------------------------------------------------
!
      subroutine read_each_rayleigh_component(file_name, rayleigh_rtp)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_binary_data_IO
!
      character(len = kchara), intent(in) :: file_name
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      type(calypso_MPI_IO_params), save :: IO_param
!
!
      if(my_rank .eq. 0) write(*,*) 'Read Rayleigh field data: ',       &
     &                  trim(file_name)
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!
      IO_param%ioff_gl = kreal * rayleigh_rtp%istart_rayleigh_in
      call calypso_mpi_seek_read_real                                   &
     &   (IO_param%id_file, rayleigh_rtp%iflag_swap, IO_param%ioff_gl,  &
     &    rayleigh_rtp%nnod_rayleigh_in, rayleigh_rtp%field_rtp(1))
      call close_mpi_file(IO_param)
!
      end subroutine read_each_rayleigh_component
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_params_rayleigh_domains                        &
     &         (sdctl, rayleigh_rtp, e_msg, ierr)
!
      use m_error_IDs
      use t_ctl_data_4_divide_sphere
!
      type(sphere_domain_control), intent(in) :: sdctl
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
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
      rayleigh_rtp%ndomain_rtp(1)                                       &
     &        = sdctl%num_radial_domain_ctl%intvalue
      rayleigh_rtp%ndomain_rtp(2)                                       &
     &        = sdctl%num_horiz_domain_ctl%intvalue
      rayleigh_rtp%ndomain_rtp(3) = 1
!
      if(rayleigh_rtp%ndomain_rtp(1)*rayleigh_rtp%ndomain_rtp(2)        &
     &      .ne. nprocs) then
        e_msg = 'Set correct horizontal and vertical pallelization'
        ierr = ierr_file
      end if
!
      end subroutine set_ctl_params_rayleigh_domains
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_rayleigh_parallel_param(rayleigh_rtp)
!
      use cal_minmax_and_stacks
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      integer(kind = kint) :: ndivideed, irest
      integer(kind = kint), allocatable :: istack_r(:), istack_h(:)
!
!
      rayleigh_rtp%irank_h = mod(my_rank,rayleigh_rtp%ndomain_rtp(2))
      rayleigh_rtp%irank_r = (my_rank - rayleigh_rtp%irank_h)           &
     &                      / rayleigh_rtp%ndomain_rtp(2)
!
      allocate(istack_r(0:rayleigh_rtp%ndomain_rtp(1)))
      allocate(istack_h(0:rayleigh_rtp%ndomain_rtp(2)))
!
      call cal_divide_and_rest(ndivideed, irest, rayleigh_rtp%nri_gl,   &
     &    rayleigh_rtp%ndomain_rtp(1))
      call set_stack_of_segments(rayleigh_rtp%ndomain_rtp(1),           &
     &    ndivideed, irest, ione, istack_r)
      rayleigh_rtp%kst = istack_r(rayleigh_rtp%irank_r  ) + 1
      rayleigh_rtp%ked = istack_r(rayleigh_rtp%irank_r+1)
!
      call cal_divide_and_rest(ndivideed, irest, rayleigh_rtp%nth_gl,   &
     &    rayleigh_rtp%ndomain_rtp(2))
      call set_stack_of_segments(rayleigh_rtp%ndomain_rtp(2),           &
     &    ndivideed, irest, ione, istack_h)
      rayleigh_rtp%lst = istack_h(rayleigh_rtp%irank_h  ) + 1
      rayleigh_rtp%led = istack_h(rayleigh_rtp%irank_h+1)
      deallocate(istack_r, istack_h)
!
      end subroutine set_rayleigh_parallel_param
!
! ----------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
