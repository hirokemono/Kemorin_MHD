!>@file   t_rayleigh_field_IO.f90
!!@brief  module t_rayleigh_field_IO
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine alloc_resolution_4_rayleigh(rayleigh_rtp)
!!      subroutine alloc_rayleigh_component                             &
!!     &         (nnod_r, istart_pe, rayleigh_rtp)
!!      subroutine dealloc_rayleigh_component(rayleigh_rtp)
!!      subroutine dealloc_resolution_4_rayleigh(rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!
!!      subroutine read_rayleigh_grid_info(file_name, rayleigh_rtp)
!!      subroutine read_rayleigh_field_param(file_name, rayleigh_rtp)
!!      subroutine bcast_rayleigh_grid_info(rayleigh_rtp)
!!      subroutine read_each_rayleigh_component                         &
!!     &         (file_name, numnod, d_scalar, rayleigh_rtp)
!!        type(sphere_domain_control), intent(in) :: sdctl
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!
!!      subroutine set_ctl_params_rayleigh_domains                      &
!!     &         (sdctl, rayleigh_rtp, e_msg, ierr)
!!      subroutine set_rayleigh_parallel_param(rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!@endverbatim
!
      module t_rayleigh_field_IO
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use t_field_data_IO
      use t_mesh_data
      use t_group_data
!
      implicit  none
!
      type rayleigh_field
!>        Endian swap flag
        integer :: iflag_swap = 0
!
!>        Trancation
        integer(kind = kint) :: ltr
!
!>        Global radial resolution
        integer(kind = kint) :: nri_gl
!>        Global meridional resolution
        integer(kind = kint) :: nth_gl
!>        Global zonal resolution
        integer(kind = kint) :: nphi_gl
!
!>        Number of subdomains for grid data
        integer(kind = kint) :: ndomain_rtp(3)
!
!>        MPI rank for radial decomposition
        integer(kind = kint) :: irank_r
!>        MPI rank for horizontal decomposition
        integer(kind = kint) :: irank_h
!
!>        Start address in radial direction
        integer(kind = kint) :: kst
!>        End address in radial direction
        integer(kind = kint) :: ked
!>        Start address in meridional direction
        integer(kind = kint) :: lst
!>        End address in meridional direction
        integer(kind = kint) :: led
!
!>        radial grid
        real(kind = kreal), allocatable :: radius_gl(:)
!>        meridional grid
        real(kind = kreal), allocatable :: theta_gl(:)
        real(kind = kreal), allocatable :: costheta_gl(:)
        real(kind = kreal), allocatable :: sintheta_gl(:)
        real(kind = kreal), allocatable :: tweights_gl(:)
!
!>        zonal grid
        real(kind = kreal), allocatable :: phi_gl(:)
        real(kind = kreal), allocatable :: aphi_gl(:)
!
!>        number of data points of rayleigh field data
        integer(kind = kint_gl) :: nnod_rayleigh_in
!>        Start address of rayleigh field data for each process
        integer(kind = kint_gl) :: istart_rayleigh_in
!>        Single component from Rayleigh data
        real(kind = kreal), allocatable :: field_rtp(:)
      end type rayleigh_field
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine alloc_resolution_4_rayleigh(rayleigh_rtp)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
      allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
      allocate(rayleigh_rtp%costheta_gl(rayleigh_rtp%nth_gl))
!
      allocate(rayleigh_rtp%sintheta_gl(rayleigh_rtp%nth_gl))
      allocate(rayleigh_rtp%tweights_gl(rayleigh_rtp%nth_gl))
!
      allocate(rayleigh_rtp%phi_gl(rayleigh_rtp%nphi_gl))
      allocate(rayleigh_rtp%aphi_gl(rayleigh_rtp%nphi_gl))
!
      end subroutine alloc_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rayleigh_component                               &
     &         (nnod_r, istart_pe, rayleigh_rtp)
!
      integer(kind = kint), intent(in) :: nnod_r
      integer(kind = kint_gl), intent(in) :: istart_pe
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      rayleigh_rtp%istart_rayleigh_in = istart_pe
      rayleigh_rtp%nnod_rayleigh_in = nnod_r
      allocate(rayleigh_rtp%field_rtp(rayleigh_rtp%nnod_rayleigh_in))
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
!
      subroutine dealloc_resolution_4_rayleigh(rayleigh_rtp)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      deallocate(rayleigh_rtp%radius_gl)
      deallocate(rayleigh_rtp%costheta_gl, rayleigh_rtp%theta_gl)
!
      deallocate(rayleigh_rtp%sintheta_gl, rayleigh_rtp%tweights_gl)
      deallocate(rayleigh_rtp%phi_gl, rayleigh_rtp%aphi_gl)
!
      end subroutine dealloc_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_rayleigh_field_param(file_name, rayleigh_rtp)
!
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
      use byte_swap_f
!
      character(len = kchara), intent(in) :: file_name
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      integer, parameter :: id_file = 15
      integer, parameter :: iflag_pi = 314
      integer :: i4_tmp(1)
!
      integer(kind = kint_gl) :: l8_byte
      integer(kind = kint) :: i
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'Read parameter file: ', trim(file_name)
        open(id_file, FILE=file_name, STATUS='OLD',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
!
        rayleigh_rtp%iflag_swap  = iendian_KEEP
        read(id_file) i4_tmp(1)
        if(i4_tmp(1) .ne. iflag_pi) then
          rayleigh_rtp%iflag_swap = iendian_FLIP
        end if
!
        read(id_file) rayleigh_rtp%nri_gl
        read(id_file) rayleigh_rtp%nth_gl
        read(id_file) rayleigh_rtp%nphi_gl
!
        if(rayleigh_rtp%iflag_swap .eq. iendian_FLIP) then
          l8_byte = 1
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nri_gl = i4_tmp(1)
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nth_gl = i4_tmp(1)
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nphi_gl = i4_tmp(1)
        end if
!
        call alloc_resolution_4_rayleigh(rayleigh_rtp)
!
        read(id_file) rayleigh_rtp%radius_gl(1:rayleigh_rtp%nri_gl)
        read(id_file) rayleigh_rtp%theta_gl(1:rayleigh_rtp%nth_gl)
        close(id_file)
!
        if(rayleigh_rtp%iflag_swap .eq. iendian_FLIP) then
          l8_byte = rayleigh_rtp%nri_gl
          call byte_swap_real_f(l8_byte, rayleigh_rtp%radius_gl)
          l8_byte = rayleigh_rtp%nth_gl
          call byte_swap_real_f(l8_byte, rayleigh_rtp%theta_gl)
        end if

!$omp parallel do private(i)
        do i = 1, rayleigh_rtp%nth_gl
          rayleigh_rtp%costheta_gl(i) = cos(rayleigh_rtp%theta_gl(i))
        end do
!$omp end parallel do
      end if
!
      end subroutine read_rayleigh_field_param
!
! ----------------------------------------------------------------------
!
      subroutine read_rayleigh_grid_info(file_name, rayleigh_rtp)
!
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
      use byte_swap_f
!
      character(len = kchara), intent(in) :: file_name
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      integer, parameter :: id_file = 15
      integer, parameter :: iflag_pi = 314
      integer :: i4_tmp(1)
!
      integer(kind = kint_gl) :: l8_byte
!
!
      if(my_rank .eq. 0) then
        write(*,*) 'Read parameter file: ', trim(file_name)
        open(id_file, FILE=file_name, STATUS='OLD',                     &
     &       FORM='UNFORMATTED', ACCESS='STREAM')
!
        rayleigh_rtp%iflag_swap  = iendian_KEEP
        read(id_file) i4_tmp(1)
        if(i4_tmp(1) .ne. iflag_pi) then
          rayleigh_rtp%iflag_swap = iendian_FLIP
        end if
!
        read(id_file) rayleigh_rtp%nri_gl
        read(id_file) rayleigh_rtp%nth_gl
        read(id_file) rayleigh_rtp%nphi_gl
!
        if(rayleigh_rtp%iflag_swap .eq. iendian_FLIP) then
          l8_byte = 1
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nri_gl = i4_tmp(1)
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nth_gl = i4_tmp(1)
          call byte_swap_int4_f(l8_byte, i4_tmp)
          rayleigh_rtp%nphi_gl = i4_tmp(1)
        end if
!
        call alloc_resolution_4_rayleigh(rayleigh_rtp)
!
        read(id_file) rayleigh_rtp%radius_gl(1:rayleigh_rtp%nri_gl)
        read(id_file) rayleigh_rtp%theta_gl(1:rayleigh_rtp%nth_gl)
!
        read(id_file) rayleigh_rtp%costheta_gl(1:rayleigh_rtp%nth_gl)
        read(id_file) rayleigh_rtp%sintheta_gl(1:rayleigh_rtp%nth_gl)
        read(id_file) rayleigh_rtp%tweights_gl(1:rayleigh_rtp%nth_gl)
!
        read(id_file) rayleigh_rtp%phi_gl(1:rayleigh_rtp%nphi_gl)
        read(id_file) rayleigh_rtp%aphi_gl(1:rayleigh_rtp%nphi_gl)
        close(id_file)
!
        if(rayleigh_rtp%iflag_swap .eq. iendian_FLIP) then
          l8_byte = rayleigh_rtp%nri_gl
          call byte_swap_real_f(l8_byte, rayleigh_rtp%radius_gl)
          l8_byte = rayleigh_rtp%nth_gl
          call byte_swap_real_f(l8_byte, rayleigh_rtp%theta_gl)

          call byte_swap_real_f(l8_byte, rayleigh_rtp%costheta_gl)
          call byte_swap_real_f(l8_byte, rayleigh_rtp%sintheta_gl)
          call byte_swap_real_f(l8_byte, rayleigh_rtp%tweights_gl)
!
          l8_byte = rayleigh_rtp%nphi_gl
          call byte_swap_real_f(l8_byte, rayleigh_rtp%phi_gl)
          call byte_swap_real_f(l8_byte, rayleigh_rtp%aphi_gl)
        end if
      end if
!
      end subroutine read_rayleigh_grid_info
!
! ----------------------------------------------------------------------
!
      subroutine bcast_rayleigh_grid_info(rayleigh_rtp)
!
      use calypso_mpi_real
      use calypso_mpi_int4
      use transfer_to_long_integers
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      call calypso_mpi_bcast_one_int4(rayleigh_rtp%iflag_swap, 0)
      call calypso_mpi_bcast_one_int4(rayleigh_rtp%nri_gl, 0)
      call calypso_mpi_bcast_one_int4(rayleigh_rtp%nth_gl, 0)
      call calypso_mpi_bcast_one_int4(rayleigh_rtp%nphi_gl, 0)
!
      if(my_rank .ne. 0) call alloc_resolution_4_rayleigh(rayleigh_rtp)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%radius_gl, cast_long(rayleigh_rtp%nri_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%theta_gl,  cast_long(rayleigh_rtp%nth_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%costheta_gl, cast_long(rayleigh_rtp%nth_gl), 0)
!
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%sintheta_gl, cast_long(rayleigh_rtp%nth_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%tweights_gl, cast_long(rayleigh_rtp%nth_gl), 0)
!
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%phi_gl, cast_long(rayleigh_rtp%nphi_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%aphi_gl, cast_long(rayleigh_rtp%nphi_gl), 0)
!
      end subroutine bcast_rayleigh_grid_info
!
! ----------------------------------------------------------------------
!
      subroutine read_each_rayleigh_component(file_name, nd,            &
     &          numnod, ntot_comp_IO, d_IO, rayleigh_rtp)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
      use MPI_binary_data_IO
!
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: nd
      integer(kind = kint), intent(in) :: numnod, ntot_comp_IO
      real(kind = kreal), intent(inout) :: d_IO(numnod, ntot_comp_IO)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      integer ::  id_mpi_file
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
!
      if(my_rank .eq. 0) write(*,*) 'Read Rayleigh field data: ',       &
     &                              trim(file_name)
      call calypso_mpi_read_file_open(file_name, id_mpi_file)
!
      ioffset = kreal * rayleigh_rtp%istart_rayleigh_in
      call mpi_read_real_b                                              &
     &   (id_mpi_file, rayleigh_rtp%iflag_swap, ioffset,                &
     &    rayleigh_rtp%nnod_rayleigh_in, rayleigh_rtp%field_rtp(1))
      call calypso_close_mpi_file(id_mpi_file)
!
!$omp parallel workshare
      d_IO(1:numnod,nd)  = rayleigh_rtp%field_rtp(1:numnod)
!$omp end parallel workshare
!
      end subroutine read_each_rayleigh_component
!
! ----------------------------------------------------------------------
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
!  ---------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
