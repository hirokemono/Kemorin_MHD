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
!!
!!      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,            &
!!     &           kst, ked, lst, led, radius, cos_theta, rayleigh_rtp)
!!      subroutine dealloc_resolution_4_rayleigh(rayleigh_rtp)
!!      subroutine load_resolution_4_rayleigh(rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!      subroutine write_resolution_4_rayleigh(file_name, rayleigh_rtp)
!!        type(rayleigh_field), intent(in) :: rayleigh_rtp
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
      character(len=kchara), parameter :: rayleigh_resolution_file      &
     &                                = 'Rayleigh_grid_kemo.dat'
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
        real(kind = kreal), allocatable :: cos_theta(:)
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
! ----------------------------------------------------------------------
!
      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,              &
     &           kst, ked, lst, led, radius, cos_theta, rayleigh_rtp)
!
      integer(kind = kint), intent(in) :: nri
      integer(kind = kint), intent(in) :: nth
      integer(kind = kint), intent(in) :: ltr
!
      integer(kind = kint), intent(in) :: kst, ked
      integer(kind = kint), intent(in) :: lst, led
!
      real(kind = kreal), intent(in) :: radius(nri)
      real(kind = kreal), intent(in) :: cos_theta(nth)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      rayleigh_rtp%ltr = ltr
      rayleigh_rtp%nri_gl = nri
      rayleigh_rtp%nth_gl = nth
      rayleigh_rtp%nphi_gl = 2 * rayleigh_rtp%nth_gl
      rayleigh_rtp%kst = kst
      rayleigh_rtp%ked = ked
      rayleigh_rtp%lst = lst
      rayleigh_rtp%led = led
!
      allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
      allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
      allocate(rayleigh_rtp%cos_theta(rayleigh_rtp%nth_gl))
!
      rayleigh_rtp%radius_gl(1:rayleigh_rtp%nri_gl)                     &
     &      = radius(1:rayleigh_rtp%nri_gl)
      rayleigh_rtp%cos_theta(1:rayleigh_rtp%nth_gl)                     &
     &      = cos_theta(1:rayleigh_rtp%nth_gl)
      rayleigh_rtp%theta_gl(1:rayleigh_rtp%nth_gl)                      &
     &      = acos(cos_theta(1:rayleigh_rtp%nth_gl))
!
      end subroutine copy_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_resolution_4_rayleigh(rayleigh_rtp)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
!
      deallocate(rayleigh_rtp%radius_gl)
      deallocate(rayleigh_rtp%cos_theta, rayleigh_rtp%theta_gl)
!
      end subroutine dealloc_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine load_resolution_4_rayleigh(rayleigh_rtp)
!
      type(rayleigh_field), intent(inout) :: rayleigh_rtp
!
      integer, allocatable :: r_rank(:), h_rank(:)
      integer(kind = kint), allocatable :: kst_read(:)
      integer(kind = kint), allocatable :: ked_read(:)
      integer(kind = kint), allocatable :: lst_read(:)
      integer(kind = kint), allocatable :: led_read(:)
!
      integer(kind = kint) :: ip, kr, lt, itmp
      character(len=kchara) :: tmpchara
!
!
      if(my_rank .eq. 0) then
        allocate(r_rank(nprocs))
        allocate(h_rank(nprocs))
        allocate(kst_read(nprocs))
        allocate(ked_read(nprocs))
        allocate(lst_read(nprocs))
        allocate(led_read(nprocs))
!
        write(*,*) 'Read Rayleigh grid parameter: ',                    &
     &            trim(rayleigh_resolution_file)
        open(13, file = rayleigh_resolution_file)
        read(13,*) tmpchara
        do ip = 1, nprocs
          read(13,*) itmp, r_rank(ip), h_rank(ip),                      &
     &          kst_read(ip), ked_read(ip), lst_read(ip), led_read(ip)
        end do
!
        read(13,*) rayleigh_rtp%nri_gl
        allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
!
        do kr = 1, rayleigh_rtp%nri_gl
          read(13,*) itmp, rayleigh_rtp%radius_gl(kr)
        end do
!
        read(13,*) rayleigh_rtp%ltr, rayleigh_rtp%nth_gl
        allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
        allocate(rayleigh_rtp%cos_theta(rayleigh_rtp%nth_gl))
!
        do lt = 1, rayleigh_rtp%nth_gl
          read(13,*) itmp, rayleigh_rtp%cos_theta(lt),                  &
     &                     rayleigh_rtp%theta_gl(lt)
        end do
        close(13)
      end if
!
      call MPI_Scatter(r_rank, 1, CALYPSO_INTEGER,                      &
     &                 rayleigh_rtp%irank_r, 1, CALYPSO_INTEGER,        &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(h_rank, 1, CALYPSO_INTEGER,                      &
     &                 rayleigh_rtp%irank_h, 1, CALYPSO_INTEGER,        &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(kst_read, 1, CALYPSO_INTEGER,                    &
     &                 rayleigh_rtp%kst, 1, CALYPSO_INTEGER,            &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(ked_read, 1, CALYPSO_INTEGER,                    &
     &                 rayleigh_rtp%ked, 1, CALYPSO_INTEGER,            &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(lst_read, 1, CALYPSO_INTEGER,                    &
     &                 rayleigh_rtp%lst, 1, CALYPSO_INTEGER,            &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(led_read, 1, CALYPSO_INTEGER,                    &
     &                 rayleigh_rtp%led, 1, CALYPSO_INTEGER,            &
     &                 0, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. 0) then
        deallocate(r_rank, h_rank)
        deallocate(kst_read, ked_read, lst_read, led_read)
      end if
!
      call MPI_BCAST(rayleigh_rtp%ltr, 1, CALYPSO_INTEGER, 0,           &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(rayleigh_rtp%nri_gl, 1, CALYPSO_INTEGER, 0,        &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(rayleigh_rtp%nth_gl, 1, CALYPSO_INTEGER, 0,        &
     &    CALYPSO_COMM, ierr_MPI)
      rayleigh_rtp%nphi_gl = 2 * rayleigh_rtp%nth_gl
!
      if(my_rank .ne. 0) then
        allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
        allocate(rayleigh_rtp%cos_theta(rayleigh_rtp%nth_gl))
        allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
      end if
      call MPI_BCAST(rayleigh_rtp%radius_gl, rayleigh_rtp%nri_gl,       &
     &               CALYPSO_REAL, 0,  CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(rayleigh_rtp%cos_theta, rayleigh_rtp%nth_gl,       &
     &               CALYPSO_REAL, 0,CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(rayleigh_rtp%theta_gl, rayleigh_rtp%nth_gl,        &
     &               CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine load_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine write_resolution_4_rayleigh(file_name, rayleigh_rtp)
!
      character(len=kchara) :: file_name
      type(rayleigh_field), intent(in) :: rayleigh_rtp
!
      integer :: ip, kr, lt
      integer, allocatable :: r_rank(:), h_rank(:)
      integer, allocatable :: kr_min(:), kr_max(:)
      integer, allocatable :: lt_min(:), lt_max(:)

!
      if(my_rank .eq. 0) then
        allocate(r_rank(nprocs))
        allocate(h_rank(nprocs))
        allocate(kr_min(nprocs))
        allocate(kr_max(nprocs))
        allocate(lt_min(nprocs))
        allocate(lt_max(nprocs))
      end if
!
      call MPI_Gather(rayleigh_rtp%irank_r, 1, MPI_INTEGER,             &
     &                r_rank, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(rayleigh_rtp%irank_h, 1, MPI_INTEGER,             &
     &                h_rank, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(rayleigh_rtp%kst, 1, MPI_INTEGER,                 &
     &                kr_min, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(rayleigh_rtp%ked, 1, MPI_INTEGER,                 &
     &                kr_max, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(rayleigh_rtp%lst, 1, MPI_INTEGER,                 &
     &                lt_min, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(rayleigh_rtp%led, 1, MPI_INTEGER,                 &
     &                lt_max, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
!
        if(my_rank .eq. 0) then
        open(12, file = file_name)
        write(12,'(2a)') 'my_rank, r_min_lc, r_max_lc, ',               &
     &               'theta_min_lc, theta_max_lc'
        do ip = 1, nprocs
          write(12,'(7i16)') ip, r_rank(ip), h_rank(ip),                &
     &                kr_min(ip), kr_max(ip), lt_min(ip), lt_max(ip)
        end do
!
        write(12,'(i16,a)')  rayleigh_rtp%nri_gl, '  kr, r'
        do kr = 1, rayleigh_rtp%nri_gl
          write(12,'(i16,1pe25.15)') kr, rayleigh_rtp%radius_gl(kr)
        end do
        write(12,'(2i16,a)')  rayleigh_rtp%ltr, rayleigh_rtp%nth_gl,    &
     &                      'lt, cos_theta, theta'
        do lt = 1, rayleigh_rtp%nth_gl
          write(12,'(i16,1p2e25.15)')                                   &
     &       lt, rayleigh_rtp%cos_theta(lt), rayleigh_rtp%theta_gl(lt)
        end do
        close(12)
        deallocate(r_rank, h_rank)
        deallocate(kr_min, kr_max, lt_min, lt_max)
      end if
!
      end subroutine write_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      end module t_rayleigh_field_IO
