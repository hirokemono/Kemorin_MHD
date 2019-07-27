!>@file   t_rayleigh_resolution.f90
!!@brief  module t_rayleigh_resolution
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,            &
!!     &           kst, ked, lst, led, radius, cos_theta, r_reso)
!!      subroutine dealloc_resolution_4_rayleigh(r_reso)
!!      subroutine load_resolution_4_rayleigh(r_reso)
!!        type(Rayleigh_grid_param), intent(inout) :: r_reso
!!      subroutine write_resolution_4_rayleigh(file_name, r_reso)
!!        type(Rayleigh_grid_param), intent(in) :: r_reso
!!@endverbatim
!
      module t_rayleigh_resolution
!
      use m_precision
      use m_constants
      use m_geometry_constants
      use calypso_mpi
      use t_mesh_data
      use t_group_data
!
      implicit none
!
      character(len=kchara), parameter :: rayleigh_resolution_file      &
     &                                = 'Rayleigh_grid_kemo.dat'
!
      type Rayleigh_grid_param
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
      end type Rayleigh_grid_param
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,              &
     &           kst, ked, lst, led, radius, cos_theta, r_reso)
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
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
!
      r_reso%ltr = ltr
      r_reso%nri_gl = nri
      r_reso%nth_gl = nth
      r_reso%nphi_gl = 2 * r_reso%nth_gl
      r_reso%kst = kst
      r_reso%ked = ked
      r_reso%lst = lst
      r_reso%led = led
!
      allocate(r_reso%radius_gl(r_reso%nri_gl))
      allocate(r_reso%theta_gl(r_reso%nth_gl))
      allocate(r_reso%cos_theta(r_reso%nth_gl))
!
      r_reso%radius_gl(1:r_reso%nri_gl) = radius(1:r_reso%nri_gl)
      r_reso%cos_theta(1:r_reso%nth_gl) = cos_theta(1:r_reso%nth_gl)
      r_reso%theta_gl(1:r_reso%nth_gl)                                  &
     &      = acos(cos_theta(1:r_reso%nth_gl))
!
      end subroutine copy_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_resolution_4_rayleigh(r_reso)
!
      type(Rayleigh_grid_param), intent(inout) :: r_reso
!
!
      deallocate(r_reso%radius_gl, r_reso%cos_theta, r_reso%theta_gl)
!
      end subroutine dealloc_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine load_resolution_4_rayleigh(r_reso)
!
      type(Rayleigh_grid_param), intent(inout) :: r_reso
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
        read(13,*) r_reso%nri_gl
        allocate(r_reso%radius_gl(r_reso%nri_gl))
!
        do kr = 1, r_reso%nri_gl
          read(13,*) itmp, r_reso%radius_gl(kr)
        end do
!
        read(13,*) r_reso%ltr, r_reso%nth_gl
        allocate(r_reso%theta_gl(r_reso%nth_gl))
        allocate(r_reso%cos_theta(r_reso%nth_gl))
!
        do lt = 1, r_reso%nth_gl
          read(13,*) itmp, r_reso%cos_theta(lt), r_reso%theta_gl(lt)
        end do
        close(13)
      end if
!
      call MPI_Scatter(r_rank, 1, CALYPSO_INTEGER,                      &
     &                 r_reso%irank_r, 1, CALYPSO_INTEGER,              &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(h_rank, 1, CALYPSO_INTEGER,                      &
     &                 r_reso%irank_h, 1, CALYPSO_INTEGER,              &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(kst_read, 1, CALYPSO_INTEGER,                    &
     &                 r_reso%kst, 1, CALYPSO_INTEGER,                  &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(ked_read, 1, CALYPSO_INTEGER,                    &
     &                 r_reso%ked, 1, CALYPSO_INTEGER,                  &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(lst_read, 1, CALYPSO_INTEGER,                    &
     &                 r_reso%lst, 1, CALYPSO_INTEGER,                  &
     &                 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Scatter(led_read, 1, CALYPSO_INTEGER,                    &
     &                 r_reso%led, 1, CALYPSO_INTEGER,                  &
     &                 0, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. 0) then
        deallocate(r_rank, h_rank)
        deallocate(kst_read, ked_read, lst_read, led_read)
      end if
!
      call MPI_BCAST                                                    &
     &   (r_reso%ltr, 1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST                                                    &
     &   (r_reso%nri_gl, 1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST                                                    &
     &   (r_reso%nth_gl, 1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      r_reso%nphi_gl = 2 * r_reso%nth_gl
!
      if(my_rank .ne. 0) then
        allocate(r_reso%radius_gl(r_reso%nri_gl))
        allocate(r_reso%cos_theta(r_reso%nth_gl))
        allocate(r_reso%theta_gl(r_reso%nth_gl))
      end if
      call MPI_BCAST(r_reso%radius_gl, r_reso%nri_gl, CALYPSO_REAL, 0,  &
     &               CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(r_reso%cos_theta, r_reso%nth_gl, CALYPSO_REAL, 0,  &
     &               CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(r_reso%theta_gl, r_reso%nth_gl, CALYPSO_REAL, 0,   &
     &    CALYPSO_COMM, ierr_MPI)
!
      end subroutine load_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine write_resolution_4_rayleigh(file_name, r_reso)
!
      character(len=kchara) :: file_name
      type(Rayleigh_grid_param), intent(in) :: r_reso
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
      call MPI_Gather(r_reso%irank_r, 1, MPI_INTEGER,                   &
     &                r_rank, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(r_reso%irank_h, 1, MPI_INTEGER,                   &
     &                h_rank, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(r_reso%kst, 1, MPI_INTEGER,                       &
     &                kr_min, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(r_reso%ked, 1, MPI_INTEGER,                       &
     &                kr_max, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(r_reso%lst, 1, MPI_INTEGER,                       &
     &                lt_min, 1, MPI_INTEGER,                           &
     &                0, CALYPSO_COMM, ierr_MPI)
      call MPI_Gather(r_reso%led, 1, MPI_INTEGER,                       &
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
        write(12,'(i16,a)')  r_reso%nri_gl, 'kr, r'
        do kr = 1, r_reso%nri_gl
          write(12,'(i16,1pe25.15)') kr, r_reso%radius_gl(kr)
        end do
        write(12,'(2i16,a)')  r_reso%ltr, r_reso%nth_gl,                &
     &                      'lt, cos_theta, theta'
        do lt = 1, r_reso%nth_gl
          write(12,'(i16,1p2e25.15)')                                   &
     &             lt, r_reso%cos_theta(lt), r_reso%theta_gl
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
      end module t_rayleigh_resolution
