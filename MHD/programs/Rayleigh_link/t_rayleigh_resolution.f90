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
        integer(kind = kint) :: ltr
!
        integer(kind = kint) :: nri
        integer(kind = kint) :: nth
        integer(kind = kint) :: nphi
!
        integer(kind = kint) :: irank_r
        integer(kind = kint) :: irank_h
!
        integer(kind = kint) :: kst
        integer(kind = kint) :: ked
        integer(kind = kint) :: lst
        integer(kind = kint) :: led
!
        real(kind = kreal), allocatable :: radius(:)
        real(kind = kreal), allocatable :: theta(:)
        real(kind = kreal), allocatable :: cos_theta(:)
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
      r_reso%nri = nri
      r_reso%nth = nth
      r_reso%nphi = 2 * r_reso%nth
      r_reso%kst = kst
      r_reso%ked = ked
      r_reso%lst = lst
      r_reso%led = led
!
      allocate(r_reso%radius(r_reso%nri))
      allocate(r_reso%cos_theta(r_reso%nth))
      allocate(r_reso%theta(r_reso%nth))
!
      r_reso%radius(1:r_reso%nri) = radius(1:r_reso%nri)
      r_reso%cos_theta(1:r_reso%nth) = cos_theta(1:r_reso%nth)
      r_reso%theta(1:r_reso%nth) = acos(cos_theta(1:r_reso%nth))
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
      deallocate(r_reso%radius, r_reso%cos_theta, r_reso%theta)
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
        read(13,*) r_reso%nri
        allocate(r_reso%radius(r_reso%nri))
!
        do kr = 1, r_reso%nri
          read(13,*) itmp, r_reso%radius(kr)
        end do
!
        read(13,*) r_reso%ltr, r_reso%nth
        allocate(r_reso%cos_theta(r_reso%nth))
        allocate(r_reso%theta(r_reso%nth))
!
        do lt = 1, r_reso%nth
          read(13,*) itmp, r_reso%cos_theta(lt), r_reso%theta(lt)
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
     &   (r_reso%nri, 1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST                                                    &
     &   (r_reso%nth, 1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      r_reso%nphi = 2 * r_reso%nth
!
      if(my_rank .ne. 0) then
        allocate(r_reso%radius(r_reso%nri))
        allocate(r_reso%cos_theta(r_reso%nth))
        allocate(r_reso%theta(r_reso%nth))
      end if
      call MPI_BCAST(r_reso%radius, r_reso%nri, CALYPSO_REAL, 0,        &
     &               CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(r_reso%cos_theta, r_reso%nth, CALYPSO_REAL, 0,     &
     &               CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(r_reso%theta, r_reso%nth, CALYPSO_REAL, 0,         &
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
        write(12,'(i16,a)')  r_reso%nri, 'kr, r'
        do kr = 1, r_reso%nri
          write(12,'(i16,1pe25.15)') kr, r_reso%radius(kr)
        end do
        write(12,'(2i16,a)')  r_reso%ltr, r_reso%nth,                   &
     &                      'lt, cos_theta, theta'
        do lt = 1, r_reso%nth
          write(12,'(i16,1p2e25.15)')                                   &
     &             lt, r_reso%cos_theta(lt), r_reso%theta
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
