!>@file   palallel_conf_4_rayleigh.f90
!!@brief  module palallel_conf_4_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,            &
!!     &           kst, ked, lst, led, radius, cos_theta, rayleigh_rtp)
!!
!!      subroutine load_resolution_4_rayleigh(file_name, rayleigh_rtp)
!!        type(rayleigh_field), intent(inout) :: rayleigh_rtp
!!      subroutine write_resolution_4_rayleigh(file_name, rayleigh_rtp)
!!        type(rayleigh_field), intent(in) :: rayleigh_rtp
!!@endverbatim
!
      module palallel_conf_4_rayleigh
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use calypso_mpi
      use t_rayleigh_field_IO
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_resolution_4_rayleigh(nri, nth, ltr,              &
     &           kst, ked, lst, led, radius, cos_theta, rayleigh_rtp)
!
      integer(kind = kint), intent(in) :: nri, nth, ltr
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
      call alloc_resolution_4_rayleigh(rayleigh_rtp)
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
! ----------------------------------------------------------------------
!
      subroutine load_resolution_4_rayleigh(file_name, rayleigh_rtp)
!
      use calypso_mpi_int
      use calypso_mpi_real
      use transfer_to_long_integers
!
      character(len=kchara) :: file_name
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
        write(*,*) 'Read Rayleigh grid parameter: ', trim(file_name)
        open(13, file = file_name)
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
      call calypso_mpi_scatter_one_int(r_rank, rayleigh_rtp%irank_r, 0)
      call calypso_mpi_scatter_one_int(h_rank, rayleigh_rtp%irank_h, 0)
      call calypso_mpi_scatter_one_int(kst_read, rayleigh_rtp%kst, 0)
      call calypso_mpi_scatter_one_int(ked_read, rayleigh_rtp%ked, 0)
      call calypso_mpi_scatter_one_int(lst_read, rayleigh_rtp%lst, 0)
      call calypso_mpi_scatter_one_int(led_read, rayleigh_rtp%led, 0)
      if(my_rank .eq. 0) then
        deallocate(r_rank, h_rank)
        deallocate(kst_read, ked_read, lst_read, led_read)
      end if
!
      call calypso_mpi_bcast_one_int(rayleigh_rtp%ltr, 0)
      call calypso_mpi_bcast_one_int(rayleigh_rtp%nri_gl, 0)
      call calypso_mpi_bcast_one_int(rayleigh_rtp%nth_gl, 0)
      rayleigh_rtp%nphi_gl = 2 * rayleigh_rtp%nth_gl
!
      if(my_rank .ne. 0) then
        allocate(rayleigh_rtp%radius_gl(rayleigh_rtp%nri_gl))
        allocate(rayleigh_rtp%cos_theta(rayleigh_rtp%nth_gl))
        allocate(rayleigh_rtp%theta_gl(rayleigh_rtp%nth_gl))
      end if
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%radius_gl, cast_long(rayleigh_rtp%nri_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%cos_theta, cast_long(rayleigh_rtp%nth_gl), 0)
      call calypso_mpi_bcast_real                                       &
     &   (rayleigh_rtp%theta_gl, cast_long(rayleigh_rtp%nth_gl), 0)
!
      end subroutine load_resolution_4_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine write_resolution_4_rayleigh(file_name, rayleigh_rtp)
!
      use calypso_mpi_int
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
      call calypso_mpi_gather_one_int(rayleigh_rtp%irank_r, r_rank, 0)
      call calypso_mpi_gather_one_int(rayleigh_rtp%irank_h, h_rank, 0)
      call calypso_mpi_gather_one_int(rayleigh_rtp%kst, kr_min, 0)
      call calypso_mpi_gather_one_int(rayleigh_rtp%ked, kr_max, 0)
      call calypso_mpi_gather_one_int(rayleigh_rtp%lst, lt_min, 0)
      call calypso_mpi_gather_one_int(rayleigh_rtp%led, lt_max, 0)
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
      end module palallel_conf_4_rayleigh
