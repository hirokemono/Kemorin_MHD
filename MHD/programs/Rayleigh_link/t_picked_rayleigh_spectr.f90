!>@file   t_picked_rayleigh_spectr.f90
!!        module t_picked_rayleigh_spectr
!!
!!@author H. Matsui
!!@date Programmed in June, 2014
!!
!>@brief  Module for picked spectr data from Rayleigh
!!
!!@verbatim
!!      subroutine init_each_rayleigh_spectr(nri, radius, ra_picked)
!!      subroutine alloc_each_rayleigh_spectr(nri, ra_picked)
!!      subroutine dealloc_each_rayleigh_spectr(ra_picked)
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: radius(nri)
!!        type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!!
!!      subroutine set_picked_rayleigh_file_name                        &
!!     &         (file_prefix, field_name, l, m, i_step, ra_picked)
!!        character(len = kchara) :: file_prefix, field_name
!!        integer(kind = kint), intent(in) :: l, m, i_step
!!        type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!!
!!      subroutine write_picked_rayleigh_scalar(ra_picked)
!!      subroutine write_picked_rayleigh_vector(ra_picked)
!!      subroutine write_picked_rayleigh_prev_scl(ra_picked)
!!      subroutine write_picked_rayleigh_prev_vec(ra_picked)
!!        type(picked_rayleigh_spectr), intent(in) :: ra_picked
!!
!!      subroutine read_picked_rayleigh_scalar(ra_picked)
!!      subroutine read_picked_rayleigh_vector(ra_picked)
!!      subroutine read_picked_rayleigh_prev_scl(ra_picked)
!!      subroutine read_picked_rayleigh_prev_vec(ra_picked)
!!        type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!!@endverbatim
!!
!!
      module t_picked_rayleigh_spectr
!
      use m_precision
!
      implicit none
!
      integer(kind = kint), parameter, private :: id_out = 12
!
      type picked_rayleigh_spectr
!>        output file name
        character(len = kchara) :: file_name
!
!>        Number of Chabyhsev mode or radial point
        integer(kind = kint) :: nri
!>        radius
        real(kind = kreal), allocatable :: radius(:)
!
!>        Real component of poloidal (or scalar)
        real(kind = kreal), allocatable :: poloidal_re(:)
!>        Imaginary component of poloidal (or scalar)
        real(kind = kreal), allocatable :: poloidal_im(:)
!>        Real component of toroidal
        real(kind = kreal), allocatable :: toroidal_re(:)
!>        Imaginary component of toroidal
        real(kind = kreal), allocatable :: toroidal_im(:)
      end type picked_rayleigh_spectr
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine init_each_rayleigh_spectr(nri, radius, ra_picked)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: radius(nri)
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
!
      call alloc_each_rayleigh_spectr(nri, ra_picked)
!
!$omp parallel workshare
      ra_picked%radius(1:nri) = radius(1:nri)
!$omp end parallel workshare
!
      end subroutine init_each_rayleigh_spectr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_each_rayleigh_spectr(nri, ra_picked)
!
      integer(kind = kint), intent(in) :: nri
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
!
      ra_picked%nri = nri
      allocate(ra_picked%radius(ra_picked%nri))
      allocate(ra_picked%poloidal_re(ra_picked%nri))
      allocate(ra_picked%poloidal_im(ra_picked%nri))
      allocate(ra_picked%toroidal_re(ra_picked%nri))
      allocate(ra_picked%toroidal_im(ra_picked%nri))
!
!$omp parallel workshare
      ra_picked%radius(1:nri) =      0.0d0
      ra_picked%poloidal_re(1:nri) = 0.0d0
      ra_picked%poloidal_im(1:nri) = 0.0d0
      ra_picked%toroidal_re(1:nri) = 0.0d0
      ra_picked%toroidal_im(1:nri) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_each_rayleigh_spectr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_each_rayleigh_spectr(ra_picked)
!
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
!
      deallocate(ra_picked%radius)
      deallocate(ra_picked%poloidal_re, ra_picked%poloidal_im)
      deallocate(ra_picked%toroidal_re, ra_picked%toroidal_im)
!
      end subroutine dealloc_each_rayleigh_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_picked_rayleigh_file_name                          &
     &         (file_prefix, field_name, l, m, i_step, ra_picked)
!
      character(len = kchara) :: file_prefix, field_name
      integer(kind = kint), intent(in) :: l, m, i_step
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      character(len = kchara) :: degree_chara, order_chara
!
!
      write(degree_chara,*) l
      write(order_chara,*)  m
      write(ra_picked%file_name,'(a,a1,a,a1,a,a1,a,a1,i8.8,a4)')        &
     &          trim(file_prefix), '_', trim(field_name),               &
     &          '_', trim(ADJUSTL(degree_chara)),                       &
     &          '_', trim(ADJUSTL(order_chara)), '.',                   &
     &          i_step, '.dat'
!
      end subroutine set_picked_rayleigh_file_name
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_picked_rayleigh_scalar(ra_picked)
!
      type(picked_rayleigh_spectr), intent(in) :: ra_picked
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'Output scalar spectr: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name)
      write(id_out,'(a)') '# Num. of radial points'
      write(id_out,'(i16)') ra_picked%nri
      write(id_out, '(a)') 'Chebyshev_mode Real Imaginary'
      do k = 1, ra_picked%nri
        write(id_out,'(i6,1p2e25.12)') (k-1),                           &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k)
      end do
      close(id_out)
!
      end subroutine write_picked_rayleigh_scalar
!
! -----------------------------------------------------------------------
!
      subroutine write_picked_rayleigh_vector(ra_picked)
!
      type(picked_rayleigh_spectr), intent(in) :: ra_picked
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'Output vector spectr: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name)
      write(id_out,'(a)') '# Num. of radial points'
      write(id_out,'(i16)') ra_picked%nri
      write(id_out, '(3a)') 'Chebyshev_mode ',                          &
     &                     'poloidal_Real poloidal_Imaginary ',         &
     &                     'toroidal_Real toroidal_Imaginary'
      do k = 1, ra_picked%nri
        write(id_out,'(i6,1p4e25.12)') (k-1),                           &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k), &
     &              ra_picked%toroidal_re(k), ra_picked%toroidal_im(k)
      end do
      close(id_out)
!
      end subroutine write_picked_rayleigh_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_picked_rayleigh_prev_scl(ra_picked)
!
      type(picked_rayleigh_spectr), intent(in) :: ra_picked
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'Output previous scalar: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name)
      write(id_out,'(a)') '# Num. of radial points'
      write(id_out,'(i16)') ra_picked%nri
      write(id_out, '(a)') 'Radius Real Imaginary'
      do k = 1, ra_picked%nri
        write(id_out,'(1p3e25.12)') ra_picked%radius(k),                &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k)
      end do
      close(id_out)
!
      end subroutine write_picked_rayleigh_prev_scl
!
! -----------------------------------------------------------------------
!
      subroutine write_picked_rayleigh_prev_vec(ra_picked)
!
      type(picked_rayleigh_spectr), intent(in) :: ra_picked
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'Output previous vector: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name)
      write(id_out,'(a)') '# Num. of radial points'
      write(id_out,'(i16)') ra_picked%nri
      write(id_out, '(3a)') 'radius ',                                  &
     &                     'poloidal_Real poloidal_Imaginary ',         &
     &                     'toroidal_Real toroidal_Imaginary'
      do k = 1, ra_picked%nri
        write(id_out,'(1p5e25.12)') ra_picked%radius(k),                &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k), &
     &              ra_picked%toroidal_re(k), ra_picked%toroidal_im(k)
      end do
      close(id_out)
!
      end subroutine write_picked_rayleigh_prev_vec
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_picked_rayleigh_scalar(ra_picked)
!
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      integer(kind = kint) :: k, itmp
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'Output scalar spectr: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name, STATUS='old')
      read(id_out,*) tmpchara
      read(id_out,*) ra_picked%nri
!
      call alloc_each_rayleigh_spectr(ra_picked%nri, ra_picked)
!
      read(id_out,*) tmpchara
      do k = 1, ra_picked%nri
        read(id_out,*) itmp,                                            &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k)
      end do
      close(id_out)
!
      end subroutine read_picked_rayleigh_scalar
!
! -----------------------------------------------------------------------
!
      subroutine read_picked_rayleigh_vector(ra_picked)
!
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      integer(kind = kint) :: k, itmp
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'Output vector spectr: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name, STATUS='old')
      read(id_out,*) tmpchara
      read(id_out,*) ra_picked%nri
!
      call alloc_each_rayleigh_spectr(ra_picked%nri, ra_picked)
!
      read(id_out,*) tmpchara
      do k = 1, ra_picked%nri
        read(id_out,*) itmp,                                            &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k), &
     &              ra_picked%toroidal_re(k), ra_picked%toroidal_im(k)
      end do
      close(id_out)
!
      end subroutine read_picked_rayleigh_vector
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_picked_rayleigh_prev_scl(ra_picked)
!
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      integer(kind = kint) :: k
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'Output previous scalar: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name, STATUS='old')
      read(id_out,*) tmpchara
      read(id_out,*) ra_picked%nri
!
      call alloc_each_rayleigh_spectr(ra_picked%nri, ra_picked)
!
      read(id_out,*) tmpchara
      do k = 1, ra_picked%nri
        read(id_out,*) ra_picked%radius(k),                             &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k)
      end do
      close(id_out)
!
      end subroutine read_picked_rayleigh_prev_scl
!
! -----------------------------------------------------------------------
!
      subroutine read_picked_rayleigh_prev_vec(ra_picked)
!
      type(picked_rayleigh_spectr), intent(inout) :: ra_picked
!
      integer(kind = kint) :: k
      character(len=kchara) :: tmpchara
!
!
      write(*,*) 'Output previous vector: ', trim(ra_picked%file_name)
      open(id_out, file=ra_picked%file_name, STATUS='old')
      read(id_out,*) tmpchara
      read(id_out,*) ra_picked%nri
!
      call alloc_each_rayleigh_spectr(ra_picked%nri, ra_picked)
!
      read(id_out,*) tmpchara
      do k = 1, ra_picked%nri
        read(id_out,*) ra_picked%radius(k),                             &
     &              ra_picked%poloidal_re(k), ra_picked%poloidal_im(k), &
     &              ra_picked%toroidal_re(k), ra_picked%toroidal_im(k)
      end do
      close(id_out)
!
      end subroutine read_picked_rayleigh_prev_vec
!
! -----------------------------------------------------------------------
!
      end module t_picked_rayleigh_spectr
