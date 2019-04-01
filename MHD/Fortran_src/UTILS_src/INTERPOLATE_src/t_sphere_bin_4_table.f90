!
!     module t_sphere_bin_4_table
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine alloc_sphere_divide_points
!      subroutine dealloc_sphere_divide_points
!
!      subroutine set_sph_grid_4_bin
!
      module t_sphere_bin_4_table
!
      use m_precision
!
      implicit none
!
      type sphere_bin_4_table
        integer(kind = kint) :: ntot_sph_bin
!
        integer(kind = kint) :: num_sph_grid(3) = (/1, 1, 0/)
        integer(kind = kint) :: num_sph_bin(3) = (/1, 1, 1/)
!
        real(kind = kreal), allocatable :: r_divide(:)
        real(kind = kreal), allocatable :: theta_divide(:)
        real(kind = kreal), allocatable :: phi_divide(:)
      end type sphere_bin_4_table
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_sphere_divide_points(sph_bin)
!
      type(sphere_bin_4_table), intent(inout) :: sph_bin
!
      allocate( sph_bin%r_divide(0:sph_bin%num_sph_grid(1)) )
      allocate( sph_bin%theta_divide(0:sph_bin%num_sph_grid(2)) )
      allocate( sph_bin%phi_divide(0:sph_bin%num_sph_grid(3)) )
!
      sph_bin%r_divide = 0.0d0
      sph_bin%theta_divide = 0.0d0
      sph_bin%phi_divide = 0.0d0
!
      end subroutine alloc_sphere_divide_points
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_sphere_divide_points(sph_bin)
!
      type(sphere_bin_4_table), intent(inout) :: sph_bin
!
      deallocate(sph_bin%r_divide)
      deallocate(sph_bin%theta_divide)
      deallocate(sph_bin%phi_divide)
!
      end subroutine dealloc_sphere_divide_points
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_sph_grid_4_bin(sph_bin)
!
      type(sphere_bin_4_table), intent(inout) :: sph_bin
!
      real(kind = kreal) :: pi
      real(kind = kreal), parameter :: one = 1.0d0, two = 2.0d0
      real(kind = kreal), parameter :: four = 4.0d0
      integer(kind = kint) :: i
!
!
      pi = four * atan(one)
!
      sph_bin%ntot_sph_bin = sph_bin%num_sph_bin(1)                     &
     &               * sph_bin%num_sph_bin(2) * sph_bin%num_sph_bin(3)
!
      do i = 0, sph_bin%num_sph_grid(2)
        sph_bin%theta_divide(i) = pi * dble(i)                          &
     &                           / dble(sph_bin%num_sph_grid(2))
      end do
!
      do i = 0, sph_bin%num_sph_grid(3)
        sph_bin%phi_divide(i) = two * pi * dble(i)                      &
     &                         / dble(sph_bin%num_sph_grid(3))
      end do
!
      end subroutine set_sph_grid_4_bin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_sph_grid_4_bin(id_file, sph_bin)
!
      integer(kind = kint), intent(in) :: id_file
      type(sphere_bin_4_table), intent(inout) :: sph_bin
!
      integer(kind = kint) :: i
!
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of grid (r, theta, phi)'
      write(id_file,*) '#'
!
      write(id_file,'(3i16)') sph_bin%num_sph_grid
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of bins (r, theta, phi)'
      write(id_file,*) '#'
!
      write(id_file,'(3i16)') sph_bin%num_sph_bin
!
      write(id_file,*) '#'
      write(id_file,*) '#  radial grid'
      write(id_file,*) '#'
!
      do i = 0, sph_bin%num_sph_grid(1)
        write(id_file,'(i16,1pE25.15e3)') i, sph_bin%r_divide(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  elevation grid'
      write(id_file,*) '#'
!
      do i = 0, sph_bin%num_sph_grid(2)
        write(id_file,'(i16,1pE25.15e3)') i, sph_bin%theta_divide(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  azimuthal grid'
      write(id_file,*) '#'
!
      do i = 0, sph_bin%num_sph_grid(3)
        write(id_file,'(i16,1pE25.15e3)') i, sph_bin%phi_divide(i)
      end do
!
      end subroutine check_sph_grid_4_bin
!
!  ---------------------------------------------------------------------
!
      end module t_sphere_bin_4_table
