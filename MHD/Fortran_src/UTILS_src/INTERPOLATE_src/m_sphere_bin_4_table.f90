!
!     module m_sphere_bin_4_table
!
      module m_sphere_bin_4_table
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ntot_sph_bin
!
      integer(kind = kint) :: num_sph_grid(3) = (/1, 1, 0/)
      integer(kind = kint) :: num_sph_bin(3) = (/1, 1, 1/)
!
      real(kind = kreal), allocatable :: r_divide(:)
      real(kind = kreal), allocatable :: theta_divide(:)
      real(kind = kreal), allocatable :: phi_divide(:)
!
!      subroutine allocate_sphere_divide_points
!      subroutine deallocate_sphere_divide_points
!
!      subroutine set_sph_grid_4_bin
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_sphere_divide_points
!
      allocate( r_divide(0:num_sph_grid(1)) )
      allocate( theta_divide(0:num_sph_grid(2)) )
      allocate( phi_divide(0:num_sph_grid(3)) )
!
      r_divide = 0.0d0
      theta_divide = 0.0d0
      phi_divide = 0.0d0
!
      end subroutine allocate_sphere_divide_points
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_sphere_divide_points
!
      deallocate(r_divide)
      deallocate(theta_divide)
      deallocate(phi_divide)
!
      end subroutine deallocate_sphere_divide_points
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_sph_grid_4_bin
!
      real(kind = kreal) :: pi
      real(kind = kreal), parameter :: one = 1.0d0, two = 2.0d0
      real(kind = kreal), parameter :: four = 4.0d0
      integer(kind = kint) :: i
!
!
      pi = four * atan(one)
!
      ntot_sph_bin = num_sph_bin(1)*num_sph_bin(2)*num_sph_bin(3)
!
      do i = 0, num_sph_grid(2)
        theta_divide(i) = pi * dble(i) / dble( num_sph_grid(2) )
      end do
!
      do i = 0, num_sph_grid(3)
        phi_divide(i) = two * pi * dble(i) / dble( num_sph_grid(3) )
      end do
!
      end subroutine set_sph_grid_4_bin
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_sph_grid_4_bin(id_file)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint) :: i
!
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of grid (r, theta, phi)'
      write(id_file,*) '#'
!
      write(id_file,'(3i15)') num_sph_grid
!
      write(id_file,*) '#'
      write(id_file,*) '#  number of bins (r, theta, phi)'
      write(id_file,*) '#'
!
      write(id_file,'(3i15)') num_sph_bin
!
      write(id_file,*) '#'
      write(id_file,*) '#  radial grid'
      write(id_file,*) '#'
!
      do i = 0, num_sph_grid(1)
        write(id_file,'(i15,1pE25.15e3)') i, r_divide(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  elevation grid'
      write(id_file,*) '#'
!
      do i = 0, num_sph_grid(2)
        write(id_file,'(i15,1pE25.15e3)') i, theta_divide(i)
      end do
!
      write(id_file,*) '#'
      write(id_file,*) '#  azimuthal grid'
      write(id_file,*) '#'
!
      do i = 0, num_sph_grid(3)
        write(id_file,'(i15,1pE25.15e3)') i, phi_divide(i)
      end do
!
      end subroutine check_sph_grid_4_bin
!
!  ---------------------------------------------------------------------
!
      end module m_sphere_bin_4_table
