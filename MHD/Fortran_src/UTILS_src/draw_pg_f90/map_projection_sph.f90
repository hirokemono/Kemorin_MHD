!
!      module map_projection_sph
!
!      Written by H. Matsui on March, 2009
!
!      subroutine allocate_map_grid(num_lat, num_long)
!      subroutine deallocate_map_grid
!      subroutine plmap (visble, x, y, z)
!      subroutine aitoff (sin_t, cos_t, phi, xg, yg)
!          map projection using the Hammer-Aitoff equal-area projection
!*************************************************
!*
!*   make grid data for surface mapping
!*
!*************************************************
!*
!*  sin_t, cos_t : theta of spherical coordinate (rad)
!*  phi : phi of spherical coordinate  (rad)
!*
!*  xg : x-point of grid -2...2
!*  yg : y-point of grid -1...1
!*
!*
!*************************************************
!
      module map_projection_sph
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: ntheta, nphi
      real(kind= kreal), allocatable :: phi_map(:)
      real(kind= kreal), allocatable :: cos_theta_map(:)
      real(kind= kreal), allocatable :: sin_theta_map(:)
!
      private :: ntheta, nphi
      private :: phi_map, cos_theta_map, sin_theta_map
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_map_grid(num_lat, num_long)
!
      integer(kind = kint), intent(in) :: num_lat, num_long
!
!
      ntheta = num_lat
      nphi =   num_long
      allocate( phi_map(nphi) )
!
      allocate( cos_theta_map(ntheta) )
      allocate( sin_theta_map(ntheta) )
!
      if(nphi .gt. 0) phi_map = 0.0d0
      if(ntheta .gt. 0) cos_theta_map = 0.0d0
      if(ntheta .gt. 0) sin_theta_map = 0.0d0
!
      end subroutine allocate_map_grid
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_map_grid
!
      deallocate( phi_map )
      deallocate( cos_theta_map, sin_theta_map )
!
      end subroutine deallocate_map_grid
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine plmap (visble, x, y, z)
!*
      integer(kind = kint), intent(inout) :: visble
      real(kind = kreal), intent(in) ::    x, y, z
!*
      integer(kind = kint) :: ix, iy
      real(kind = kreal) :: xl2, den, xg, yg
      real :: xg_real, yg_real
!
!
      ix = int(x)
      iy = int(y)
!*
      call aitoff (sin_theta_map(iy), cos_theta_map(iy), phi_map(ix),   &
     &    xg, yg)
      xg_real = real(xg)
      yg_real = real(yg)
!
      if (visble.eq.0) then
        call pgmove(xg_real, yg_real)
      else
        call pgdraw(xg_real, yg_real)
      end if
!*
      end subroutine plmap
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine aitoff(sin_t, cos_t, phi, xg, yg)
!*
      real(kind = kreal), intent(in) :: sin_t, cos_t, phi
      real(kind = kreal), intent(inout) :: xg, yg
!
      real(kind = kreal) :: xl2, den
      real(kind = kreal), parameter :: one = 1.0d0, two = 2.0d0
      real(kind = kreal), parameter :: half = one / two
!*
!
      xl2 = half * phi
      den = sqrt( one + sin_t*sin(xl2) )
      xg = -real( two * sin_t * cos(xl2) / den)
      yg =  real( cos_t / den)
!*
      end subroutine aitoff
!
! ----------------------------------------------------------------------
!
      end module map_projection_sph
