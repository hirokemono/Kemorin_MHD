!set_gauss_integration.90
!      module set_gauss_integration
!
!      Written by H. Matsui
!
!      subroutine construct_gauss_coefs
!
! *************************************************
!
! construct points and coefficients for 
! gauss lagandre integration
!
!    Integration area:  -1 < x < 1
!
! *************************************************
!      subroutine set_gauss_colatitude
!
! *************************************************
!
! construct position of gauss co-latitude
!
!
! *************************************************
!
      module set_gauss_integration
!
      use m_precision
!
      use m_gauss_points
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine construct_gauss_coefs
!
!
      real(kind = kreal) :: x1, x2
      real(kind = kreal) :: z, z1, xm, xl, pp, p1, p2, p3
      integer(kind=kint) :: n, m, nl, j, i
!
      real(kind = kreal), parameter :: zero = 0.0d0, one = 1.0d0
      real(kind = kreal), parameter :: two = 2.0d0, half = one/two
      real(kind = kreal), parameter :: four = 4.0d0, quad = one/four
      real(kind = kreal) :: pi
      integer(kind=kint), parameter :: ione = 1, itwo = 2
!
!
      pi = four * atan(one)
!
      x1 = zero
      x2 = one
!
      w_point(1) = half
      w_coefs(1) = one
!      do i = 1, 1
!       write(*,*) 1,i, w_coefs(i), w_point(i)
!      end do
!
!
      nl = ione
!
      do n = 2, n_point
        m = ( n+1-mod(n+1,2) ) / 2
        xm = half * (x2+x1)
        xl = half * (x2-x1)
!
        do i = 1, m
          z = cos ( pi*(dble(i)-quad)/(dble(n)+half) )
!
          do
!
            p1 = one
            p2 = zero
!
            do j = 1, n
              p3 = p2
              p2 = p1
              p1 = ((two*dble(j)-one)*z*p2-(dble(j)-one)*p3) / dble(j)
            end do
!
            pp = dble(n)*(z*p1-p2)/(z*z-one)
            z1 = z
            z = z1 - p1/pp
            if ( abs(z-z1) .lt. 3.0e-12 ) exit
!
          end do
!
          w_point(i) = xm-xl*z
          w_point(n-i+1) = xm+xl*z
          w_coefs(i) = two*xl / ((one - z*z)*pp*pp)
          w_coefs(n-i+1) = w_coefs(i)
!          write(*,*) n, i, w_point(i), w_coefs(i)
!          write(*,*) n, n-i+1, w_point(n-i+1),  w_coefs(n-i+1)
!
        end do
!
!        do i = 1, n
!          write(*,*) n,i, w_coefs(i), w_point(i)
!        end do
!
      end do
!
      do i = 1, n_point
        w_point(i) = w_point(i)*two - one
        w_coefs(i) = w_coefs(i)*two
      end do
!
      return
      end subroutine construct_gauss_coefs 
!
! -----------------------------------------------------------------------
!
      subroutine set_gauss_colatitude
!
      real(kind = kreal), parameter :: four = 4.0d0, one = 1.0d0
      real(kind = kreal) :: pi
!
      integer (kind = kint) :: i
!
!
      pi = four * atan(one)
!
      do i = 1, n_point
        w_colat(i) = acos(w_point(i))
        w_col_deg(i) = 180.d0 * w_colat(i) / pi
      end do
!
      do i = 1, 2*n_point
        w_azim(i)     = pi*dble(i-1) / dble(n_point)
        w_azim_deg(i) = 180.0d0*dble(i-1) / dble(n_point)
      end do
!
!      do i = 1, n_point
!        write(*,'(i5,1p3E25.15e3)')                                    &
!     &        i,w_point(i),w_colat(i),w_col_deg(i)
!      end do
!
      return
      end subroutine set_gauss_colatitude
!
! -----------------------------------------------------------------------
!
      end module set_gauss_integration
