!
!      program gaussian_points
!
      program gaussian_points
!
      use m_precision
      use m_gauss_points
!
      implicit none
!
      integer (kind = kint) :: i, nth_g
!
   10 continue
!
      write(*,*) 'imput number of points (end: negative values)'
      read(*,*) nth_g
!
      if (nth_g.le.0) go to 999
!
      call construct_gauss_coefs(nth_g, gauss1)
      call set_gauss_colatitude(gauss1)
!
      write(*,*) 'gaussian points and coefficients'
      do i = 1, gauss1%n_point
        write(*,'(i5,1p2E25.15e3)')                                     &
     &         i, gauss1%point(i), gauss1%weight(i)
      end do
!
      write(*,*) 'Gauss-Legendre colatitude'
      do i = 1, gauss1%n_point
        write(*,'(i5,1p3E25.15e3)') i, gauss1%point(i),                 &
     &         gauss1%colat(i), gauss1%colat_deg(i)
      end do
!
      write(*,*) 'Azimuth'
      do i = 1, 2 * gauss1%n_point
        write(*,'(i5,1p2E25.15e3)')                                     &
     &         i, gauss1%azimuth(i), gauss1%azim_deg(i)
      end do
!
      call dealloc_gauss_colatitude(gauss1)
      call dealloc_gauss_points(gauss1)
!
      go to 10
!
!
 999  continue
      stop
      end
