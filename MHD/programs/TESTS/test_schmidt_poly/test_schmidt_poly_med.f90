!
!      program test_schmidt_poly_med
!
      program test_schmidt_poly_med
!
!     Written by H. Matsui on June, 2007
!
      use m_schmidt_poly_on_gauss
      use m_spherical_harmonics
      use schmidt_poly_on_meridian
!
      implicit none
!
      integer(kind = kint) :: j, l, m
!
!
      write(*,*) 'input num. of points'
      read(*,*) nth_g
!
      write(*,*) 'input truncation'
      read(*,*) ltr_g
!
      call s_cal_schmidt_poly_on_med
!
      open (50,file='schmidt_polynomials.dat')
      call check_gauss_colat_med
!
      do j = 1, jmax_g
        l = idx(j,1)
        m = abs( idx(j,2) )
        write(50,'(3i6,1pE25.15e3,1pe15.7)') j, idx(j,1),               &
    &        abs(idx(j,2)), dint_p(j), dint_p(j)*dble(2*l+1)
      end do
!
      call check_schmidt_poly_med(idx)
      close(50)
!
      stop
      end program test_schmidt_poly_med
