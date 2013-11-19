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
      integer(kind = kint) :: j, l, m, i, itime
      integer(kind = 4) :: iend, istart, t_rate, t_max, lst, led
!
!
      write(*,*) 'input num. of points'
      read(*,*) nth_g
!
      write(*,*) 'input range of point'
      read(*,*) lst, led
!
      write(*,*) 'input truncation'
      read(*,*) ltr_g
!
      call system_clock(istart)
      call cal_full_legendre_on_med(lst, led)
      call system_clock(iend, t_rate, t_max)
      write(*,*) 'Elapsed time:', (iend-istart) / dble(t_rate)
!
!      call check_gauss_colat_med
!
!      do j = 1, jmax_g
!        l = idx(j,1)
!        m = abs( idx(j,2) )
!        write(50,'(3i6,1pE25.15e3,1pe15.7)') j, idx(j,1),              &
!    &        abs(idx(j,2)), dint_p(j), dint_p(j)*dble(2*l+1)
!      end do
!
      open (50,file='schmidt_polynomials.dat')
      write(50,'(a)') 'j, l, m, i, theta, P_lm, dPdr_lm'
      l = ltr_g
      do m = 0, l
        j = l*(l+1) + m
        do i = lst, led
          write(50,'(4i10,1p3E25.15e3)') j, l, m, i,                    &
     &      g_colat_med(i), P_smdt(i,j), dPdt_smdt(i,j)
        end do
      end do
      close(50)
!
      stop
      end program test_schmidt_poly_med
