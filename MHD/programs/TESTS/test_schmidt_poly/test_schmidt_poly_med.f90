!
!      program test_schmidt_poly_med
!
      program test_schmidt_poly_med
!
!     Written by H. Matsui on June, 2007
!
      use t_schmidt_poly_on_gauss
      use m_spherical_harmonics
      use schmidt_poly_on_meridian
!
      implicit none
!
      type(gauss_legendre_data), save :: leg_t
!
      integer(kind = kint) :: j, l, m, i, itime
      integer(kind = kint) :: iend, istart, t_rate, t_max, lst, led
!
!
      write(*,*) 'input num. of points'
      read(*,*) leg_t%nth_g
!
      write(*,*) 'input range of point'
      read(*,*) lst, led
!
      write(*,*) 'input truncation'
      read(*,*) leg_t%ltr_g
!
      call system_clock(istart)
      call cal_full_legendre_on_med(lst, led, leg_t)
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
      l = leg_t%ltr_g
      do m = 0, l
        j = l*(l+1) + m
        do i = lst, led
          write(50,'(4i16,1p4E25.15e3)') j, l, m, i,                    &
     &      leg_t%g_colat_med(i), leg_t%P_org(i,j),                     &
     &      leg_t%P_smdt(i,j), leg_t%dPdt_smdt(i,j)
        end do
      end do
      close(50)
!
      stop
      end program test_schmidt_poly_med
