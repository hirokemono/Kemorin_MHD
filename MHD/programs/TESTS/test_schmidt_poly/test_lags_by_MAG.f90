!
      program test_lags_by_MAG
!
      use mag_pbar
!
      implicit none
!
      integer(kind = 4) :: ltr, ntheta, lst, led
      integer(kind = 4) :: i, j, l, m, lc, mc, lm
      integer(kind = 4) :: iend, istart, t_rate, t_max
!
      write(*,*) 'imput number of points'
      read(*,*) ntheta
      write(*,*) 'input range of point'
      read(*,*) lst, led
      write(*,*) 'input truncation'
      read(*,*) ltr
!
      call system_clock(istart)
      call alloc_mag_lag(ntheta, ltr)
      call mag_gauss_point(ntheta)
      call mag_lagendre(ntheta, ltr)
      call system_clock(iend, t_rate, t_max)
!
      call ordering_mag_lag(ntheta, ltr)
      write(*,*) 'Elapsed time:', (iend-istart) / dble(t_rate)
!
!
!      write(50,*) 'Gauss-Legendre colatitude by MAG'
!      do i = 1, ntheta
!        write(50,'(i5,1p2E25.15e3)') i, colat(i), gauss_w(i)
!      end do
!
      open(50,file='lagendre_mag.dat')
      write(50,'(a)') 'j, l, m, i, theta, P_lm, dPdr_lm'
!      do l = 1, ltr
      l = ltr
        do m = 0, l
          j = l*(l+1)+m
          do i = lst, led
            write(50,'(4i10,1p3E25.15e3)') j, l, m, i,                  &
     &             colat(ntheta-i+1), p_mag(j,i), dp_mag(j,i)
          end do
        end do
!      end do
      close(50)
!
      stop
      end program test_lags_by_MAG
