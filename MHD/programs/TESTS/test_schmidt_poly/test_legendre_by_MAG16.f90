!
      program test_legendre_by_MAG16
!
      use m_precision
      use mag_pbar16
!
      implicit none
!
      integer(kind = kint) :: ltr, ntheta
!
      integer(kind = kint) :: i, j, l, m, lst, led
      integer(kind = kint) :: iend, istart, t_rate, t_max
!
!
      write(*,*) 'imput number of points'
      read(*,*) ntheta
      write(*,*) 'input range of point'
      read(*,*) lst, led
      write(*,*) 'input truncation'
      read(*,*) ltr
!
      call system_clock(istart)
      call alloc_mag_lag16(ntheta, ltr)
      call mag_gauss_point16(ntheta)
      call mag_lagendre16(ntheta, ltr)
      call system_clock(iend, t_rate, t_max)
!
      call ordering_mag_lag16(ntheta, ltr)
      write(*,*) 'Elapsed time:', (iend-istart) / dble(t_rate)
!
!
!      write(50,*) 'Gauss-Legendre colatitude by MAG'
!      do i = 1, ntheta
!        write(50,'(i5,1p2E25.15e3)') i, colat(i), gauss_w(i)
!      end do
!
      open(50,file='lagendre_mag16.dat')
      write(50,'(a)') 'j, l, m, i, theta, P_lm, dPdr_lm'
!      do l = 1, ltr
      l = ltr
        do m = 0, l
          j = l*(l+1)+m
          do i = lst, led
            write(50,'(4i16,1p3E25.15e3)') j, l, m, i,                  &
     &             colat(ntheta-i+1), p_mag(j,i), dp_mag(j,i)
          end do
        end do
!      end do
      close(50)
 !
      stop
      end program test_legendre_by_MAG16
