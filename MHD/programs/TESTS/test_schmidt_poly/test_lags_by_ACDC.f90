!
      program test_lags_by_ACDC
!
      use ACDC_Legendre_Poly
      use ACDC_Theta_Derivatives
!
      implicit none
!
      integer(kind = 4) :: ltr, ntheta
!
      Integer, Allocatable :: m_vals(:)
!
      integer(kind = 4) :: i, j, l, m, lc, mc, lm, itime
      integer(kind = 4) :: iend, istart, t_rate, t_max
!
!
      write(*,*) 'imput number of points'
      read(*,*) ntheta
      write(*,*) 'input truncation'
      read(*,*) ltr
!
      call system_clock(istart)
!      m_vals(:) = m_values(my_mp%min:my_mp%max)
      
!      allocate(m_vals(1:tmp))
!      m_vals(:) = m_values(my_mp%min:my_mp%max)
!      Call Initialize_Legendre(n_theta,l_max,m_vals)
!      tmp = my_r%delta
!      Call Initialize_Theta_Derivatives(m_vals,l_max,tmp)
      deallocate(m_vals)
      call system_clock(iend, t_rate, t_max)
      write(*,*) 'Elapsed time:', (iend-istart) / dble(t_rate)
!
!
!      write(50,*) 'Gauss-Legendre colatitude by MAG'
!      do i = 1, ntheta
!        write(50,'(i5,1p2E25.15e3)') i, colat(i), gauss_w(i)
!      end do
!
!      open(50,file='lagendre_mag16.dat')
!      write(50,'(a)') 'j, l, m, i, theta, P_lm, dPdr_lm'
!      do l = 1, ltr
!      l = ltr
!        do m = 0, l
!          j = l*(l+1)+m
!          do i = 1, ntheta
!            write(50,'(4i10,1p3E25.15e3)') j, l, m, i,                  &
!     &             colat(ntheta-i+1), p_mag(j,i), dp_mag(j,i)
!          end do
!        end do
!      end do
!      close(50)
 !
      stop
      end program test_lags_by_ACDC
