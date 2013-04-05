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
      integer (kind = kint) :: i
!
   10 continue
!
      write(*,*) 'imput number of points (end: negative values)'
      read(*,*) n_point
!
      if (n_point.le.0) go to 999
!
      call allocate_gauss_points
!
      call construct_gauss_coefs
!
      write(*,*) 'gaussian points and coefficients'
      do i = 1, n_point
        write(*,'(i5,1p2E25.15e3)') i, w_point(i), w_coefs(i)
      end do
!
!
      call allocate_gauss_colatitude
!
      call set_gauss_colatitude
!
!
      write(*,*) 'gauss-Lagendre colatitude'
      do i = 1, n_point
        write(*,'(i5,1p3E25.15e3)') i,w_point(i),                       &
     &         w_colat(i),w_col_deg(i)
      end do
!
      write(*,*) 'Azimuth'
      do i = 1, 2*n_point
        write(*,'(i5,1p2E25.15e3)') i, w_azim(i), w_azim_deg(i)
      end do
!
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      go to 10
!
!
 999  continue
      stop
      end
