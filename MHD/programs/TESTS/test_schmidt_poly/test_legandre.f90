!>@file   test_legandre.f90
!!@brief  program test_legandre
!!
!!@author H. Matsui
!!@date Programmed in 2008
!
!> @brief Test program for Legendre polynomials 
!!        with Schmidt quasi-normalization
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     program test_legandre
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      program test_legandre
!
      use mag_pbar
      use ispack_lag
!
      use m_spheric_parameter
      use m_schmidt_poly_on_rtm
      use m_gauss_points
      use m_schmidt_polynomial
!
      use t_spheric_rlm_data
!
      use schmidt_poly_on_rtm_grid
!
      implicit none
!
      integer(kind = 4) :: ltr, ntheta, num_gauss
!
      integer(kind = 4) :: i, j, l, m, lc, mc, lm, l_check
!
   10 continue
!
      write(*,*) 'Imput number of points (end: negative values)'
      read(*,*) num_gauss
!
      if (num_gauss.le.0) go to 999
!
      write(*,*) 'Imput truncation degree'
      read(*,*)  ltr
!
      nidx_rtm(2) = num_gauss
      sph_rlm1%nidx_rlm(2) = ltr*(ltr+2) + 1
      sph_param1%l_truncation = ltr
      nth = ltr
      call alloc_type_sph_1d_index_rlm(sph_rlm1)
      nidx_rlm(1:2) = sph_rlm1%nidx_rlm(1:2)
!
      do l = 0, ltr
        do m = -l, l
          j = l*(l+1) + m
          sph_rlm1%idx_gl_1d_rlm_j(j+1,1) = j
          sph_rlm1%idx_gl_1d_rlm_j(j+1,2) = l
          sph_rlm1%idx_gl_1d_rlm_j(j+1,3) = m
        end do
      end do
!
      call allocate_gauss_colat_rtm(sph_rtm1%nidx_rtm(2))
      call allocate_gauss_points(num_gauss)
      call construct_gauss_coefs
!
      call allocate_gauss_colatitude
      call set_gauss_colatitude
!
      call set_gauss_points_rtm
!
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      call allocate_schmidt_poly_rtm                                    &
     &   (sph_rtm%nidx_rtm(2), sph_rlm%nidx_rlm(2), sph_rj%nidx_rj(2))
      call set_lagender_4_rlm
!
!      do j = 1, nidx_rlm(2)
!        write(*,*) j, sph_rlm1%idx_gl_1d_rlm_j(j,1:3)
!      end do
!
      write(*,*) 'Gauss-Legendre colatitude'
      do i = 1, n_point
        write(*,'(i5,1p2E25.15e3)') i, g_colat_rtm(i), weight_rtm(i)
      end do
!
      ntheta = n_point
      call alloc_mag_lag(ntheta, ltr)
      call mag_gauss_point(ntheta)
!
      write(*,*) 'Gauss-Legendre colatitude by MAG'
      do i = 1, ntheta
        write(*,'(i5,1p2E25.15e3)') i, colat(i), gauss_w(i)
      end do
!
      write(*,*) 'difference'
      do i = 1, n_point
        write(*,'(i5,1p2E25.15e3)') i,                                  &
     &                              g_colat_rtm(i)-colat(n_point-i+1),  &
     &                              weight_rtm(i)-gauss_w(n_point-i+1)
      end do
!
!      colat(1:ntheta) =   g_colat_rtm(1:ntheta)
!      gauss_w(1:ntheta) = weight_rtm(1:ntheta)
      call mag_lagendre(ntheta, ltr)
      call norm_mag_lag(ntheta, ltr)
!
!
  88  continue
        write(*,*) 'input degree to check'
        read(*,*) l_check
        if (l_check .lt. 0) go to 77
!
        open(60,file='lagendre_Kemo.dat')
        write(60,'(a)') 'j, l, m, i, r, P_rtm, dPdt_rtm'
        do j = l_check*(l_check+1)+1, l_check*(l_check+2)+1
          if(sph_rlm1%idx_gl_1d_rlm_j(j,3) .ge. 0) then
            do i = 1, n_point
              write(60,'(4i5,1p3e23.14e3)')                             &
     &            sph_rlm1%idx_gl_1d_rlm_j(j,1:3), i,                   &
     &            g_colat_rtm(i), P_rtm(i,j), dPdt_rtm(i,j)
            end do
          end if
        end do
        close(60)
!
        open(50,file='lagendre_mag.dat')
        write(50,'(a)') 'j, l, m, i, r, aleg1, aleg3'
        l = l_check
          do m = 0, l
            j = l*(l+1)+m
            do i = 1, ntheta
              write(50,'(4i5,1p3e23.14e3)') j, l, m, i,                 &
     &             colat(ntheta-i+1), p_mag(j,i), dp_mag(j,i)
            end do
          end do
        close(50)
!
        go to 88
  77  continue
!
!      call SMINIT(ltr, (2*n_point), n_point, 1)
!
!      do l = 0, ltr
!        do m = 0, l
!          write(*,*) 'l,m', l, m, ltr-l+m,l, m, ltr-l
!          do i = 1, n_point/2
!            write(70,'(4i5,1p4E25.15e3)') j, l, m, i, Y_ispack(i,1:2), &
!     &          P_ispack(1,ltr-l+m,l,i), P_ispack(1,m,l-1,i)
!          end do
!        end do
!      end do
!
!      call SMFIN
      call dealloc_mag_lag
!
      call deallocate_schmidt_polynomial
      call deallocate_schmidt_poly_rtm
      call deallocate_gauss_colat_rtm
!
!      go to 10
!
!
 999  continue
!
      stop
      end program test_legandre
