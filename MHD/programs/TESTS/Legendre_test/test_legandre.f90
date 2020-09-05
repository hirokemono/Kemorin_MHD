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
      use t_spheric_parameter
      use t_schmidt_poly_on_rtm
!
      use t_spheric_rlm_data
!
      use schmidt_poly_on_rtm_grid
!
      implicit none
!
      type(sph_shell_parameters), save :: sph_param_test
      type(legendre_4_sph_trans), save :: leg_t
!
      type(sph_rtm_grid), save :: sph_rtm_test
      type(sph_rlm_grid), save :: sph_rlm_test
      type(sph_rj_grid), save ::  sph_rj_test
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
      sph_rtm_test%nidx_rtm(2) = num_gauss
      sph_rlm_test%nidx_rlm(2) = ltr*(ltr+2) + 1
      sph_param_test%l_truncation = ltr
      call alloc_sph_1d_index_rlm(sph_rlm_test)
      nidx_rlm(1:2) = sph_rlm_test%nidx_rlm(1:2)
!
      do l = 0, ltr
        do m = -l, l
          j = l*(l+1) + m
          sph_rlm_test%idx_gl_1d_rlm_j(j+1,1) = j
          sph_rlm_test%idx_gl_1d_rlm_j(j+1,2) = l
          sph_rlm_test%idx_gl_1d_rlm_j(j+1,3) = m
        end do
      end do
!
      call set_gauss_points_rtm(sph_rtm_test%nidx_rtm(2), leg_t)
!
      call allocate_schmidt_poly_rtm(sph_rtm_test%nidx_rtm(2),          &
     &    sph_rlm_test%nidx_rlm(2), sph_rj_test%nidx_rj(2), leg_t)
      call set_lagender_4_rlm                                           &
     &   (sph_param_test%l_truncation, sph_rtm_test, sph_rlm_test,      &
     &    leg_t%g_colat_rtm, leg_t%P_rtm, leg_t%dPdt_rtm)
!
!      do j = 1, nidx_rlm(2)
!        write(*,*) j, sph_rlm_test%idx_gl_1d_rlm_j(j,1:3)
!      end do
!
      write(*,*) 'Gauss-Legendre colatitude'
      do i = 1, num_gauss
        write(*,'(i5,1p2E25.15e3)') i, g_colat_rtm(i), weight_rtm(i)
      end do
!
      ntheta = num_gauss
      call alloc_mag_lag(ntheta, ltr)
      call mag_gauss_point(ntheta)
!
      write(*,*) 'Gauss-Legendre colatitude by MAG'
      do i = 1, ntheta
        write(*,'(i5,1p2E25.15e3)') i, colat(i), gauss_w(i)
      end do
!
      write(*,*) 'difference'
      do i = 1, num_gauss
        write(*,'(i5,1p2E25.15e3)') i,                                  &
     &                       g_colat_rtm(i)-colat(num_gauss-i+1),       &
     &                       weight_rtm(i)-gauss_w(num_gauss-i+1)
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
          if(sph_rlm_test%idx_gl_1d_rlm_j(j,3) .ge. 0) then
            do i = 1, num_gauss
              write(60,'(4i5,1p3e23.14e3)')                             &
     &            sph_rlm_test%idx_gl_1d_rlm_j(j,1:3), i,               &
     &            g_colat_rtm(i), P_rtm(i,j), leg_t%dPdt_rtm(i,j)
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
!      call SMINIT(ltr, (2*num_gauss), num_gauss, 1)
!
!      do l = 0, ltr
!        do m = 0, l
!          write(*,*) 'l,m', l, m, ltr-l+m,l, m, ltr-l
!          do i = 1, num_gauss/2
!            write(70,'(4i5,1p4E25.15e3)') j, l, m, i, Y_ispack(i,1:2), &
!     &          P_ispack(1,ltr-l+m,l,i), P_ispack(1,m,l-1,i)
!          end do
!        end do
!      end do
!
!      call SMFIN
      call dealloc_mag_lag
!
      call dealloc_schmidt_poly_rtm(leg_t)
      call dealloc_gauss_colat_rtm(leg_t)
!
!      go to 10
!
!
 999  continue
!
      stop
      end program test_legandre
