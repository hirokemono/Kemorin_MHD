!int_edge_horiz_filter_peri.f90
!      module int_edge_horiz_filter_peri
!
!      Written by H. Matsui
!
!      subroutine int_edge_filter_peri(ndep_filter, numnod_h, hsize,    &
!     &     xmom_h, xmom_ht)
!
      module int_edge_horiz_filter_peri
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_filter_peri(ndep_filter, numnod_h, hsize,     &
     &     xmom_h, xmom_ht)
!
      use m_constants
      use m_commute_filter_z
      use m_z_filter_values
      use m_gauss_points
      use m_gauss_integration
      use m_int_edge_data
      use m_work_4_integration
      use set_filter_moments
!
      integer(kind = kint) :: ndep_filter, numnod_h
      real(kind = kreal) :: hsize
      real(kind = kreal), dimension(ndep_filter,0:2) :: xmom_h
      real(kind = kreal), dimension(0:3) :: xmom_ht
!
      integer (kind = kint) :: jnod, ie1, ie2, is, iee, kf
      integer (kind = kint) :: i, j, k, je, j0
!
      real(kind = kreal) :: zz1, zz2, zs, ze
      real(kind = kreal), dimension(n_point) :: filter_0
!
!
      write(*,*) 'iflag_filter_h', iflag_filter_h
      write(*,*) 'f_width_h', f_width_h
!
        do je = 1, ncomp_mat-1
          j0 = je - (ncomp_mat-1)/2 - 1
          zs = dble(2*(j0)  )
          ze = dble(2*(j0+1))
          zz1 = 2.0d0*dble(j0  ) * hsize
          zz2 = 2.0d0*dble(j0+1) * hsize
!          zz1 = dble(j0  ) * hsize / dble(numnod_h-1)
!          zz2 = dble(j0+1) * hsize / dble(numnod_h-1)
!
          call set_points_4_integration(zs, ze)
!
          do j = 1, 2
            jnod = je + j - 1
!
            if ( iflag_filter_h .eq. 0) then
              call filter_moment_tophat(izero, n_point, f_width_h,      &
     &            filter_0, x_point)
            else if (iflag_filter_h .eq. 1) then
              call filter_moment_linear(izero, n_point, f_width_h,      &
     &            filter_0, x_point)
            else
              call filter_moment_gaussian(izero, n_point, f_width_h,    &
     &            filter_0, x_point)
            end if
!
            do i = 1, n_point
             f_point(1,i) = half * filter_0(i)                          &
     &                 * ( one + (-1)**j * (x_point(i)-dble(2*j0+1)) ) 
             do kf = 2, nfilter6_1+1
              f_point(kf,i) = half * f_point(kf-1,i) * ( zz2 + zz1      &
     &           + ( (zz2-zz1)*(x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call gaussian_integration(sk_norm_n(0))
!
            do kf = 0, 2
              xmom_h(jnod,kf) = xmom_h(jnod,kf) + sk_norm_n(kf)
            end do
!
          end do
        end do
!
      do kf = 0, 2
        do jnod = 1, ncomp_mat
          xmom_ht(kf) = xmom_ht(kf) + xmom_h(jnod,kf)
        end do
      end do
!
      do kf = 0, 2
       do j = 1, ndep_filter
        xmom_h(jnod,kf) = xmom_h(jnod,kf) / xmom_ht(0)
       end do
      end do
      xmom_ht(2) = xmom_ht(2) / xmom_ht(0)
      xmom_ht(1) = xmom_ht(1) / xmom_ht(0)
      xmom_ht(0) = xmom_ht(0) / xmom_ht(0)
!      xmom_ht(3) = xmom_ht(2) * (dble(numnod_h-1) / hsize)**2
!
      end subroutine int_edge_filter_peri
!
!   --------------------------------------------------------------------
!
      end module int_edge_horiz_filter_peri
