!int_edge_norm_nod_z_filter.f90
!      module int_edge_norm_nod_z_filter
!
      module int_edge_norm_nod_z_filter
!
!      Written by H. Matsui
!
      use m_precision
!
      implicit none
!
!      subroutine int_edge_norm_nod
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_norm_nod
!
      use m_constants
      use m_geometry_data
      use m_commute_filter_z
      use m_work_4_integration
      use m_z_filter_values
      use m_matrix_4_z_commute
      use m_neibor_data_z
      use m_gauss_points
      use m_gauss_integration
      use m_int_edge_data
      use set_filter_moments
!
      integer(kind = kint_gl) :: inod0, jele
      integer(kind = kint) :: inod
      integer(kind = kint) :: kf, jnod1, jnod2
      integer(kind = kint) :: i, j, je, jj, j0
      real(kind = kreal) :: zz0, zz1, zz2, zs, ze
!
!
      do inod = 1, node1%numnod
        inod0 = node1%inod_global(inod)
        zz0 =   node1%xx(inod0,3) 
        do je = 1, nfilter2_3 - 1
          j0 = je - nneib_nod(inod0,1) - 1
          jele = inod0 + j0
          zs = dble(2*(j0)  )
          ze = dble(2*(j0+1))
          jnod1 = edge1%ie_edge(jele,1)
          jnod2 = edge1%ie_edge(jele,2)
          zz1 = node1%xx(jnod1,3)
          zz2 = node1%xx(jnod2,3)
!
!
          call set_points_4_integration(zs, ze)
!
          do j = 1, 2
            jj = je + j - 1
!
            if ( iflag_filter .eq. 0) then
              call filter_moment_tophat(nfilter6_1, n_point, f_width,   &
     &            f_point, x_point)
            else if (iflag_filter .eq. 1) then
              call filter_moment_linear(nfilter6_1, n_point, f_width,   &
     &            f_point, x_point)
            else
              call filter_moment_gaussian(nfilter6_1, n_point, f_width, &
     &            f_point, x_point)
            end if
!
            do i = 1, n_point
             f_point(1,i) = half * dz(jele)  * f_point(1,i)             &
     &                 * ( one + (-1)**j * (x_point(i)-dble(2*j0+1)) ) 
             do kf = 2, nfilter6_1+1
              f_point(kf,i) = half * f_point(kf-1,i) * ( zz2 + zz1      &
     &           - two*zz0 + ( (zz2-zz1)*(x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call gaussian_integration(sk_norm_n(0))
!
            do kf = 0, nfilter2_3
             d_norm_nod(inod,jj,kf) = d_norm_nod(inod,jj,kf)            &
     &                               + sk_norm_n(kf)
            end do
!
          end do
        end do
      end do
!
!
      end subroutine int_edge_norm_nod
!
!   --------------------------------------------------------------------
!
      end module int_edge_norm_nod_z_filter
