!>@file   int_edge_norm_nod.f90
!!        module int_edge_norm_nod
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine s_int_edge_norm_nod(node, ele, edge, gauss, neib_z,  &
!!     &          zfil_param, dz_ele, g_int, sk_norm_n, d_norm_nod)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(gauss_points), intent(in) :: gauss
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(vert_commute_filter_param), intent(in) :: zfil_param
!!        real(kind = kreal), intent(in) :: dz_ele
!!        integer(kind = kint), intent(in) :: nfilter2_3, nfilter6_1
!!        type(gauss_integrations), intent(inout) :: g_int
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!        real(kind = kreal), intent(inout) :: sk_norm_n(0:nfilter6_1)
!!        real(kind = kreal), intent(inout)                             &
!!     &         :: d_norm_nod(1:node%numnod,1:nfilter2_3,0:nfilter2_3)
!!@endverbatim
!
      module int_edge_norm_nod
!
      use m_precision
!
      use t_geometry_data
      use t_edge_data
      use t_gauss_points
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine s_int_edge_norm_nod(node, ele, edge, gauss, neib_z,    &
     &          zfil_param, dz_ele, nfilter2_3, nfilter6_1,             &
     &          g_int, sk_norm_n, d_norm_nod)
!
      use m_constants
!
      use t_vert_commute_filter_param
      use t_neighbour_data_z
!
      use set_filter_moments
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(edge_data), intent(in) :: edge
      type(gauss_points), intent(in) :: gauss
      type(neighbour_data_z), intent(in) :: neib_z
      type(vert_commute_filter_param), intent(in) :: zfil_param
      real(kind = kreal), intent(in) :: dz_ele(ele%numele)
      integer(kind = kint), intent(in) :: nfilter2_3, nfilter6_1
!
      type(gauss_integrations), intent(inout) :: g_int
      real(kind = kreal), intent(inout) :: sk_norm_n(0:nfilter6_1)
      real(kind = kreal), intent(inout)                                 &
     &         :: d_norm_nod(1:node%numnod,1:nfilter2_3,0:nfilter2_3)
!
      integer(kind = kint_gl) :: inod0, jele
      integer(kind = kint) :: inod
      integer(kind = kint) :: kf, jnod1, jnod2
      integer(kind = kint) :: i, j, je, jj, j0
      real(kind = kreal) :: zz0, zz1, zz2, zs, ze
!
!
      do inod = 1, node%numnod
        inod0 = node%inod_global(inod)
        zz0 =   node%xx(inod0,3) 
        do je = 1, nfilter2_3 - 1
          j0 = je - neib_z%nneib_nod(inod0,1) - 1
          jele = inod0 + j0
          zs = dble(2*(j0)  )
          ze = dble(2*(j0+1))
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          zz1 = node%xx(jnod1,3)
          zz2 = node%xx(jnod2,3)
!
          call set_points_4_integration(zs, ze, gauss, g_int)
!
          do j = 1, 2
            jj = je + j - 1
!
            if(zfil_param%iflag_filter_z .eq. id_tophat) then
              call filter_moment_tophat(nfilter6_1, gauss%n_point,      &
     &            zfil_param%f_width_z, g_int%f_point, g_int%x_point)
            else if(zfil_param%iflag_filter_z .eq. id_Linear) then
              call filter_moment_linear(nfilter6_1, gauss%n_point,      &
     &            zfil_param%f_width_z, g_int%f_point, g_int%x_point)
            else
              call filter_moment_gaussian(nfilter6_1, gauss%n_point,    &
     &            zfil_param%f_width_z, g_int%f_point, g_int%x_point)
            end if
!
            do i = 1, gauss%n_point
              g_int%f_point(1,i)                                        &
     &           = half * dz_ele(jele) * g_int%f_point(1,i)             &
     &            * (one + (-1)**j * (g_int%x_point(i)-dble(2*j0+1)))
             do kf = 2, nfilter6_1+1
               g_int%f_point(kf,i) = half * g_int%f_point(kf-1,i)       &
     &            * ( zz2 + zz1 - two*zz0                               &
     &             + ( (zz2-zz1)*(g_int%x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call cal_gauss_integrals(gauss, g_int, sk_norm_n(0))
!
            do kf = 0, nfilter2_3
              d_norm_nod(inod,jj,kf) = d_norm_nod(inod,jj,kf)           &
     &                                + sk_norm_n(kf)
            end do
!
          end do
        end do
      end do
!
!
      end subroutine s_int_edge_norm_nod
!
!   --------------------------------------------------------------------
!
      end module int_edge_norm_nod
