!>@file   int_edge_commute_z_filter.f90
!!        module int_edge_commute_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief edge integration for commutrative filter
!!
!!@verbatim
!!      subroutine copy_ref_vert_filter_moments(z_commute, xmom_ht_z)
!!        type(vart_filter_moments), intent(in) :: z_commute
!!        real(kind = kreal), intent(inout) :: xmom_ht_z(0:2)
!!      subroutine int_edge_commutative_filter(node, ele,               &
!!     &          ie_edge, dz_ele, gauss, zfil_param, neib_z2,          &
!!     &          nfilter6_1, sk_norm_n, g_int, ndep_filter, c_filter,  &
!!     &          xmom_int_org, xmom_int, xmom_int_t, xmom_int_to)
!!        integer(kind = kint), intent(in) :: ie_edge(ele%numele,2)
!!        real(kind = kreal), intent(in) :: dz_ele(ele%numele)
!!        type(gauss_points), intent(in) :: gauss
!!        type(vert_commute_filter_param), intent(in) :: zfil_param
!!        type(neighbour_data_z), intent(in) :: neib_z2
!!        integer(kind = kint), intent(in) :: nfilter6_1
!!        integer(kind = kint), intent(in) :: ndep_filter
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: c_filter(ndep_filter,node%numnod)
!!        real(kind = kreal), intent(inout) :: sk_norm_n(0:nfilter6_1)
!!        type(gauss_integrations), intent(inout) :: g_int
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: xmom_int_org(node%numnod,ndep_filter,0:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: xmom_int(node%numnod,ndep_filter,0:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: xmom_int_t(node%numnod,0:2)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: xmom_int_to(node%numnod,0:2)
!!@endverbatim
!
      module int_edge_commute_z_filter
!
      use m_precision
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
      subroutine copy_ref_vert_filter_moments(z_commute, xmom_ht_z)
!
      use t_commute_filter_z
!
      type(vart_filter_moments), intent(in) :: z_commute
      real(kind = kreal), intent(inout) :: xmom_ht_z(0:2)
!
      integer(kind = kint) :: i, j
!
!
      do i = 1, z_commute%ncomp_norm
        do j = 0, 2
          if(z_commute%kcomp_norm_z(i) .eq. j)                          &
     &                             xmom_ht_z(j) = z_commute%f_mom_z(i)
        end do
      end do
!
      end subroutine copy_ref_vert_filter_moments
!
!   --------------------------------------------------------------------
!
      subroutine int_edge_commutative_filter(node, ele,                 &
     &          ie_edge, dz_ele, gauss, zfil_param, neib_z2,            &
     &          nfilter6_1, sk_norm_n, g_int, ndep_filter, c_filter,    &
     &          xmom_int_org, xmom_int, xmom_int_t, xmom_int_to)
!
      use m_constants
!
      use t_geometry_data
      use t_vert_commute_filter_param
      use t_neighbour_data_z
      use set_filter_moments
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: ie_edge(ele%numele,2)
      real(kind = kreal), intent(in) :: dz_ele(ele%numele)
      type(gauss_points), intent(in) :: gauss
      type(vert_commute_filter_param), intent(in) :: zfil_param
      type(neighbour_data_z), intent(in) :: neib_z2
      integer(kind = kint), intent(in) :: nfilter6_1
      integer(kind = kint), intent(in) :: ndep_filter
      real(kind = kreal), intent(in)                                    &
     &                   :: c_filter(ndep_filter,node%numnod)
!
      type(gauss_integrations), intent(inout) :: g_int
      real(kind = kreal), intent(inout) :: sk_norm_n(0:nfilter6_1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: xmom_int_org(node%numnod,ndep_filter,0:2)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xmom_int(node%numnod,ndep_filter,0:2)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xmom_int_t(node%numnod,0:2)
      real(kind = kreal), intent(inout)                                 &
     &                   :: xmom_int_to(node%numnod,0:2)
!
      integer (kind = kint) :: inod, jnod, kf
      integer (kind = kint) :: i, j, je, jele
      integer (kind = kint) :: jnod1, jnod2, j0
!
      real(kind = kreal) :: zz0, zz1, zz2, zs, ze
      real(kind = kreal) :: filter_0(gauss%n_point)
!
!
      do inod = 1, node%numnod
        zz0 = node%xx(inod,3)
        do je = 1, zfil_param%ncomp_mat-1
          j0 = je - neib_z2%nneib_nod(inod,1) - 1
          jele = inod + j0
          zs = dble(2*(j0)  )
          ze = dble(2*(j0+1))
          jnod1 = ie_edge(jele,1)
          jnod2 = ie_edge(jele,2)
          zz1 = node%xx(jnod1,3)
          zz2 = node%xx(jnod2,3)
!
          call set_points_4_integration(zs, ze, gauss, g_int)
!
          do j = 1, 2
            jnod = je + j - 1
!
            if(zfil_param%iflag_filter_z .eq. id_tophat) then
              call filter_moment_tophat(izero, gauss%n_point,           &
     &            zfil_param%f_width_z, filter_0, g_int%x_point)
            else if(zfil_param%iflag_filter_z .eq. id_Linear) then
              call filter_moment_linear(izero, gauss%n_point,           &
     &            zfil_param%f_width_z, filter_0, g_int%x_point)
            else
              call filter_moment_gaussian(izero, gauss%n_point,         &
     &            zfil_param%f_width_z, filter_0, g_int%x_point)
            end if
!
            do i = 1, gauss%n_point
              g_int%f_point(1,i) = half * filter_0(i)                   &
     &             * (one + (-1)**j * (g_int%x_point(i)-dble(2*j0+1))) 
             do kf = 2, nfilter6_1+1
               g_int%f_point(kf,i) = half * g_int%f_point(kf-1,i)       &
     &             * ( zz2 + zz1 - two*zz0                              &
     &              + ( (zz2-zz1)*(g_int%x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call cal_gauss_integrals(gauss, g_int, sk_norm_n(0))
!
            do kf = 0, 2
              xmom_int_org(inod,jnod,kf) = xmom_int_org(inod,jnod,kf)   &
     &                         + sk_norm_n(kf)
            end do
!
!
            do i = 1, gauss%n_point
              g_int%f_point(1,i) = filter_0(i)  * quad * dz_ele(jele)   &
     &            * (one + (-1)**j * (g_int%x_point(i)-dble(2*j0+1)))   &
     &             * ( c_filter(je,inod)                                &
     &             * ( one - (g_int%x_point(i)-dble(2*j0+1)) )          &
     &              + c_filter(je+1,inod)                               &
     &             * ( one + (g_int%x_point(i)-dble(2*j0+1)) ) )
             do kf = 2, nfilter6_1+1
               g_int%f_point(kf,i) = half * g_int%f_point(kf-1,i)       &
     &            * ( zz2 + zz1 - two*zz0                               &
     &             + ( (zz2-zz1)*(g_int%x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call cal_gauss_integrals(gauss, g_int, sk_norm_n(0))
!
            do kf = 0, 2
              xmom_int(inod,jnod,kf) = xmom_int(inod,jnod,kf)           &
     &                         + sk_norm_n(kf)
            end do
!
          end do
        end do
      end do
!
!
      do kf = 0, 2
        do inod = 1, node%numnod
          do jnod = 1, zfil_param%ncomp_mat
            xmom_int_t(inod,kf) = xmom_int_t(inod,kf)                   &
     &                          + xmom_int(inod,jnod,kf)
            xmom_int_to(inod,kf) = xmom_int_to(inod,kf)                 &
     &                          + xmom_int_org(inod,jnod,kf)
          end do
        end do
      end do
!
      end subroutine int_edge_commutative_filter
!
!   --------------------------------------------------------------------
!
      end module int_edge_commute_z_filter
