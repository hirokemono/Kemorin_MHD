!>@file   t_normal_nod_for_z_filter.f90
!!        module t_normal_nod_for_z_filter
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief FEM shape functgions for vertical filter
!!
!!@verbatim
!!      subroutine alloc_normal_nod_z_filter                            &
!!     &         (node, nfilter2_3, numfilter, nrm_z_fil)
!!        subroutine dealloc_normal_nod_z_filter(nrm_z_fil)
!!        type(node_data), intent(in) :: node
!!        integer(kind = kint), intent(in) :: nfilter2_3, numfilter
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!
!!      subroutine check_nod_normalize_matrix(id_rank, node, nrm_z_fil)
!!        integer, intent(in) :: id_rank
!!        type(node_data), intent(in) :: node
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!
!!      subroutine int_edge_norm_nod(node, ele, edge, gauss, neib_z,    &
!!     &          zfil_param, dz_ele, g_int, sk_norm_n, d_norm_nod)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(gauss_points), intent(in) :: gauss
!!        type(neighbour_data_z), intent(in) :: neib_z
!!        type(vert_commute_filter_param), intent(in) :: zfil_param
!!        real(kind = kreal), intent(in) :: dz_ele
!!        type(gauss_integrations), intent(inout) :: g_int
!!        type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!!@endverbatim
!
      module t_normal_nod_for_z_filter
!
      use m_precision
!
      use t_geometry_data
      use t_edge_data
      use t_gauss_points
!
      implicit none
!
      type normal_nod_for_z_filter
        integer(kind = kint) :: nfilter6_1 = 0
        real(kind = kreal), allocatable:: sk_norm_n(:)
!
        integer(kind = kint) :: nfilter2_3 = 0
        real(kind = kreal), allocatable:: d_norm_nod(:,:,:)
      end type normal_nod_for_z_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_normal_nod_z_filter                              &
     &         (node, nfilter2_3, numfilter, nrm_z_fil)
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: nfilter2_3, numfilter
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
!
      nrm_z_fil%nfilter2_3 = nfilter2_3
      nrm_z_fil%nfilter6_1 = 6*numfilter + 1
!
      allocate(nrm_z_fil%sk_norm_n(0:nrm_z_fil%nfilter6_1))
      nrm_z_fil%sk_norm_n(0:nrm_z_fil%nfilter6_1) = 0.0d0
!
      allocate(nrm_z_fil%d_norm_nod(node%numnod,                        &
     &                              nfilter2_3,0:nfilter2_3))
      nrm_z_fil%d_norm_nod(1:node%numnod,1:nfilter2_3,0:nfilter2_3)     &
     &                                                         = 0.0d0
!
      end subroutine alloc_normal_nod_z_filter
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_normal_nod_z_filter(nrm_z_fil)
!
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
      deallocate(nrm_z_fil%sk_norm_n)
      deallocate(nrm_z_fil%d_norm_nod)
!
      end subroutine dealloc_normal_nod_z_filter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine check_nod_normalize_matrix(id_rank, node, nrm_z_fil)
!
      use m_commute_filter_z
!
      integer, intent(in) :: id_rank
      type(node_data), intent(in) :: node
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
!
      integer(kind = kint) :: i, k
!
!
      do k = 0, nrm_z_fil%nfilter2_3
        do i = 1, node%numnod
        write(id_rank+60,*) 'd_norm_nod (node_id,order) = ', i, k
        write(id_rank+60,'(1p5e16.8)')                                  &
     &          nrm_z_fil%d_norm_nod(i,1:nrm_z_fil%nfilter2_3,k)
        end do
      end do
!
      end subroutine check_nod_normalize_matrix
!
! -----------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine int_edge_norm_nod(node, ele, edge, gauss, neib_z,      &
     &          zfil_param, dz_ele, g_int, nrm_z_fil)
!
      use m_constants
      use m_commute_filter_z
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
!
      type(gauss_integrations), intent(inout) :: g_int
      type(normal_nod_for_z_filter), intent(inout) :: nrm_z_fil
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
        do je = 1, nrm_z_fil%nfilter2_3 - 1
          j0 = je - neib_z%nneib_nod(inod0,1) - 1
          jele = inod0 + j0
          zs = dble(2*(j0)  )
          ze = dble(2*(j0+1))
          jnod1 = edge%ie_edge(jele,1)
          jnod2 = edge%ie_edge(jele,2)
          zz1 = node%xx(jnod1,3)
          zz2 = node%xx(jnod2,3)
!
!
          call set_points_4_integration(zs, ze, gauss, g_int)
!
          do j = 1, 2
            jj = je + j - 1
!
            if ( iflag_filter .eq. 0) then
              call filter_moment_tophat                                 &
     &           (nrm_z_fil%nfilter6_1, gauss%n_point,                  &
     &            f_width, g_int%f_point, g_int%x_point)
            else if (iflag_filter .eq. 1) then
              call filter_moment_linear                                 &
     &           (nrm_z_fil%nfilter6_1, gauss%n_point,                  &
     &            f_width, g_int%f_point, g_int%x_point)
            else
              call filter_moment_gaussian                               &
     &           (nrm_z_fil%nfilter6_1, gauss%n_point,                  &
     &            f_width, g_int%f_point, g_int%x_point)
            end if
!
            do i = 1, gauss%n_point
              g_int%f_point(1,i)                                        &
     &           = half * dz_ele(jele) * g_int%f_point(1,i)             &
     &            * (one + (-1)**j * (g_int%x_point(i)-dble(2*j0+1)))
             do kf = 2, nrm_z_fil%nfilter6_1+1
               g_int%f_point(kf,i) = half * g_int%f_point(kf-1,i)       &
     &            * ( zz2 + zz1 - two*zz0                               &
     &             + ( (zz2-zz1)*(g_int%x_point(i)-dble(2*j0+1)) ))
             end do
            end do
!
            call cal_gauss_integrals(gauss, g_int,                      & 
     &                               nrm_z_fil%sk_norm_n(0))
!
            do kf = 0, nrm_z_fil%nfilter2_3
             nrm_z_fil%d_norm_nod(inod,jj,kf)                           &
     &            = nrm_z_fil%d_norm_nod(inod,jj,kf)                    &
     &             + nrm_z_fil%sk_norm_n(kf)
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
      end module t_normal_nod_for_z_filter
