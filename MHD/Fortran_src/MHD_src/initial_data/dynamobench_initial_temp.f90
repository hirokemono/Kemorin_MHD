!dynamobench_initial_temp.f90
!      module dynamobench_initial_temp
!
!      programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!      modified by H. Matsui on July, 2006
!
!!      subroutine set_initial_temp(isig, depth_top, depth_bottom,      &
!!     &          node, nnod_fl, inod_fluid, ncomp_nod,                 &
!!     &          i_velo, i_press, i_temp, d_nod)
!
!
      module dynamobench_initial_temp
!
      use m_precision
      use m_constants
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp(isig, depth_top, depth_bottom,        &
     &          node, nnod_fl, inod_fluid, ncomp_nod,                   &
     &          i_velo, i_press, i_temp, d_nod)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      real (kind = kreal), intent(in) :: depth_top, depth_bottom
      integer ( kind = kint), intent(in) :: isig
      integer (kind = kint), intent(in) :: nnod_fl
      integer (kind = kint), intent(in) :: inod_fluid(nnod_fl)
      integer (kind = kint), intent(in) :: ncomp_nod
      integer (kind = kint), intent(in) :: i_velo, i_press, i_temp
!
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
      integer ( kind = kint) :: inod, inum
      real (kind = kreal) :: real_m
      real (kind = kreal) :: pi, xr, sit, csp
!
!
      real_m = dble( mod(isig,ikilo) / icent )
      pi = four * atan(one)
!
!$omp parallel do
      do inod = 1, node%numnod
       d_nod(inod,i_velo  ) = 0.0d0
       d_nod(inod,i_velo+1) = 0.0d0
       d_nod(inod,i_velo+2) = 0.0d0
       d_nod(inod,i_press ) = 0.0d0
      end do
!$omp end parallel do
!
!
!$omp parallel do
      do inod = 1, node%numnod
        if(node%rr(inod) .lt. depth_bottom)                             &
     &                 d_nod(inod,i_temp) = one
        if(node%rr(inod) .gt. depth_top)                                &
     &                 d_nod(inod,i_temp) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(inod, xr, sit, csp)
      do inum = 1, nnod_fl
        inod = inod_fluid(inum)
!
        xr = two * node%rr(inod)                                        &
     &      - one * (depth_top+depth_bottom)                            &
     &       / (depth_top-depth_bottom)
        sit = sin( node%theta(inod) )
        csp = cos( real_m*node%phi(inod) )
!
        d_nod(inod,i_temp) =                                            &
     &            - depth_bottom/(depth_top-depth_bottom)               &
     &             + (depth_bottom*depth_top)*one                       &
     &           / ( ( depth_top-depth_bottom )**2 * node%rr(inod) )    &
     &            + 0.1d0 * ( one - 3.0d0*xr**2 + 3.0d0*xr**4           &
     &             - xr**6) * sit**4 * csp                              &
     &           * 2.10d2 / (sqrt( 1.792d4 *pi ))
      end do
!$omp end parallel do
!
      end subroutine set_initial_temp
!
!-----------------------------------------------------------------------
!
      end module dynamobench_initial_temp
