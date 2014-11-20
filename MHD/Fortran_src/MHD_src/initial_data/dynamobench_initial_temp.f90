!dynamobench_initial_temp.f90
!      module dynamobench_initial_temp
!
!      programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!      modified by H. Matsui on July, 2006
!
!      subroutine set_initial_temp(isig)
!
!
      module dynamobench_initial_temp
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temp (isig)
!
      use m_constants
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_node_phys_address
      use m_node_phys_data
      use m_physical_property
      use m_geometry_parameter
!
      integer ( kind = kint), intent(in) :: isig
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
      do inod = 1, numnod
       d_nod(inod,iphys%i_velo  ) = 0.0d0
       d_nod(inod,iphys%i_velo+1) = 0.0d0
       d_nod(inod,iphys%i_velo+2) = 0.0d0
       d_nod(inod,iphys%i_press ) = 0.0d0
      end do
!$omp end parallel do
!
!
!$omp parallel do
      do inod = 1, numnod
        if(radius(inod) .lt. depth_high_t)                              &
     &                 d_nod(inod,iphys%i_temp) = one
        if(radius(inod) .gt. depth_low_t)                               &
     &                 d_nod(inod,iphys%i_temp) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(inod, xr, sit, csp)
      do inum = 1, numnod_fluid
        inod = inod_fluid(inum)
!
        xr = two * radius(inod)                                         &
     &      - one * (depth_low_t+depth_high_t)                          &
     &       / (depth_low_t-depth_high_t)
        sit = sin( colatitude(inod) )
        csp = cos( real_m*longitude(inod) )
!
        d_nod(inod,iphys%i_temp) =                                      &
     &            - depth_high_t/(depth_low_t-depth_high_t)             &
     &             + (depth_high_t*depth_low_t)*one                     &
     &             / ( ( depth_low_t-depth_high_t )**2 * radius(inod) ) &
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
