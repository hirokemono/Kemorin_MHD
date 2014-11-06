!dynamobench_r_func_sph_velo.f90
!      module dynamobench_r_func_sph_velo
!
!      Written by H. Matsui
!
!      subroutine radial_function_sph_velo(r)
!
      module dynamobench_r_func_sph_velo
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
!
      subroutine radial_function_sph_velo(r)
!
      use m_spherical_harmonics
!
      real (kind = kreal), intent(in) :: r
!
      real (kind = kreal) :: ri, pi
!
!
        pi = 4.0d0*atan(1.0d0)
        ri = 2.0/3.0
!
        vp = 0.0d0
        vt = 0.0d0
        dvp = 0.0d0
!
!
        vt(2) = (r-ri)**2 * (1.0d0 - (r-ri)**2 )                        &
     &     *sqrt(0.5/1.10123456) 
!
        vp(6) = (r-ri)**6 * (1.0d0 - (r-ri)**2 )**4                     &
     &     *sqrt(0.08/228.1742)
!
        vp(4) = (r-ri)**4 * (1.0d0 - (r-ri)**2 )**2                     &
     &     *sqrt(0.21/106.847) * sin( 3.0*pi*(r - ri ) )
!
        vp(8) = (r-ri)**4 * (1.0d0 - (r-ri)**2 )**2                     &
     &     *sqrt(0.21/106.847) * cos( 3.0*pi*(r - ri ) )
!
!
        dvp(4) = (r-ri)**3 * (1.0d0 - (r-ri)**2 )                       &
     &     * ( ( 4.0d0-8.0d0*(r-ri)**2 ) * sin( 3.0*pi*(r-ri))          &
     &        + 3.0d0 *pi* (r-ri) * (1.0d0 - (r-ri)**2 )                &
     &       * cos( 3.0*pi*(r-ri)) ) * sqrt(0.21/106.847)
!!
        dvp(8) =  (r-ri)**3 * (1.0d0 - (r-ri)**2 )                      &
     &     * ( ( 4.0d0-8.0d0*(r-ri)**2 ) * cos( 3.0*pi*(r-ri))          &
     &        - 3.0d0 *pi* (r-ri) * (1.0d0 - (r-ri)**2 )                &
     &       * sin( 3.0*pi*(r-ri)) ) * sqrt(0.21/106.847)
!!
        dvp(6) = 6.0d0 * (r-ri)**5 * (1.0d0 - (r-ri)**2 )**2            &
     &          * ( 1.0d0 - 2.0d0*(r-ri)**2 ) * sqrt(0.08/228.1742)
!
!
        end subroutine radial_function_sph_velo
!
!-----------------------------------------------------------------------
!
      end module dynamobench_r_func_sph_velo
