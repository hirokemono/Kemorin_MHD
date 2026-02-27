!>@file   initial_magne_dynamobench.f90
!!@brief  module initial_magne_dynamobench
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial magnetic field as dipole and toroidal
!!
!!@verbatim
!!      subroutine initial_magne_shell_dipole                           &
!!     &         (sph, l, m, kr_in, kr_out, r_in, r_out,                &
!!     &          n_point, d_rj_magne, d_rj_current)
!!      subroutine initial_magne_shell_toroidal                         &
!!     &         (sph, l, m, kr_in, kr_out, r_in,                       &
!!     &          n_point, d_rj_magne, d_rj_current)
!!        type(sph_grids), intent(in) :: sph
!!        integer(kind = kint), intent(in) :: l, m
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: r_in, r_out
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
!!        real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!!@endverbatim
!
      module initial_magne_sph_mhd
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_shell_dipole                             &
     &         (sph, l, m, kr_in, kr_out, r_in, r_out,                  &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real (kind = kreal) :: rr
      integer(kind = kint) :: inod, k, js
!
!!!!!     Y_{1}^{m} component of poloidal magnetic field
      js = find_local_sph_mode_address(sph, l, m)
      if (js .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = kr_in, kr_out
          inod = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!
          d_rj_magne(inod,1) = (five / eight)                           &
     &        * (-three * rr**3 + four * r_out * rr**2 - r_in**4/rr)
          d_rj_magne(inod,2) = (five / eight)                           &
     &        * (-dnine * rr**2 + eight * r_out * rr + r_in**4/rr**2)
!
          d_rj_current(inod,3) = (five*three / two) * rr
        end do
!$omp end parallel do
      end if
!
      end subroutine initial_magne_shell_dipole
!
!-----------------------------------------------------------------------
!
      subroutine initial_magne_shell_toroidal                           &
     &         (sph, l, m, kr_in, kr_out, r_in,                         &
     &          n_point, d_rj_magne, d_rj_current)
!
      use spherical_indices_picker
!
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: d_rj_magne(n_point,3)
      real(kind = kreal), intent(inout) :: d_rj_current(n_point,3)
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: inod, k, jt
!
!!!!!     Y_{2}^{m} component of toroidal magnetic field
      pi = four * atan(one)
      jt = find_local_sph_mode_address(sph, l, m)
      if (jt .gt. 0) then
!$omp parallel do private(k,inod,rr)
        do k = kr_in, kr_out
          inod = local_sph_data_address(sph, k, jt)
          rr = radius_1d_rj_r(sph, k)
          d_rj_magne(inod,3) =  (ten/three) * rr * sin(pi*(rr-r_in))
!
          d_rj_current(inod,1) = d_rj_magne(inod,3)
          d_rj_current(inod,2) = (ten / three) * sin(pi*(rr-r_in))      &
     &                  + (ten / three) * pi * rr * cos(pi*(rr-r_in))
        end do
!$omp end parallel do
      end if
!
      end subroutine initial_magne_shell_toroidal
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      end module initial_magne_sph_mhd
