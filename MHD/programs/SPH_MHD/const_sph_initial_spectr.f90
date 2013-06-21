!>@file   const_sph_initial_spectr.f90
!!@brief  module const_sph_initial_spectr
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine sph_initial_spectrum
!!
!!       Sample program to generate initial field
!!       This program generates initial condition
!!        for dynamo benchmark case 1
!!@endverbatim
!
!
      module const_sph_initial_spectr
!
      use m_precision
!
      use m_constants
      use m_spheric_parameter
      use m_sph_phys_address
!
      implicit none
!
      private :: find_local_sph_mode_address
      private :: set_initial_velocity
      private :: set_initial_temperature
      private :: set_initial_composition
      private :: set_initial_magne_sph
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_spectrum
!
      use m_initial_field_control
      use m_t_int_parameter
      use m_t_step_parameter
      use m_field_data_IO
      use sph_mhd_rst_IO_control
      use set_sph_restart_IO
!
!
      i_step_MHD = i_step_init
      time   =     time_init
!
!  Set initial velocity if velocity is exist
      if(ipol%i_velo .gt. izero) call  set_initial_velocity
!
!  Set initial temperature if temperature is exist
      if(ipol%i_temp .gt. izero) call  set_initial_temperature
!
!  Set initial composition if composition is exist
      if(ipol%i_light .gt. izero) call set_initial_composition
!
!  Set initial magnetic field if magnetic field is exist
      if(ipol%i_magne .gt. izero) call set_initial_magne_sph
!
!
!  Copy initial field to restart IO data
      call set_sph_restart_num_to_IO
!
      call output_sph_restart_control
!
      end subroutine sph_initial_spectrum
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_velocity
!
      use m_sph_spectr_data
!
      integer ( kind = kint) :: inod
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_velo) = zero
        d_rj(inod,itor%i_velo) = zero
      end do
!$omp end parallel do
!
      end subroutine set_initial_velocity
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_temperature
!
      use m_sph_spectr_data
!
      integer ( kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
      real(kind = kreal), parameter :: A_temp = 0.1d0
!
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_temp) = zero
      end do
!$omp end parallel do
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!   set reference temperature (l = m = 0)
      jj = find_local_sph_mode_address(izero, izero)
!
      if (jj .gt. 0) then
        do k = 1, nidx_rj(1)
          inod = jj + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_temp) = (ar_1d_rj(k,1) * 20.d0/13.0d0        &
     &                              - 1.0d0 ) * 7.0d0 / 13.0d0
        end do
      end if
!
!
!   set initial temperature (l = m = 4)
      jj =  find_local_sph_mode_address(ifour, ifour)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
!
          d_rj(inod,ipol%i_temp) = (one-three*xr**2+three*xr**4-xr**6)  &
     &                            * A_temp * six / (sqrt(pi))
        end do
      end if
!
      end subroutine set_initial_temperature
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_composition
!
      use m_sph_spectr_data
!
      integer (kind = kint) :: inod, k, jj
      real (kind = kreal) :: pi, xr, shell
      real(kind = kreal), parameter :: A_light = 0.1d0
!
!
      pi = four * atan(one)
      shell = r_CMB - r_ICB
!
!$omp parallel do
      do inod = 1, nnod_rj
        d_rj(inod,ipol%i_light) = zero
      end do
!$omp end parallel do
!
!
!   set initial composition (l = m = 4)
      jj =  find_local_sph_mode_address(ifour, ifour)
!
      if (jj .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          xr = two * radius_1d_rj_r(k) - one * (r_CMB+r_ICB) / shell
          inod = jj + (k-1)*nidx_rj(2)
          d_rj(inod,ipol%i_light) = (one-three*xr**2+three*xr**4-xr**6) &
     &                       * A_light * six / (sqrt(pi))
        end do
      end if
!
      end subroutine set_initial_composition
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_magne_sph
!
      use m_control_params_sph_MHD
      use m_sph_spectr_data
!
      real (kind = kreal) :: pi, rr
      integer(kind = kint) :: is, it, k, js, jt
!
!
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj
        d_rj(is,ipol%i_magne) = zero
        d_rj(is,itor%i_magne) = zero
      end do
!$omp end parallel do
!
!
!   Set initial poloidal magnetic field
!
      js =  find_local_sph_mode_address(ione, izero)
!
      if (js .gt. 0) then
        do k = nlayer_ICB, nlayer_CMB
          is = js + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
          d_rj(is,ipol%i_magne) =  (five / two) * rr**2                 &
     &                       * (four*r_CMB - three*rr) / (r_CMB+three)
        end do
      end if
!
!   Set initial toroidal magnetic field
!
      jt =  find_local_sph_mode_address(itwo, izero)
!
      if (jt .gt. 0) then
        do k = 1, nlayer_CMB
          it = jt + (k-1)*nidx_rj(2)
          rr = radius_1d_rj_r(k)
!
          d_rj(it,itor%i_magne)                                         &
     &          =  (ten / three) * rr * sin(pi*rr/r_CMB)
          d_rj(it,ipol%i_current) =  d_rj(it,itor%i_magne)
          d_rj(it,idpdr%i_current)                                      &
     &          = (ten / three) * (sin(pi*rr/r_CMB)    &
     &                          + (pi/r_CMB) * rr * cos(pi*rr/r_CMB) )
        end do
      end if
!
      end subroutine set_initial_magne_sph
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer function find_local_sph_mode_address(l, m)
!
      integer(kind = kint), intent(in) :: l, m
!
      integer(kind = kint) :: j
!
!
      find_local_sph_mode_address = 0
      do j = 1, nidx_rj(2)
        if (   idx_gl_1d_rj_j(j,2) .eq. l                               &
     &   .and. idx_gl_1d_rj_j(j,3) .eq. m) then
          find_local_sph_mode_address = j
          return
        end if
      end do
!
      end function find_local_sph_mode_address
!
!-----------------------------------------------------------------------
!
      end module const_sph_initial_spectr
