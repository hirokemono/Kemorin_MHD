!>@file   initial_field_example_2.f90
!!@brief  module initial_field_example_2
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial data for spectrum dynamos
!!
!!@verbatim
!!      subroutine init_thermal_stratification                          &
!!     &         (sph_bc_T, sph, ipol, rj_fld)
!!      subroutine set_initial_dipole_magne                             &
!!     &         (sph_bc_B, sph, ipol, rj_fld)
!!      subroutine set_initial_homogeneous_magne                        &
!!     &         (sph_bc_B, sph, ipol, rj_fld)
!!      subroutine add_inner_core_heat_sce_flatT                          &
!!     &         (sph_bc_T, bcs_T, sph, ipol, rj_fld)
!!       Poloidal magnetic field :: d_rj(:,ipol%base%i_magne  )
!!       Toroidal magnetic field :: d_rj(:,ipol%base%i_magne+2)
!!@endverbatim
!
!
      module initial_field_example_2
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_MHD_file_parameter
      use t_IO_step_parameter
      use t_time_data
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
      use t_radial_reference_field
      use t_field_data_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine init_thermal_stratification                            &
     &         (sph_bc_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      integer :: jj, k
      integer ( kind = kint) :: inod, i_center
      real (kind = kreal) :: pi, rr, xr, shell
      real(kind = kreal), parameter :: A_temp = 0.1d0
!
      real(kind = kreal), parameter :: real_OC =   2270
      real(kind = kreal), parameter :: real_strat = 100
      real(kind= kreal) :: r_from_base, r_strat
!
!
      if(ipol%base%i_temp .eq. izero) return
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%base%i_temp) = zero
      end do
!$omp end parallel do
!
      pi = four * atan(one)
      shell = sph_bc_T%r_CMB(0) - sph_bc_T%r_ICB(0)
!
!   search address for (l = m = 0)
      jj = find_local_sph_mode_address(sph, 0, 0)
!
      r_strat = (sph_bc_T%r_CMB(0) - sph_bc_T%r_ICB(0))                 &
     &         * (real_strat / real_OC)
!   set reference temperature if (l = m = 0) mode is there
      if (jj .gt. 0) then
        do k = 1, sph_bc_T%kr_in-1
          inod = local_sph_data_address(sph, k, jj)
          rj_fld%d_fld(inod,ipol%base%i_temp) = 0.0d0
        end do
        do k = sph_bc_T%kr_in, sph_bc_T%kr_out
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!
          r_from_base = rr - (sph_bc_T%r_CMB(0) - r_strat)
          if(rr .gt. (sph_bc_T%r_CMB(0) - r_strat)) then
            rj_fld%d_fld(inod,ipol%base%i_temp) = r_from_base / r_strat
          else
            rj_fld%d_fld(inod,ipol%base%i_temp) = 0.0d0
          end if
        end do
      end if
!
      end subroutine init_thermal_stratification
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_initial_dipole_magne                               &
     &         (sph_bc_B, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal), parameter :: Bnrm = 1.0d0
!
      real (kind = kreal) :: pi, rr, b_ref
      integer(kind = kint) :: is, it, is_ICB, is_CMB
      integer :: js, jt, k
!
!
      if(ipol%base%i_magne .eq. izero) return
      pi = four * atan(one)
!
!$omp parallel do
      do is = 1, nnod_rj(sph)
        rj_fld%d_fld(is,ipol%base%i_magne  ) = zero
        rj_fld%d_fld(is,ipol%base%i_magne+1) = zero
        rj_fld%d_fld(is,ipol%base%i_magne+2) = zero
      end do
!$omp end parallel do
!
!
!    Find local addrtess for (l,m) = (1,0)
      js =  find_local_sph_mode_address(sph, 1, 0)
!
      if (js .gt. 0) then
        do k = sph_bc_B%kr_in, sph_bc_B%kr_out
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%base%i_magne)                            &
     &                           =  (5.0d0/8.0d0) * (-3.0d0 * rr**3     &
     &                             + 4.0d0 * sph_bc_B%r_CMB(0) * rr**2  &
     &                             - sph_bc_B%r_ICB(0)**4 / rr)
        end do
!
        is = local_sph_data_address(sph, sph_bc_B%kr_out, js)
        rr = radius_1d_rj_r(sph, sph_bc_B%kr_out)
        b_ref = rj_fld%d_fld(is,ipol%base%i_magne)
        do k = sph_bc_B%kr_in, sph_bc_B%kr_out
          is = local_sph_data_address(sph, k, js)
          rj_fld%d_fld(is,ipol%base%i_magne)                            &
     &       = rj_fld%d_fld(is,ipol%base%i_magne)                       &
     &        * rr**2 * Bnrm / rj_fld%d_fld(is,ipol%base%i_magne)
        end do
!
!   Fill potential field if inner core exist
        is_ICB = local_sph_data_address(sph, int(sph_bc_B%kr_in), js)
        do k = 1, sph_bc_B%kr_in-1
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k) / sph_bc_B%r_ICB(0)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%base%i_magne)                            &
     &       =  rj_fld%d_fld(is_ICB,ipol%base%i_magne) * rr**(ione+1)
        end do
!
!   Fill potential field if external of the core exist
        is_CMB = local_sph_data_address(sph, int(sph_bc_B%kr_out), js)
        do k = sph_bc_B%kr_out+1, nidx_rj(sph,1)
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k) / sph_bc_B%r_CMB(0)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%base%i_magne)                            &
     &       =  rj_fld%d_fld(is_CMB,ipol%base%i_magne) * rr**(-ione)
        end do
      end if
!
      end subroutine set_initial_dipole_magne
!
!-----------------------------------------------------------------------
!
      subroutine set_initial_homogeneous_magne                          &
     &         (sph_bc_B, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_B
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: pi, rr, mag
      integer(kind = kint) :: is, it, is_ICB, is_CMB
      integer :: js, jt, k
!
!
      if(ipol%base%i_magne .eq. izero) return
      pi = four * atan(one)
      mag = 1.0d0
!
!$omp parallel do
      do is = 1, nnod_rj(sph)
        rj_fld%d_fld(is,ipol%base%i_magne  ) = zero
        rj_fld%d_fld(is,ipol%base%i_magne+1) = zero
        rj_fld%d_fld(is,ipol%base%i_magne+2) = zero
      end do
!$omp end parallel do
!
!
!    Find local addrtess for (l,m) = (1,0)
      js =  find_local_sph_mode_address(sph, 1, 0)
!
      if (js .gt. 0) then
        do k = 1, nidx_rj(sph,1)
          is = local_sph_data_address(sph, k, js)
          rr = radius_1d_rj_r(sph, k)
!   Substitute poloidal mangetic field
          rj_fld%d_fld(is,ipol%base%i_magne  ) = mag * rr**2
          rj_fld%d_fld(is,ipol%base%i_magne+1) = two * mag * rr
        end do
      end if
!
      end subroutine set_initial_homogeneous_magne
!
!-----------------------------------------------------------------------
!
      subroutine add_inner_core_heat_sce_flatT                          &
     &         (sph_bc_T, bcs_T, sph, ipol, rj_fld)
!
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
      real (kind = kreal) :: rr, q, T_ICB, f_ICB
      integer(kind = kint) :: inod, i_center
      integer :: jj, k
!
!
      if(ipol%base%i_heat_source * ipol%base%i_temp .eq. izero) return
!
!
!$omp parallel do
      do inod = 1, nnod_rj(sph)
        rj_fld%d_fld(inod,ipol%base%i_heat_source) = zero
      end do
!$omp end parallel do
!
!
!    Find address for l = m = 0
      jj =  find_local_sph_mode_address(sph, 0, 0)
!
      if (jj .gt. 0) then
        f_ICB = -bcs_T%CMB_Sspec%S_BC(jj)                               &
     &         * (sph_bc_T%r_CMB(0) / r_ICB(sph))**2
        q = three * f_ICB / r_ICB(sph)
        write(*,*) 'Set inner core heat source and temperature', f_ICB, q
!
        inod = local_sph_data_address(sph, nlayer_ICB(sph), jj)
        T_ICB = rj_fld%d_fld(inod,ipol%base%i_temp)
        write(*,*) 'ICB temp ', nlayer_ICB(sph), T_ICB, r_ICB(sph)
!
        do k = 1, nlayer_ICB(sph)
          inod = local_sph_data_address(sph, k, jj)
          rr = radius_1d_rj_r(sph, k)
!   Substitute initial heat source
          rj_fld%d_fld(inod,ipol%base%i_heat_source) = q
!   Fill inner core temperature
          rj_fld%d_fld(inod,ipol%base%i_temp) = T_ICB
          write(*,*) 'inner core temp ', k, rr, &
     &               rj_fld%d_fld(inod,ipol%base%i_heat_source),  &
     &               rj_fld%d_fld(inod,ipol%base%i_temp)
        end do
      end if
!
!    Center
      i_center = inod_rj_center(sph)
      if(i_center .gt. 0) then
        rj_fld%d_fld(i_center,ipol%base%i_heat_source) = q
        rj_fld%d_fld(i_center,ipol%base%i_temp)                         &
     &         = T_ICB
          write(*,*) 'center core temp ',  &
     &               rj_fld%d_fld(i_center,ipol%base%i_heat_source),  &
     &               rj_fld%d_fld(i_center,ipol%base%i_temp)
      end if
!
      end subroutine add_inner_core_heat_sce_flatT
!
!-----------------------------------------------------------------------
!
      end module initial_field_example_2
