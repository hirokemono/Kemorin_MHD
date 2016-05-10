!>@file   set_radius_func.f90
!!@brief  module set_radius_func
!!
!!@author H. Matsui
!!@date Programmed in June., 1994
!!@n    Modified in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine set_radius_rot_reft_dat_4_sph(r_hot, r_cold,         &
!!     &          temp_hot, temp_cold, rotate, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine const_2nd_fdm_matrices
!!      subroutine const_2nd_fdm_coefs
!!@endverbatim
!!
!!@n @param r_hot        radius at highest temperature point
!!@n @param r_cold       radius at lowest temperature point
!!@n @param temp_hot     temperature at highest temperature point
!!@n @param temp_cold    temperature at lowest temperature point
!!@n @param rotate(3)    rotation vector
!
      module set_radius_func
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_constants
      use m_spheric_parameter
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine set_radius_rot_reft_dat_4_sph(r_hot, r_cold,           &
     &          temp_hot, temp_cold, rotate, rj_fld)
!
      use m_poloidal_rotation
      use m_sph_phys_address
!
      use t_phys_data
!
      use set_radius_func_noequi
      use set_radius_4_sph_dynamo
      use set_reference_temp_sph
!
      real(kind = kreal), intent(in) :: r_hot, r_cold
      real(kind = kreal), intent(in) :: temp_hot, temp_cold
      real(kind = kreal), intent(in) :: rotate(3)
!
      type(phys_data), intent(inout) :: rj_fld
!
!* --------  radius  --------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo                                  &
     &   (sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r,                   &
     &    sph_param1%iflag_radial_grid, sph_param1%nlayer_ICB,          &
     &    sph_param1%nlayer_CMB, sph_param1%nlayer_2_center,            &
     &    sph_rj1%ar_1d_rj, sph_rj1%r_ele_rj, sph_rj1%ar_ele_rj,        &
     &    sph_param1%radius_ICB, sph_param1%radius_CMB,                 &
     &    sph_param1%R_earth)
!
!   Choose radial grid mode
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_dr_for_nonequi'
      call allocate_dr_rj_noequi(sph_rj1%nidx_rj(1))
      call set_dr_for_nonequi(sph_param1%nlayer_CMB,                    &
     &    sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r)
!
!*  ----------   reference of temperature --------
!*
      if (ipol%i_ref_t .gt. 0) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &               write(*,*) 'set_reftemp_4_sph'
        call set_reftemp_4_sph(sph_rj1%idx_rj_degree_zero,              &
     &      sph_rj1%nidx_rj, sph_rj1%ar_1d_rj,                          &
     &      sph_param1%nlayer_ICB, sph_param1%nlayer_CMB,               &
     &      r_hot, r_cold, temp_hot, temp_cold,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!*
!*  ----------  rotation of earth  ---------------
!*
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(sph_rlm1, sph_rj1, rotate)
!
      end subroutine set_radius_rot_reft_dat_4_sph
!
!  -------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_2nd_fdm_matrices
!
      use m_fdm_coefs
      use set_radius_func_noequi
!
!
      call allocate_fdm_matrices(sph_rj1%nidx_rj(1))
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi(sph_param1%nlayer_ICB,           &
     &    sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r)
      call deallocate_dr_rj_noequi
!
      end subroutine const_2nd_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_2nd_fdm_coefs
!
      use m_fdm_coefs
      use set_radius_func_noequi
!
!
      call allocate_fdm_coefs(sph_rj1%nidx_rj(1))
      call copy_fdm_nod_coefs_from_mat(sph_rj1%nidx_rj(1))
      call deallocate_fdm_matrices
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_2_coefs                                          &
     &     (sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r)
      end if
!
      end subroutine const_2nd_fdm_coefs
!
! -----------------------------------------------------------------------
!
      end module set_radius_func
