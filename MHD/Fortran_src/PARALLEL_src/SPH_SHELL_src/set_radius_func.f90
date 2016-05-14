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
!!     &          temp_hot, temp_cold, rotate, sph_rlm, sph_rj,         &
!!     &          sph_params, rj_fld)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(phys_data), intent(inout) :: rj_fld
!!      subroutine const_2nd_fdm_matrices
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
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
!
      use t_spheric_parameter
      use t_phys_data
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
     &          temp_hot, temp_cold, rotate, sph_rlm, sph_rj,           &
     &          sph_params, rj_fld)
!
      use m_poloidal_rotation
      use m_sph_phys_address
!
      use set_radius_func_noequi
      use set_radius_4_sph_dynamo
      use set_reference_temp_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_rlm_grid), intent(in) :: sph_rlm
!
      real(kind = kreal), intent(in) :: r_hot, r_cold
      real(kind = kreal), intent(in) :: temp_hot, temp_cold
      real(kind = kreal), intent(in) :: rotate(3)
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(phys_data), intent(inout) :: rj_fld
!
!* --------  radius  --------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_radius_dat_4_sph_dynamo'
      call set_radius_dat_4_sph_dynamo                                  &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    sph_params%iflag_radial_grid, sph_params%nlayer_ICB,          &
     &    sph_params%nlayer_CMB, sph_params%nlayer_2_center,            &
     &    sph_rj%ar_1d_rj, sph_rj%r_ele_rj, sph_rj%ar_ele_rj,           &
     &    sph_params%radius_ICB, sph_params%radius_CMB,                 &
     &    sph_params%R_earth)
!
!   Choose radial grid mode
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_dr_for_nonequi'
      call allocate_dr_rj_noequi(sph_rj%nidx_rj(1))
      call set_dr_for_nonequi(sph_params%nlayer_CMB,                    &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
!
!*  ----------   reference of temperature --------
!*
      if (ipol%i_ref_t .gt. 0) then
        if (iflag_debug .ge. iflag_routine_msg)                         &
     &               write(*,*) 'set_reftemp_4_sph'
        call set_reftemp_4_sph(sph_rj%idx_rj_degree_zero,               &
     &      sph_rj%nidx_rj, sph_rj%ar_1d_rj,                            &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB,               &
     &      r_hot, r_cold, temp_hot, temp_cold,                         &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!*
!*  ----------  rotation of earth  ---------------
!*
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &                write(*,*) 'set_rot_earth_4_sph'
      call set_rot_earth_4_sph(sph_rlm, sph_rj, rotate)
!
      end subroutine set_radius_rot_reft_dat_4_sph
!
!  -------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_2nd_fdm_matrices(sph_params, sph_rj)
!
      use m_fdm_coefs
      use set_radius_func_noequi
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
!
      call allocate_fdm_matrices(sph_rj%nidx_rj(1))
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi(sph_params%nlayer_ICB,           &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
      call deallocate_dr_rj_noequi
!
!
      call allocate_fdm_coefs(sph_rj%nidx_rj(1))
      call copy_fdm_nod_coefs_from_mat(sph_rj%nidx_rj(1))
      call deallocate_fdm_matrices
!
      if(iflag_debug .eq. iflag_full_msg) then
        call check_fdm_2_coefs                                          &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
      end if
!
      end subroutine const_2nd_fdm_matrices
!
! -----------------------------------------------------------------------
!
      end module set_radius_func
