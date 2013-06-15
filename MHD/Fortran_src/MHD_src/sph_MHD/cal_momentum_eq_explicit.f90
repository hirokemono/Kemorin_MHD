!cal_momentum_eq_explicit.f90
!      module cal_momentum_eq_explicit
!
!      Written by H. Matsui on Ocrt. 2009
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use m_control_parameter
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_momentum_eq_exp_sph
!
      use m_control_params_sph_MHD
      use cal_explicit_terms
      use m_parallel_var_dof
      use cal_sph_field_by_rotation
      use cal_nonlinear_sph_MHD
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2
!
      call cal_rot_of_induction_sph
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph
!
      end subroutine s_cal_momentum_eq_exp_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine s_cal_expricit_sph_adams
!
      use m_control_params_sph_MHD
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
!
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt. 0)   call cal_vorticity_eq_adams
!
      if(iflag_t_evo_4_magne .gt. 0)  call cal_diff_induction_MHD_adams
      if(iflag_t_evo_4_temp .gt. 0)   call cal_heat_diff_advect_adams
      if(iflag_t_evo_4_composit.gt.0) call cal_scalar_diff_advect_adams
!$omp end parallel
!
      end subroutine s_cal_expricit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_expricit_sph_euler(i_step)
!
      use m_control_params_sph_MHD
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use cal_vorticity_terms_adams
!
      integer(kind = kint), intent(in) :: i_step
!
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt. 0)   call cal_vorticity_eq_euler
!
      if(iflag_t_evo_4_temp .gt. 0)   call cal_heat_diff_advect_euler
      if(iflag_t_evo_4_magne .gt. 0)  call cal_diff_induction_MHD_euler
      if(iflag_t_evo_4_composit.gt.0) call cal_scalar_diff_advect_euler
!
      if (i_step .eq. 1) then
        if(iflag_t_evo_4_velo .gt. 0)   call set_adams_advect_4_ini
        if(iflag_t_evo_4_temp .gt. 0)   call set_adams_heat_ini
        if(iflag_t_evo_4_magne.gt.0)    call set_adams_mag_induct_ini
        if(iflag_t_evo_4_composit.gt.0) call set_adams_dscalar_ini
      end if
!$omp end parallel
!
      end subroutine s_cal_expricit_sph_euler
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
