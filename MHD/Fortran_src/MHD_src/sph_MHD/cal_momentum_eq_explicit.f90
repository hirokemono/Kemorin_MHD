!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine cal_momentum_eq_exp_sph(rj_fld)
!!      subroutine cal_expricit_sph_adams(rj_fld)
!!      subroutine cal_expricit_sph_euler(i_step, rj_fld)
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
      use m_control_parameter
      use m_spheric_parameter
!
      use t_phys_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_momentum_eq_exp_sph(rj_fld)
!
      use cal_explicit_terms
      use calypso_mpi
      use cal_sph_field_by_rotation
      use cal_nonlinear_sph_MHD
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_rot_of_forces_sph_2'
      call cal_rot_of_forces_sph_2(sph_rj1, rj_fld)
!
      call cal_rot_of_induction_sph(sph_rj1, rj_fld)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'cal_div_of_fluxes_sph'
      call cal_div_of_fluxes_sph(sph_rj1, rj_fld)
!
      end subroutine cal_momentum_eq_exp_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_adams(rj_fld)
!
      use m_boundary_params_sph_MHD
      use m_physical_property
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use select_diff_adv_source
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call cal_vorticity_eq_adams(sph_bc_U%kr_in, sph_bc_U%kr_out,    &
     &      nnod_rj, sph_rj1%nidx_rj(2),                                &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_adams                               &
     &     (rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,      &
     &      ipol%i_temp, ipol%i_pre_heat, coef_exp_t, coef_h_src,       &
     &      sph_rj1, rj_fld)
      end if
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call sel_scalar_diff_adv_src_adams                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,     &
     &      ipol%i_light, ipol%i_pre_composit, coef_exp_c, coef_c_src,  &
     &      sph_rj1, rj_fld)
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_expricit_sph_euler(i_step, rj_fld)
!
      use m_boundary_params_sph_MHD
      use m_physical_property
      use cal_explicit_terms
      use cal_vorticity_terms_adams
      use select_diff_adv_source
!
      integer(kind = kint), intent(in) :: i_step
      type(phys_data), intent(inout) :: rj_fld
!
!$omp parallel
      if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
        call cal_vorticity_eq_euler(sph_bc_U%kr_in, sph_bc_U%kr_out,    &
     &      nnod_rj, sph_rj1%nidx_rj(2),                                &
     &      rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_T%kr_in, sph_bc_T%kr_out,                            &
     &      ipol%i_t_diffuse, ipol%i_h_advect, ipol%i_heat_source,      &
     &      ipol%i_temp, coef_exp_t, coef_temp, coef_h_src,             &
     &      sph_rj1, rj_fld)
      end if
      if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
        call cal_diff_induction_MHD_euler                               &
     &     (rj_fld%ntot_phys, rj_fld%d_fld)
      end if
      if(iflag_t_evo_4_composit .gt. id_no_evolution) then
        call sel_scalar_diff_adv_src_euler                              &
     &     (sph_bc_C%kr_in, sph_bc_C%kr_out,                            &
     &      ipol%i_c_diffuse, ipol%i_c_advect, ipol%i_light_source,     &
     &      ipol%i_light, coef_exp_c, coef_light, coef_c_src,           &
     &      sph_rj1, rj_fld)
      end if
!
      if (i_step .eq. 1) then
        if(iflag_t_evo_4_velo .gt.     id_no_evolution) then
          call set_ini_adams_inertia                                    &
     &       (nnod_rj, rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(iflag_t_evo_4_temp .gt.     id_no_evolution) then
          call sel_ini_adams_scalar_w_src                               &
     &       (sph_bc_T%kr_in, sph_bc_T%kr_out, ipol%i_h_advect,         &
     &        ipol%i_heat_source, ipol%i_pre_heat,                      &
     &        coef_h_src, sph_rj1, rj_fld)
        end if
        if(iflag_t_evo_4_magne .gt.    id_no_evolution) then
          call set_ini_adams_mag_induct                                 &
     &       (rj_fld%ntot_phys, rj_fld%d_fld)
        end if
        if(iflag_t_evo_4_composit .gt. id_no_evolution) then
          call sel_ini_adams_scalar_w_src                               &
     &       (sph_bc_C%kr_in, sph_bc_C%kr_out, ipol%i_c_advect,         &
     &        ipol%i_light_source, ipol%i_pre_composit,                 &
     &        coef_c_src, sph_rj1, rj_fld)
        end if
      end if
!$omp end parallel
!
      end subroutine cal_expricit_sph_euler
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
