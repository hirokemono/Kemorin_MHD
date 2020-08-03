!>@file   set_SGS_term_labels.f90
!!        module set_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      subroutine set_force_w_SGS_addresses                            &
!!     &         (i_phys, field_name, frc_w_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: frc_w_SGS
!!      subroutine set_true_SGS_term_addresses                          &
!!     &         (i_phys, field_name, true_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: true_SGS
!!      subroutine set_true_div_SGS_term_addresses                      &
!!     &         (i_phys, field_name, true_div_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: true_div_SGS
!!      subroutine set_true_SGS_ene_flux_addresses                      &
!!     &         (i_phys, field_name, true_SGS_eflux, flag)
!!
!!      subroutine set_wide_SGS_term_addresses                          &
!!     &         (i_phys, field_name, wide_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: wide_SGS
!!      subroutine set_double_SGS_term_addresses                        &
!!     &         (i_phys, field_name, dble_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: dble_SGS
!!
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   momentum_flux_w_SGS    [frc_w_SGS%i_SGS_m_flux]
!!   maxwell_tensor_w_SGS   [frc_w_SGS%i_SGS_maxwell]
!!   induction_tensor_w_SGS [frc_w_SGS%i_SGS_induct_t]
!!
!!   heat_flux_w_SGS        [frc_w_SGS%i_SGS_h_flux]
!!   compostion_flux_w_SGS  [frc_w_SGS%i_SGS_c_flux]
!!
!!   intertia_w_SGS         [frc_w_SGS%i_SGS_inertia]
!!   Lorentz_w_SGS          [frc_w_SGS%i_SGS_Lorentz]
!!
!!   vecp_induction_w_SGS   [frc_w_SGS%i_SGS_vp_induct]
!!   induction_w_SGS        [frc_w_SGS%i_SGS_induction]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  product of fields names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   SGS_Lorentz_true         [true_SGS%i_SGS_Lorentz]
!!   SGS_mag_induction_true   [true_SGS%i_SGS_induction]
!!
!!   SGS_div_m_flux_true      [true_div_SGS%i_SGS_m_flux]
!!   SGS_div_h_flux_true      [true_div_SGS%i_SGS_h_flux]
!!   SGS_div_c_flux_true      [true_div_SGS%i_SGS_c_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  product of fields names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   SGS_Lorentz_true         [true_SGS%i_SGS_Lorentz]
!!   SGS_mag_induction_true   [true_SGS%i_SGS_induction]
!!
!!   SGS_div_m_flux_true      [true_div_SGS%i_SGS_m_flux]
!!   SGS_div_h_flux_true      [true_div_SGS%i_SGS_h_flux]
!!   SGS_div_c_flux_true      [true_div_SGS%i_SGS_c_flux]
!!
!!   Reynolds_work_true             [true_SGS_eflux%i_reynolds_wk]
!!   SGS_Lorentz_work_true          [true_SGS_eflux%i_SGS_Lor_wk]
!!   SGS_mag_induction_flux_true    [true_SGS_eflux%i_SGS_me_gen]
!!   SGS_temp_flux_gen_true         [true_SGS_eflux%i_SGS_temp_gen]
!!   SGS_comp_flux_gen_true         [true_SGS_eflux%i_SGS_comp_gen]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!     SGS terms using wider filter
!!   wide_SGS_inertia           [wide_SGS%i_SGS_inertia]
!!   wide_SGS_Lorentz           [wide_SGS%i_SGS_Lorentz]
!!
!!   wide_SGS_vp_induction      [wide_SGS%i_SGS_vp_induct]
!!
!!   wide_SGS_heat_flux         [wide_SGS%i_SGS_h_flux]
!!   wide_SGS_composit_flux     [wide_SGS%i_SGS_c_flux]
!!
!!     SGS terms using doulbe filter
!!   double_SGS_inertia         [dble_SGS%i_SGS_inertia]
!!   double_SGS_Lorentz         [dble_SGS%i_SGS_Lorentz]
!!
!!   double_SGS_vp_induction    [dble_SGS%i_SGS_vp_induct]
!!
!!   double_SGS_heat_flux       [dble_SGS%i_SGS_h_flux]
!!   double_SGS_composit_flux   [dble_SGS%i_SGS_c_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_SGS_term_labels
!
      use m_precision
      use m_constants
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_force_w_SGS_addresses                              &
     &         (i_phys, field_name, frc_w_SGS, flag)
!
      use m_force_w_SGS_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: frc_w_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_force_w_SGS(field_name)                              &
     &    .or. check_flux_tensor_w_SGS(field_name)                      &
     &    .or. check_induction_tensor_w_SGS(field_name)
      if(flag) then
        if (field_name .eq. momentum_flux_w_SGS%name ) then
          frc_w_SGS%i_SGS_m_flux =    i_phys
        else if (field_name .eq. maxwell_tensor_w_SGS%name ) then
          frc_w_SGS%i_SGS_maxwell =   i_phys
        else if (field_name .eq. induction_tensor_w_SGS%name ) then
          frc_w_SGS%i_SGS_induct_t =  i_phys
!
        else if (field_name .eq. heat_flux_w_SGS%name) then
          frc_w_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. compostion_flux_w_SGS%name) then
          frc_w_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. intertia_w_SGS%name) then
          frc_w_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. Lorentz_w_SGS%name) then
          frc_w_SGS%i_SGS_Lorentz =   i_phys
!
        else if (field_name .eq. vecp_induction_w_SGS%name) then
          frc_w_SGS%i_SGS_vp_induct = i_phys
        else if (field_name .eq. induction_w_SGS%name) then
          frc_w_SGS%i_SGS_induction = i_phys
        end if
      end if
!
      end subroutine set_force_w_SGS_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_true_SGS_term_addresses                            &
     &         (i_phys, field_name, true_SGS, flag)
!
      use m_true_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: true_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_true_SGS_vector_terms(field_name)
      if(flag) then
        if(field_name .eq. SGS_Lorentz_true%name) then
          true_SGS%i_SGS_Lorentz =   i_phys
        else if (field_name .eq. SGS_mag_induction_true%name) then
          true_SGS%i_SGS_induction = i_phys
        end if
      end if
!
      end subroutine set_true_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_true_div_SGS_term_addresses                        &
     &         (i_phys, field_name, true_div_SGS, flag)
!
      use m_true_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: true_div_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_true_div_SGS_flux_vector(field_name)                 &
     &    .or. check_true_div_SGS_flux_tensor(field_name)
      if(flag) then
        if (field_name .eq. SGS_div_m_flux_true%name ) then
          true_div_SGS%i_SGS_m_flux =     i_phys
        else if (field_name .eq. SGS_div_h_flux_true%name ) then
          true_div_SGS%i_SGS_h_flux =     i_phys
        else if (field_name .eq. SGS_div_c_flux_true%name ) then
          true_div_SGS%i_SGS_c_flux =     i_phys
        end if
      end if
!
      end subroutine set_true_div_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_true_SGS_ene_flux_addresses                        &
     &         (i_phys, field_name, true_SGS_eflux, flag)
!
      use m_true_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_ene_flux_address), intent(inout) :: true_SGS_eflux
      logical, intent(inout) :: flag
!
!
      flag = check_true_SGS_ene_fluxes(field_name)
      if(flag) then
        if (field_name .eq. Reynolds_work_true%name ) then
          true_SGS_eflux%i_reynolds_wk =     i_phys
        else if (field_name .eq. SGS_Lorentz_work_true%name ) then
          true_SGS_eflux%i_SGS_Lor_wk =      i_phys
!
        else if (field_name .eq. SGS_mag_induction_flux_true%name) then
          true_SGS_eflux%i_SGS_me_gen =      i_phys
!
        else if (field_name .eq. SGS_temp_flux_gen_true%name) then
          true_SGS_eflux%i_SGS_temp_gen =    i_phys
        else if (field_name .eq. SGS_comp_flux_gen_true%name) then
          true_SGS_eflux%i_SGS_comp_gen =    i_phys
        end if
      end if
!
      end subroutine set_true_SGS_ene_flux_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_wide_SGS_term_addresses                            &
     &         (i_phys, field_name, wide_SGS, flag)
!
      use m_wide_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: wide_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_wide_SGS_vector_terms(field_name)
      if(flag) then
        if (field_name .eq. wide_SGS_heat_flux%name) then
          wide_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. wide_SGS_composit_flux%name) then
          wide_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. wide_SGS_inertia%name) then
          wide_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. wide_SGS_Lorentz%name) then
          wide_SGS%i_SGS_Lorentz =    i_phys
!
        else if (field_name .eq. wide_SGS_vp_induction%name) then
          wide_SGS%i_SGS_vp_induct =   i_phys
        end if
      end if
!
      end subroutine set_wide_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_double_SGS_term_addresses                          &
     &         (i_phys, field_name, dble_SGS, flag)
!
      use m_wide_SGS_term_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: dble_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_double_SGS_vector_terms(field_name)
      if(flag) then
        if (field_name .eq. double_SGS_heat_flux%name) then
          dble_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. double_SGS_composit_flux%name) then
          dble_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. double_SGS_inertia%name) then
          dble_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. double_SGS_Lorentz%name) then
          dble_SGS%i_SGS_Lorentz =   i_phys
!
        else if (field_name .eq. double_SGS_vp_induction%name) then
          dble_SGS%i_SGS_vp_induct = i_phys
        end if
      end if
!
      end subroutine set_double_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      end module set_SGS_term_labels
