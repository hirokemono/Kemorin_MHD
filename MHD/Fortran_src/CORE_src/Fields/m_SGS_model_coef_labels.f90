!>@file   m_SGS_model_coef_labels.f90
!!        module m_SGS_model_coef_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS model coefficients and work area
!!
!!@verbatim
!!      logical function check_SGS_moedel_coefs(field_name)
!!      logical function check_dynamic_SGS_work(field_name)
!!      logical function check_commute_SGS_work(field_name)
!!
!!      subroutine set_SGS_model_coefs_names(array_c2i)
!!      subroutine set_dynamic_SGS_work_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   Csim_SGS_heat_flux       [Csim%i_SGS_h_flux]: SGS heat flux
!!   Csim_SGS_composit_flux   [Csim%i_SGS_c_flux]: SGS composition flux
!!
!!   Csim_SGS_inertia         [Csim%i_SGS_m_flux]: SGS inertia
!!   Csim_SGS_Lorentz         [Csim%i_SGS_Lorentz]: SGS Lorentz force
!!
!!   Csim_SGS_buoyancy        [Csim%i_SGS_buoyancy]:
!!                                            SGS Thermal buoyancy
!!   Csim_SGS_composit_buo    [Csim%i_SGS_comp_buo]:
!!                                            SGS compositional buoyancy
!!
!!   Csim_SGS_induction       [Csim%i_SGS_vp_induct]:
!!                                            SGS induction  u \times 
!!
!! Work area for dynamic model in FEM_MHD
!!
!!   SGS_simi    [i_simi]
!!   SGS_grad    [i_nlg]
!!   SGS_grad_f  [i_wd_nlg]
!!
!!   SGS_diffuse     [i_wk_diffuse]
!!
!!   temp_4_SGS      [i_sgs_temp]
!!   temp_4_SGS      [i_sgs_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_SGS_model_coef_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>        Field label for model coefficient of SGS heat flux
!!         @f$ C^{sim}_{I} @f$
      type(field_def), parameter :: Csim_SGS_heat_flux                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_heat_flux',                      &
     &                math = '$ C^{sim}_{I} $')
!>        Field label for model coefficient of SGS composition flux
!!         @f$ C^{sim}_{Ic} @f$
      type(field_def), parameter :: Csim_SGS_composit_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_flux',                  &
     &                math = '$ C^{sim}_{Ic}} $')
!>        Field label for model coefficient of SGS inertia term
!!         @f$ C^{sim}_{\tau} @f$
      type(field_def), parameter :: Csim_SGS_inertia                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_inertia',                        &
     &                math = '$ C^{sim}_{\tau}} $')
!>        Field label for model coefficient of SGS Lorentz force
!!         @f$ C^{sim}_{M} @f$
      type(field_def), parameter :: Csim_SGS_Lorentz                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_Lorentz',                        &
     &                math = '$ C^{sim}_{M}} $')
!>        Field label for model coefficient of SGS induction
!!         @f$ C^{sim}_{\alpha} @f$
      type(field_def), parameter :: Csim_SGS_induction                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_induction',                      &
     &                math = '$ C^{sim}_{\alpha}} $')
!>        Field label for model coefficient of SGS buoyancy
!!         @f$ C^{sim}_{buo} @f$
      type(field_def), parameter :: Csim_SGS_buoyancy                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_buoyancy',                       &
     &                math = '$  C^{sim}_{buo} $')
!>        Field label for odel coefficient of SGS compositional buoyancy
!!         @f$ C^{sim}_{buo_C} @f$
      type(field_def), parameter :: Csim_SGS_composit_buo               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Csim_SGS_composit_buo',                   &
     &                math = '$  C^{sim}_{buo_C} $')
!
!
!>        Field label for SGS term by scale similarity method
!!         @f$ \mathcal{L} @f$
      type(field_def), parameter :: SGS_simi                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_simi',                                &
     &                math = '$  \mathcal{L} $')
!>        Field label for SGS term by nonlinear gradient method
!!         @f$ I_{i} @f$
      type(field_def), parameter :: SGS_grad                            &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad',                                &
     &                math = '$ I_{i} $')
!>        Field label for SGS term by nonlinear gradient method
!>        using fileterd field
!!         @f$ I_{i}^{2\Delta} @f$
      type(field_def), parameter :: SGS_grad_f                          &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_grad_f',                              &
     &                math = '$ I_{i}^{2\Delta} $')
!
!>        Field label for SGS term by turbulence diffusivity
!!         @f$ I_{\nu} @f$
      type(field_def), parameter :: SGS_diffuse                         &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'SGS_diffuse',                             &
     &                math = '$ I_{\nu} $')
!
!>        Field label for temperature to obatin commutation error
      type(field_def), parameter :: temp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_4_SGS',                              &
     &                math = '$ T $')
!>        Field label for composition variation
!!        to obatin commutation error
      type(field_def), parameter :: comp_4_SGS                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_4_SGS',                              &
     &                math = '$ C $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_SGS_moedel_coefs(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_SGS_moedel_coefs                                            &
     &   =    (field_name .eq. Csim_SGS_heat_flux%name)                 &
     &   .or. (field_name .eq. Csim_SGS_composit_flux%name)             &
     &   .or. (field_name .eq. Csim_SGS_inertia%name)                   &
     &   .or. (field_name .eq. Csim_SGS_Lorentz%name)                   &
     &   .or. (field_name .eq. Csim_SGS_induction%name)                 &
     &   .or. (field_name .eq. Csim_SGS_buoyancy%name)                  &
     &   .or. (field_name .eq. Csim_SGS_composit_buo%name)
!
      end function check_SGS_moedel_coefs
!
! ----------------------------------------------------------------------
!
      logical function check_dynamic_SGS_work(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_dynamic_SGS_work                                            &
     &   =    (field_name .eq. SGS_simi%name)                           &
     &   .or. (field_name .eq. SGS_grad%name)                           &
     &   .or. (field_name .eq. SGS_grad_f%name)                         &
     &   .or. (field_name .eq. SGS_diffuse%name)
!
      end function check_dynamic_SGS_work
!
! ----------------------------------------------------------------------
!
      logical function check_commute_SGS_work(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_commute_SGS_work                                            &
     &   =    (field_name .eq. temp_4_SGS%name)                         &
     &   .or. (field_name .eq. comp_4_SGS%name)
!
      end function check_commute_SGS_work
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_SGS_model_coefs_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(Csim_SGS_heat_flux,     array_c2i)
      call set_field_label_to_ctl(Csim_SGS_composit_flux, array_c2i)
      call set_field_label_to_ctl(Csim_SGS_inertia,       array_c2i)
      call set_field_label_to_ctl(Csim_SGS_Lorentz,       array_c2i)
      call set_field_label_to_ctl(Csim_SGS_induction,     array_c2i)
      call set_field_label_to_ctl(Csim_SGS_buoyancy,      array_c2i)
      call set_field_label_to_ctl(Csim_SGS_composit_buo,  array_c2i)
!
      end subroutine set_SGS_model_coefs_names
!
! ----------------------------------------------------------------------
!
      subroutine set_dynamic_SGS_work_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(SGS_diffuse,  array_c2i)
      call set_field_label_to_ctl(SGS_simi,  array_c2i)
      call set_field_label_to_ctl(SGS_grad,  array_c2i)
      call set_field_label_to_ctl(SGS_grad_f,  array_c2i)
      call set_field_label_to_ctl(temp_4_SGS,  array_c2i)
      call set_field_label_to_ctl(comp_4_SGS,  array_c2i)
!
      end subroutine set_dynamic_SGS_work_names
!
! ----------------------------------------------------------------------
!
      end module m_SGS_model_coef_labels
