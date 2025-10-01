!>@file   m_true_SGS_term_labels.f90
!!        module m_true_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels for true SGS terms and SGS heat fluxes
!!
!!@verbatim
!!      logical function check_true_SGS_vector_terms(field_name)
!!
!!      logical function check_true_div_SGS_flux_vector(field_name)
!!      logical function check_true_div_SGS_flux_tensor(field_name)
!!
!!      logical function check_true_SGS_ene_fluxes(field_name)
!!        type(SGS_ene_flux_address), intent(inout) :: true_SGS_eflux
!!
!!      subroutine set_true_SGS_term_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
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
!!@endverbatim
!!
      module m_true_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>        Field label for true divergence of SGS Maxwell tensor
!!         @f$ e_{ijk} \left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      type(field_def), parameter :: SGS_Lorentz_true                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_Lorentz_true',                        &
     &                math = '$ e_{ijk} \left(\widetilde{J_{j}B_{k}}'   &
     &                     // ' - \tilde{J}_{j}\tilde{B}_{k} \right)$')
!>        Field label for true divergence
!>            of SGS magnetic induction tensor
!!         @f$ e_{ijk} \partial_{j} e_{klm} \left(\widetilde{u_{l}B_{m}}
!!            - \tilde{u}_{l}\tilde{B}_{m} \right) @f$
      type(field_def), parameter :: SGS_mag_induction_true              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_mag_induction_true',                  &
     &                math = '$ e_{ijk} \partial_{j} e_{klm}'           &
     &                     // ' \left(\widetilde{u_{l}B_{m}}'           &
     &                     // ' - \tilde{u}_{j}\tilde{B}_{k} \right)$')
!
!>        Field label for true divergence of SGS momentum flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}u_{j}}
!!             - \tilde{u}_{i}\tilde{u}_{j} \right) @f$
      type(field_def), parameter :: SGS_div_m_flux_true                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_div_m_flux_true',                     &
     &                math = '$ \partial_{i}'                           &
     &                     // ' \left(\widetilde{u_{i}u_{j}}'           &
     &                     // ' - \tilde{u}_{i}\tilde{u}_{j} \right)$')
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      type(field_def), parameter :: SGS_div_h_flux_true                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_div_h_flux_true',                     &
     &                math = '$ \partial_{i} \left(\widetilde{u_{i}T}'  &
     &                     // ' - \tilde{u}_{i}\tilde{T} \right) $')
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      type(field_def), parameter :: SGS_div_c_flux_true                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_div_c_flux_true',                     &
     &                math = '$ \partial_{i} \left(\widetilde{u_{i}C}'  &
     &                     // ' - \tilde{u}_{i}\tilde{C} \right) $')
!
!>        Field label for work of true SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} \left(\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k} \right) @f$
      type(field_def), parameter :: Reynolds_work_true                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Reynolds_work_true',                      &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                // ' \left(\widetilde{\omega_{j}u_{k}}'           &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!>        Field label for work of true SGS Lorentz force
!!         @f$  u_{i} e_{ijk} \left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      type(field_def), parameter :: SGS_Lorentz_work_true               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_Lorentz_work_true',                   &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                // ' \left(\widetilde{\omega_{j}u_{k}}'           &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!>        Field label for energy flux of true SGS induction
!!         @f$ B_{i} e_{ijk} \partial_{j} e_{klm} 
!!            \left(\widetilde{u_{l}B_{m}}
!!                 - \tilde{u}_{l}\tilde{B}_{m} \right) @f$
      type(field_def), parameter :: SGS_mag_induction_flux_true         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_mag_induction_flux_true',             &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                // ' \left(\widetilde{\omega_{j}u_{k}}'           &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!>        Field label for temperature generation
!!                   by true SGS heat flux
!!         @f$ T \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      type(field_def), parameter :: SGS_temp_flux_gen_true              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_temp_flux_gen_true',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                // ' \left(\widetilde{\omega_{j}u_{k}}'           &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!>        Field label for composition generation
!!                   by true SGS compostion flux
!!         @f$ C \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      type(field_def), parameter :: SGS_comp_flux_gen_true              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_flux_gen_true',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                // ' \left(\widetilde{\omega_{j}u_{k}}'           &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_true_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_true_SGS_vector_terms                                       &
     &   =    (field_name .eq. SGS_Lorentz_true%name)                   &
     &   .or. (field_name .eq. SGS_mag_induction_true%name)
!
      end function check_true_SGS_vector_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      logical function check_true_div_SGS_flux_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_true_div_SGS_flux_vector                                    &
     &   =    (field_name .eq. SGS_div_h_flux_true%name)                &
     &   .or. (field_name .eq. SGS_div_c_flux_true%name)
!
      end function check_true_div_SGS_flux_vector
!
! ----------------------------------------------------------------------
!
      logical function check_true_div_SGS_flux_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_true_div_SGS_flux_tensor                                    &
     &   =    (field_name .eq. SGS_div_m_flux_true%name)
!
      end function check_true_div_SGS_flux_tensor
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      logical function check_true_SGS_ene_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_true_SGS_ene_fluxes                                         &
     &   =    (field_name .eq. Reynolds_work_true%name)                 &
     &   .or. (field_name .eq. SGS_Lorentz_work_true%name)              &
!
     &   .or. (field_name .eq. SGS_mag_induction_flux_true%name)        &
!
     &   .or. (field_name .eq. SGS_temp_flux_gen_true%name)             &
     &   .or. (field_name .eq. SGS_comp_flux_gen_true%name)
!
      end function check_true_SGS_ene_fluxes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_true_SGS_term_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(SGS_Lorentz_true,       array_c2i)
      call set_field_label_to_ctl(SGS_mag_induction_true, array_c2i)
      call set_field_label_to_ctl(SGS_div_m_flux_true,    array_c2i)
      call set_field_label_to_ctl(SGS_div_h_flux_true,    array_c2i)
      call set_field_label_to_ctl(SGS_div_c_flux_true,    array_c2i)
      call set_field_label_to_ctl(Reynolds_work_true,     array_c2i)
      call set_field_label_to_ctl(SGS_Lorentz_work_true,  array_c2i)
      call set_field_label_to_ctl(SGS_mag_induction_flux_true,          &
     &                            array_c2i)
      call set_field_label_to_ctl(SGS_temp_flux_gen_true, array_c2i)
      call set_field_label_to_ctl(SGS_comp_flux_gen_true, array_c2i)
!
      end subroutine set_true_SGS_term_names
!
! ----------------------------------------------------------------------
!
      end module m_true_SGS_term_labels
