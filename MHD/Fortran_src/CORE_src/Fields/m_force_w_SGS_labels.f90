!>@file   m_force_w_SGS_labels.f90
!!        module m_force_w_SGS_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!!      logical function check_force_w_SGS(field_name)
!!      logical function check_flux_tensor_w_SGS(field_name)
!!      logical function check_induction_tensor_w_SGS(field_name)
!!
!!      subroutine set_force_with_SGS_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
!!@endverbatim
!!
      module m_force_w_SGS_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!>        Field label of momentum flux with SGS term
!!         @f$ u_{i} u_{j} + \left(\widetilde{u_{i}u_{j}}
!!                          - \tilde{u}_{i}\tilde{u}_{j} \right)@f$
      type(field_def), parameter :: momentum_flux_w_SGS                 &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'momentum_flux_w_SGS',                     &
     &                math = '$ u_{i}u_{j}'                             &
     &                    // ' + \left(\widetilde{u_{i}u_{j}}'          &
     &                    // ' - \tilde{u}_{i}\tilde{u}_{j} \right)$')
!>        Field label of momentum flux with SGS term
!!         @f$ B_{i} B_{j} + \left(\widetilde{B_{i}B_{j}}
!!                          - \tilde{B}_{i}\tilde{B}_{j} \right) @f$
      type(field_def), parameter :: maxwell_tensor_w_SGS                &
     &    = field_def(n_comp = n_sym_tensor,                            &
     &                name = 'maxwell_tensor_w_SGS',                    &
     &                math = '$ B_{i}B_{j}'                             &
     &                    // ' + \left(\widetilde{B_{i}B_{j}}'          &
     &                    // ' - \tilde{B}_{i}\tilde{B}_{j} \right)$')
!>        Field label of Tensor for magnetic induction with SGS term
!!         @f$ u_{i} B_{j}  - B_{i} u_{j}
!!        + \left(\widetilde{u_{i}B_{j}} - \widetilde{B_{i}u_{j}}\right)
!!        - \left(\tilde{u}_{i}\tilde{B}_{j} 
!!              - \tilde{B}_{i}\tilde{u}_{j} \right)@f$
      type(field_def), parameter :: induction_tensor_w_SGS              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_tensor_w_SGS',                  &
     &                math = '$ u_{i} B_{j}  - B_{i} u_{j}'             &
     &                     // ' + \left( \widetilde{u_{i}B_{j}}'        &
     &                     // ' - \widetilde{B_{i}u_{j}} \right)'       &
     &                     // ' - \left( \tilde{u}_{i}\tilde{B}_{j}'    &
     &                     // ' + \tilde{B}_{i}\tilde{u}_{j} \right)$')
!
!>        Field label of heat flux with SGS term
!!         @f$ u_{i}T + \left(\widetilde{u_{i}T}
!!                          - \tilde{u}_{i}\tilde{T} \right) @f$
      type(field_def), parameter :: heat_flux_w_SGS                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'heat_flux_w_SGS',                         &
     &                math = '$ u_{i} T + \left(\widetilde{u_{i}T}'     &
     &                    // ' - \tilde{u}_{i}\tilde{T} \right) $')
!>        Field label of compositinoal flux with SGS term
!!         @f$ u_{i} C + \left(\widetilde{u_{i}C} 
!!                           - \tilde{u}_{i}\tilde{C} \right) @f$
      type(field_def), parameter :: compostion_flux_w_SGS               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'compostion_flux_w_SGS',                   &
     &                math = '$ u_{i} C + \left(\widetilde{u_{i}C}'     &
     &                    // ' - \tilde{u}_{i}\tilde{C} \right) $')
!
!>        Field label of advection for momentum with SGS term
!!         @f$ e_{ijk} \omega_{j} u_{k}
!!           + e_{ijk} \left(\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k} \right) @f$
      type(field_def), parameter :: intertia_w_SGS                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'intertia_w_SGS',                          &
     &                math = '$ e_{ijk} \omega_{j} u_{k}'               &
     &                // ' + e_{ijk} \left(\widetilde{\omega_{j}u_{k}}' &
     &                // ' - \tilde{\omega}_{j}\tilde{u}_{k} \right)$')
!>        Field label of Lorentz force with SGS term
!!         @f$ e_{ijk} J_{j} B_{k}
!!           + e_{ijk} \left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      type(field_def), parameter :: Lorentz_w_SGS                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'Lorentz_w_SGS',                           &
     &                math = '$ e_{ijk} J_{j} B_{k}'                    &
     &                    // ' + e_{ijk} \left(\widetilde{J_{j}B_{k}}'  &
     &                    // ' - \tilde{J}_{j}\tilde{B}_{k} \right)$')
!
!>        Field label of inductino for vector potential with SGS term
!!         @f$ e_{ijk} u_{j} B_{k} 
!!           + e_{ijk} \left(\widetilde{u{j}B_{k}}
!!            - \tilde{u}_{j}\tilde{B}_{k} \right) @f$
      type(field_def), parameter :: vecp_induction_w_SGS                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'vecp_induction_w_SGS',                    &
     &                math = '$ e_{ijk} u_{j}B_{k}'                     &
     &                    // ' + e_{ijk} \left(\widetilde{u{j}B_{k}}'   &
     &                    // ' - \tilde{u}_{j}\tilde{B}_{k} \right)$')
!>        Field label of magnetic induction with SGS term
!!         @f$ e_{ijk} \partial_{j} \left(e_{klm}u_{l}B_{m} \right)  
!!           + e_{ijk} \partial_{j} e_{klm} \left(\widetilde{u{l}B_{m}}
!!                            - \tilde{u}_{l}\tilde{B}_{m} \right) @f$
      type(field_def), parameter :: induction_w_SGS                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'induction_w_SGS',                         &
     &                math = '$e_{ijk} \partial_{j}'                    &
     &                    // ' \left(e_{klm}u_{l}B_{m} \right)'         &
     &                    // ' + e_{ijk} \partial_{j}'                  &
     &                    // ' e_{klm} \left(\widetilde{u{l}B_{m}}'     &
     &                    // ' - \tilde{u}_{l}\tilde{B}_{m}) \right)$')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_force_w_SGS(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_force_w_SGS                                                 &
     &   =    (field_name .eq. heat_flux_w_SGS%name)                    &
     &   .or. (field_name .eq. compostion_flux_w_SGS%name)              &
     &   .or. (field_name .eq. intertia_w_SGS%name)                     &
     &   .or. (field_name .eq. Lorentz_w_SGS%name)                      &
!
     &   .or. (field_name .eq. vecp_induction_w_SGS%name)               &
     &   .or. (field_name .eq. induction_w_SGS%name)
!
      end function check_force_w_SGS
!
! ----------------------------------------------------------------------
!
      logical function check_flux_tensor_w_SGS(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_flux_tensor_w_SGS                                           &
     &   =    (field_name .eq. momentum_flux_w_SGS%name)                &
     &   .or. (field_name .eq. maxwell_tensor_w_SGS%name)
!
      end function check_flux_tensor_w_SGS
!
! ----------------------------------------------------------------------
!
      logical function check_induction_tensor_w_SGS(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_induction_tensor_w_SGS                                      &
     &   =    (field_name .eq. induction_tensor_w_SGS%name)
!
      end function check_induction_tensor_w_SGS
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_force_with_SGS_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(momentum_flux_w_SGS,    array_c2i)
      call set_field_label_to_ctl(maxwell_tensor_w_SGS,   array_c2i)
      call set_field_label_to_ctl(induction_tensor_w_SGS, array_c2i)
      call set_field_label_to_ctl(heat_flux_w_SGS,        array_c2i)
      call set_field_label_to_ctl(compostion_flux_w_SGS,  array_c2i)
      call set_field_label_to_ctl(intertia_w_SGS,         array_c2i)
      call set_field_label_to_ctl(Lorentz_w_SGS,          array_c2i)
      call set_field_label_to_ctl(vecp_induction_w_SGS,   array_c2i)
      call set_field_label_to_ctl(induction_w_SGS,        array_c2i)
!
      end subroutine set_force_with_SGS_names
!
! ----------------------------------------------------------------------
!
      end module m_force_w_SGS_labels
