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
!!      subroutine set_force_w_SGS_addresses                            &
!!     &         (i_phys, field_name, frc_w_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: frc_w_SGS
!!
!!      integer(kind = kint) function num_force_w_SGS()
!!      subroutine set_force_with_SGS_labels(n_comps, names, maths)
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
      use t_SGS_term_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: nforce_w_SGS = 9
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
      subroutine set_force_w_SGS_addresses                              &
     &         (i_phys, field_name, frc_w_SGS, flag)
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
          frc_w_SGS%i_SGS_m_flux =     i_phys
        else if (field_name .eq. maxwell_tensor_w_SGS%name ) then
          frc_w_SGS%i_SGS_maxwell =    i_phys
        else if (field_name .eq. induction_tensor_w_SGS%name ) then
          frc_w_SGS%i_SGS_induct_t =    i_phys
!
        else if (field_name .eq. heat_flux_w_SGS%name) then
          frc_w_SGS%i_SGS_h_flux =    i_phys
        else if (field_name .eq. compostion_flux_w_SGS%name) then
          frc_w_SGS%i_SGS_c_flux =    i_phys
!
        else if (field_name .eq. intertia_w_SGS%name) then
          frc_w_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. Lorentz_w_SGS%name) then
          frc_w_SGS%i_SGS_Lorentz =    i_phys
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
      integer(kind = kint) function num_force_w_SGS()
      num_force_w_SGS = nforce_w_SGS
      return
      end function num_force_w_SGS
!
! ----------------------------------------------------------------------
!
      subroutine set_force_with_SGS_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(nforce_w_SGS)
      character(len = kchara), intent(inout) :: names(nforce_w_SGS)
      character(len = kchara), intent(inout) :: maths(nforce_w_SGS)
!
!
      call set_field_labels(momentum_flux_w_SGS,                        &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(maxwell_tensor_w_SGS,                       &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(induction_tensor_w_SGS,                     &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(heat_flux_w_SGS,                            &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(compostion_flux_w_SGS,                      &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(intertia_w_SGS,                             &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(Lorentz_w_SGS,                              &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(vecp_induction_w_SGS,                       &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(induction_w_SGS,                            &
     &    n_comps( 9), names( 9), maths( 9))
!
      end subroutine set_force_with_SGS_labels
!
! ----------------------------------------------------------------------
!
      end module m_force_w_SGS_labels
