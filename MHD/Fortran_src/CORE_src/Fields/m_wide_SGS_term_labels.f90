!>@file   m_wide_SGS_term_labels.f90
!!        module m_wide_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms using wide filters
!!
!!@verbatim
!!      logical function check_wide_SGS_vector_terms(field_name)
!!      logical function check_double_SGS_vector_terms(field_name)
!!
!!      subroutine set_wide_SGS_term_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
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
      module m_wide_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
! 
!      SGS terms by wider filter
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      type(field_def), parameter :: wide_SGS_heat_flux                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_heat_flux',                      &
     &                math = '$ \overline{u_{i}T}'                      &
     &                     // ' - \bar{u}_{i}\bar{T} $')
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      type(field_def), parameter :: wide_SGS_composit_flux              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_composit_flux',                  &
     &                math = '$ \overline{u_{i}C}'                      &
     &                     // ' - \bar{u}_{i}\bar{C} $')
!
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk} \left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_inertia                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_inertia',                        &
     &              math = '$ e_{ijk} \left(\overline{\omega_{j}u_{k}}' &
     &                  // ' - \bar{\omega}_{j}\bar{u}_{k} \right) $')
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk} \left(\overline{J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_Lorentz                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_Lorentz',                        &
     &                math = '$ e_{ijk} \left(\overline{J_{j}B_{k}}'    &
     &                     // ' - \bar{J}_{j}\bar{B}_{k} \right) $')
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk} \left(\overline{u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      type(field_def), parameter :: wide_SGS_vp_induction               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'wide_SGS_vp_induction',                   &
     &                math = '$ e_{ijk} \left(\overline{u_{j}B_{k}}'    &
     &                     // ' - \bar{u}_{j}\bar{B}_{k} \right) $')
!
!      SGS terms by double filtering
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \widetilde{\widetilde{u_{i}T}}
!!            - \tilde{\tilde{u}}_{i}\tilde{\tilde{T}} @f$
      type(field_def), parameter :: double_SGS_heat_flux                &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_heat_flux',                    &
     &       math = '$ \widetilde{\widetilde{u_{i}T}}'                  &
     &           // ' - \tilde{\tilde{u}}_{i}\tilde{\tilde{T}} $')
!>        Field label for SGS composition flux with wider filter
!!         @f$ \widetilde{\widetilde{u_{i}C}}
!!            - \tilde{\tilde{u}}_{i}\tilde{\tilde{C}} @f$
      type(field_def), parameter :: double_SGS_composit_flux            &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_composit_flux',                &
     &       math = '$ \widetilde{\widetilde{u_{i}C}}'                  &
     &           // ' - \tilde{\tilde{u}}_{i}\tilde{\tilde{C}} $')
!
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk} \left(\widetilde{\widetilde{\omega_{j}u_{k}}}
!!          - \tilde{\tilde{\omega}}_{j}\tilde{\tilde{u}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_inertia                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_inertia',                      &
     &     math = '$ e_{ijk}'                                           &
     &          // ' \left(\widetilde{\widetilde{\omega_{j}u_{k}}}'     &
     &          // ' - \tilde{\tilde{\omega}}_{j}\tilde{\tilde{u}}_{k}' &
     &          // ' \right) $')
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk} \left(\widetilde{\widetilde{J_{j}B_{k}}}
!!            - \tilde{\tilde{J}}_{j}\tilde{\tilde{B}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_Lorentz                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_Lorentz',                      &
     &       math = '$ e_{ijk}'                                         &
     &               // ' \left(\widetilde{\widetilde{J_{j}B_{k}}}'     &
     &               // ' - \tilde{\tilde{J}}_{j}\tilde{\tilde{B}}_{k}' &
     &               // ' \right) $')
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk} \left(\widetilde{\widetilde{u_{j}B_{k}}}
!!            - \tilde{\tilde{u}}_{j}\tilde{\tilde{B}}_{k} \right) @f$
      type(field_def), parameter :: double_SGS_vp_induction             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'double_SGS_vp_induction',                 &
     &       math = '$ e_{ijk}'                                         &
     &               // ' \left(\widetilde{\widetilde{u_{j}B_{k}}}'     &
     &               // ' - \tilde{\tilde{u}}_{j}\tilde{\tilde{B}}_{k}' &
     &               // ' \right) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_wide_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_SGS_vector_terms                                       &
     &   =    (field_name .eq. wide_SGS_inertia%name)                   &
     &   .or. (field_name .eq. wide_SGS_Lorentz%name)                   &
     &   .or. (field_name .eq. wide_SGS_vp_induction%name)              &
     &   .or. (field_name .eq. wide_SGS_heat_flux%name)                 &
     &   .or. (field_name .eq. wide_SGS_composit_flux%name)
!
      end function check_wide_SGS_vector_terms
!
! ----------------------------------------------------------------------
!
      logical function check_double_SGS_vector_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_SGS_vector_terms                                     &
     &   =    (field_name .eq. double_SGS_inertia%name)                 &
     &   .or. (field_name .eq. double_SGS_Lorentz%name)                 &
     &   .or. (field_name .eq. double_SGS_vp_induction%name)            &
     &   .or. (field_name .eq. double_SGS_heat_flux%name)               &
     &   .or. (field_name .eq. double_SGS_composit_flux%name)
!
      end function check_double_SGS_vector_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_wide_SGS_term_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(wide_SGS_heat_flux,       array_c2i)
      call set_field_label_to_ctl(wide_SGS_composit_flux,   array_c2i)
      call set_field_label_to_ctl(wide_SGS_inertia,         array_c2i)
      call set_field_label_to_ctl(wide_SGS_Lorentz,         array_c2i)
      call set_field_label_to_ctl(wide_SGS_vp_induction,    array_c2i)
      call set_field_label_to_ctl(double_SGS_heat_flux,     array_c2i)
      call set_field_label_to_ctl(double_SGS_composit_flux, array_c2i)
      call set_field_label_to_ctl(double_SGS_inertia,       array_c2i)
      call set_field_label_to_ctl(double_SGS_Lorentz,       array_c2i)
      call set_field_label_to_ctl(double_SGS_vp_induction,  array_c2i)
!
      end subroutine set_wide_SGS_term_names
!
! ----------------------------------------------------------------------
!
      end module m_wide_SGS_term_labels
