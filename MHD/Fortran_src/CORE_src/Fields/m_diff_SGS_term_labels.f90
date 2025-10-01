!>@file   m_diff_SGS_term_labels.f90
!!        module m_diff_SGS_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!!      logical function check_div_SGS_flux_vector(field_name)
!!      logical function check_div_SGS_flux_tensor(field_name)
!!      logical function check_rot_SGS_terms(field_name)
!!
!!      subroutine set_diff_SGS_term_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!! divergence of SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  field name  [Address]
!!
!!   div_SGS_m_flux   [div_SGS%i_SGS_m_flux]:  SGS momentum flux
!!   div_SGS_h_flux   [div_SGS%i_SGS_h_flux]:   SGS heat flux
!!   div_SGS_c_flux   [div_SGS%i_SGS_c_flux]:   SGS composition flux
!!
!!   div_SGS_inertia  [div_SGS%i_SGS_inertia]: SGS inertia
!!   div_SGS_Lorentz  [div_SGS%i_SGS_Lorentz]:   SGS Lorentz force
!!
!! !!!!! rotation of SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  field name  [Address]
!!
!!   rot_SGS_inertia  [rot_SGS%i_SGS_inertia]: SGS inertia
!!   rot_SGS_Lorentz  [rot_SGS%i_SGS_Lorentz]:   SGS Lorentz force
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_diff_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>        Field label for divergence of SGS momentum flux
!!         @f$ \partial_{i} ( \widetilde{u_{i}u_{j}}
!!             - \tilde{u}_{i}\tilde{u}_{j}) @f$
      type(field_def), parameter :: div_SGS_m_flux                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_SGS_m_flux',                          &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}u_{j}}'  &
     &                     // ' - \tilde{u}_{i}\tilde{u}_{j}) $')
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      type(field_def), parameter :: div_SGS_h_flux                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_h_flux',                          &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}T}'      &
     &                     // ' - \tilde{u}_{i}\tilde{T}) $')
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      type(field_def), parameter :: div_SGS_c_flux                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_c_flux',                          &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}C}'      &
     &                     // ' - \tilde{u}_{i}\tilde{C}) $')
!
!>        Field label for divergence of SGS inertia term
!!         @f$ \partial_{i} e_{ijk} (\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k}) @f$
      type(field_def), parameter :: div_SGS_inertia                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_inertia',                         &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for divergence of SGS Lorentz force
!!         @f$ \partial_{i} e_{ijk} (\widetilde{J{j}u_{k}}
!!            - \tilde{J}_{j}\tilde{u}_{k}) @f$
      type(field_def), parameter :: div_SGS_Lorentz                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_Lorentz',                         &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{J_{j}B_{k}}'                &
     &                     // ' - \tilde{J}_{j}\tilde{B}_{k}) $')
!
!>        Field label for rotation of SGS inertia term
!!        @f$  e_{ijk} \partial_{j} e_{klm} (\widetilde{\omega_{n}u_{m}}
!!            - \tilde{\omega}_{n}\tilde{u}_{m})@f$
      type(field_def), parameter :: rot_SGS_inertia                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_SGS_inertia',                         &
     &                math = '$ e_{ijk} \partial_{j} e_{klm}'           &
     &                     // ' (\widetilde{\omega_{n}u_{m}}'           &
     &                     // ' - \tilde{\omega}_{n}\tilde{u}_{m}) $')
!>        Field label for rotation of SGS Lorentz force
!!        @f$  e_{ijk} \partial_{j} e_{klm}
!!        (\widetilde{J_{n}B_{m}} - \tilde{J}_{n}\tilde{B}_{m})@f$
      type(field_def), parameter :: rot_SGS_Lorentz                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_SGS_Lorentz',                         &
     &                math = '$ e_{ijk} \partial_{j} e_{klm}'           &
     &                     // ' (\widetilde{J_{n}B_{m}}'                &
     &                     // ' - \tilde{J}_{n}\tilde{B}_{m}) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_div_SGS_flux_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_SGS_flux_vector                                         &
     &   =    (field_name .eq. div_SGS_h_flux%name)                     &
     &   .or. (field_name .eq. div_SGS_c_flux%name)                     &
!
     &   .or. (field_name .eq. div_SGS_inertia%name)                    &
     &   .or. (field_name .eq. div_SGS_Lorentz%name)
!
      end function check_div_SGS_flux_vector
!
! ----------------------------------------------------------------------
!
      logical function check_div_SGS_flux_tensor(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_SGS_flux_tensor                                         &
     &   =    (field_name .eq. div_SGS_m_flux%name)
!
      end function check_div_SGS_flux_tensor
!
! ----------------------------------------------------------------------
!
      logical function check_rot_SGS_terms(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_rot_SGS_terms                                               &
     &   =    (field_name .eq. rot_SGS_inertia%name)                    &
     &   .or. (field_name .eq. rot_SGS_Lorentz%name)
!
      end function check_rot_SGS_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_SGS_term_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(div_SGS_m_flux,  array_c2i)
      call set_field_label_to_ctl(div_SGS_h_flux,  array_c2i)
      call set_field_label_to_ctl(div_SGS_c_flux,  array_c2i)
      call set_field_label_to_ctl(div_SGS_inertia, array_c2i)
      call set_field_label_to_ctl(div_SGS_Lorentz, array_c2i)
      call set_field_label_to_ctl(rot_SGS_inertia, array_c2i)
      call set_field_label_to_ctl(rot_SGS_Lorentz, array_c2i)
!
      end subroutine set_diff_SGS_term_names
!
! ----------------------------------------------------------------------
!
      end module m_diff_SGS_term_labels
