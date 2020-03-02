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
!!      subroutine set_div_SGS_term_addresses                           &
!!     &         (i_phys, field_name, div_SGS, flag)
!!        type(SGS_term_address), intent(inout) :: div_SGS
!!      subroutine set_rot_SGS_term_addresses                           &
!!     &         (i_phys, field_name, rot_SGS, flag)
!!
!!      integer(kind = kint) function num_diff_SGS_terms()
!!      subroutine set_diff_SGS_term_labels(n_comps, names, maths)
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
!!   SGS_induction    [rot_SGS%i_SGS_induction]: SGS magneitic induction
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_diff_SGS_term_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_SGS_term_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: ndiff_SGS = 8
!
!
!>        Field label for divergence of SGS momentum flux
!!         @f$ \partial_{i} ( \overline{u_{i}u_{j}}
!!             - \bar{u}_{i}\bar{u}_{j}) @f$
      type(field_def), parameter :: div_SGS_m_flux                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'div_SGS_m_flux',                          &
     &                math = '$ \partial_{i} ( \overline{u_{i}u_{j}}'   &
     &                     // ' - \bar{u}_{i}\bar{u}_{j}) $')
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}T}
!!            - \bar{u}_{i}\bar{T} \right) @f$
      type(field_def), parameter :: div_SGS_h_flux                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_h_flux',                          &
     &                math = '$ \partial_{i} ( \overline{u_{i}T}'       &
     &                     // ' - \bar{u}_{i}\bar{T}) $')
!>        Field label for divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \overline{u_{i}C}
!!            - \bar{u}_{i}\bar{C} \right) @f$
      type(field_def), parameter :: div_SGS_c_flux                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_c_flux',                          &
     &                math = '$ \partial_{i} ( \overline{u_{i}C}'       &
     &                     // ' - \bar{u}_{i}\bar{C}) $')
!
!>        Field label for divergence of SGS inertia term
!!         @f$ \partial_{i} e_{ijk} (\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k}) @f$
      type(field_def), parameter :: div_SGS_inertia                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_inertia',                         &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{\omega_{j}u_{k}}'            &
     &                     // ' - \bar{\omega}_{j}\bar{u}_{k}) $')
!>        Field label for divergence of SGS Lorentz force
!!         @f$ \partial_{i} e_{ijk} (\overline{J{j}u_{k}}
!!            - \bar{J}_{j}\bar{u}_{k}) @f$
      type(field_def), parameter :: div_SGS_Lorentz                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_SGS_Lorentz',                         &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\overline{J_{j}B_{k}}'                 &
     &                     // ' - \bar{J}_{j}\bar{B}_{k}) $')
!
!>        Field label for rotation of SGS inertia term
!!        @f$  e_{ijk} \partial_{j} e_{klm}
!!        (\overline{\omega_{n}u_{m}} - \bar{\omega}_{n}\bar{u}_{m})@f$
      type(field_def), parameter :: rot_SGS_inertia                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_SGS_inertia',                         &
     &                math = '$ e_{ijk} \partial_{j} e_{klm}'           &
     &                     // ' (\overline{\omega_{n}u_{m}}'            &
     &                     // ' - \bar{\omega}_{n}\bar{u}_{m}) $')
!>        Field label for rotation of SGS Lorentz force
!!        @f$  e_{ijk} \partial_{j} e_{klm}
!!        (\overline{J_{n}B_{m}} - \bar{J}_{n}\bar{B}_{m})@f$
      type(field_def), parameter :: rot_SGS_Lorentz                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_SGS_Lorentz',                         &
     &                math = '$ e_{ijk} \partial_{j} e_{klm}'           &
     &                     // ' (\overline{J_{n}B_{m}}'                 &
     &                     // ' - \bar{J}_{n}\bar{B}_{m}) $')
!
!>        Field label for SGS magnetic induction
!!         @f$ e_{ijk} \partual_{j} e_{klm} (\overline{u_{l}B_{m}}
!!            - \bar{u}_{l}\bar{B}_{m} ) @f$
      type(field_def), parameter :: SGS_induction                       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_induction',                           &
     &                math = '$ e_{ijk} \partual_{j} e_{klm}'           &
     &                     // ' (\overline{u_{l}B_{m}}'                 &
     &                     // ' - \bar{u}_{j}\bar{B}_{k}) $')
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
     &   .or. (field_name .eq. rot_SGS_Lorentz%name)                    &
     &   .or. (field_name .eq. SGS_induction%name)
!
      end function check_rot_SGS_terms
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_SGS_term_addresses                             &
     &         (i_phys, field_name, div_SGS, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: div_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_div_SGS_flux_vector(field_name)                      &
     &    .or. check_div_SGS_flux_tensor(field_name)
      if(flag) then
        if (field_name .eq. div_SGS_m_flux%name ) then
          div_SGS%i_SGS_m_flux =     i_phys
        else if (field_name .eq. div_SGS_h_flux%name ) then
          div_SGS%i_SGS_h_flux =     i_phys
        else if (field_name .eq. div_SGS_c_flux%name ) then
          div_SGS%i_SGS_c_flux =     i_phys
!
        else if (field_name .eq. div_SGS_inertia%name) then
          div_SGS%i_SGS_inertia =    i_phys
        else if (field_name .eq. div_SGS_Lorentz%name) then
          div_SGS%i_SGS_Lorentz =    i_phys
        end if
      end if
!
      end subroutine set_div_SGS_term_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_SGS_term_addresses                             &
     &         (i_phys, field_name, rot_SGS, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(SGS_term_address), intent(inout) :: rot_SGS
      logical, intent(inout) :: flag
!
!
      flag = check_rot_SGS_terms(field_name)
      if(flag) then
        if (field_name .eq. rot_SGS_inertia%name) then
          rot_SGS%i_SGS_inertia =   i_phys
        else if (field_name .eq. rot_SGS_Lorentz%name) then
          rot_SGS%i_SGS_Lorentz =   i_phys
!
        else if (field_name .eq. SGS_induction%name) then
          rot_SGS%i_SGS_induction = i_phys
        end if
      end if
!
      end subroutine set_rot_SGS_term_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_diff_SGS_terms()
      num_diff_SGS_terms = ndiff_SGS
      return
      end function num_diff_SGS_terms
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_SGS_term_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ndiff_SGS)
      character(len = kchara), intent(inout) :: names(ndiff_SGS)
      character(len = kchara), intent(inout) :: maths(ndiff_SGS)
!
!
      call set_field_labels(div_SGS_m_flux,                             &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(div_SGS_h_flux,                             &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_SGS_c_flux,                             &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(div_SGS_inertia,                            &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(div_SGS_Lorentz,                            &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(rot_SGS_inertia,                            &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(rot_SGS_Lorentz,                            &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(SGS_induction,                              &
     &    n_comps( 8), names( 8), maths( 8))
!
      end subroutine set_diff_SGS_term_labels
!
! ----------------------------------------------------------------------
!
      end module m_diff_SGS_term_labels
