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
!! !!!!!  SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!     SGS terms using wider filter
!!   wide_SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   wide_SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!
!!   wide_SGS_vp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!
!!   wide_SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   wide_SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!     SGS terms using doulbe filter
!!   double_SGS_inertia    [i_SGS_inertia]: SGS inertia
!!   double_SGS_Lorentz   [i_SGS_Lorentz]:   SGS Lorentz force
!!
!!   double_SGS_vp_induction   [i_SGS_vp_induct]: SGS induction  u \times 
!!
!!   double_SGS_heat_flux       [i_SGS_h_flux]:   SGS heat flux
!!   double_SGS_composit_flux   [i_SGS_c_flux]:   SGS composition flux
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_wide_SGS_term_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!
!      SGS terms by wider filter
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{u_{i}T} - \bar{u}_{i}\bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_h_flux = 'wide_SGS_heat_flux'
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{u_{i}C} - \bar{u}_{i}\bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_c_flux = 'wide_SGS_composit_flux'
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\omega_{j}u_{k}}
!!            - \bar{\omega}_{j}\bar{u}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_inertia = 'wide_SGS_inertia'
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\J_{j}B_{k}}
!!            - \bar{J}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_Lorentz = 'wide_SGS_Lorentz'
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\u_{j}B_{k}}
!!            - \bar{u}_{j}\bar{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_wide_SGS_vp_induct = 'wide_SGS_vp_induction'
!
!
!      SGS terms by double filtering
!
!>        Field label for SGS heat flux with wider filter
!!         @f$ \overline{\overline{u_{i}T}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{T}} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_h_flux = 'double_SGS_heat_flux'
!>        Field label for SGS composition flux with wider filter
!!         @f$ \overline{\overline{u_{i}C}}
!!            - \bar{\bar{u}}_{i}\bar{\bar{C}} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_c_flux = 'double_SGS_composit_flux'
!>        Field label for SGS inertia term with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\omega_{j}u_{k}}}
!!            - \bar{\bar{\omega}}_{j}\bar{\bar{u}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_inertia = 'double_SGS_inertia'
!>        Field label for SGS Lorentz force with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\J_{j}B_{k}}}
!!            - \bar{\bar{J}}_{j}\bar{\bar{B}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_Lorentz = 'double_SGS_Lorentz'
!>        Field label for SGS induction with wider filter
!!         @f$ e_{ijk}\left(\overline{\overline{\u_{j}B_{k}}}
!!            - \bar{\bar{u}}_{j}\bar{\bar{B}}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_dbl_SGS_vp_induct = 'double_SGS_vp_induction'
!
      end module m_wide_SGS_term_labels
