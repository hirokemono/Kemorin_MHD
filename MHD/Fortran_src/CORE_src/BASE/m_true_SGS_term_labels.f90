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
!!      integer(kind = kint) function num_true_SGS_terms()
!!      subroutine set_true_SGS_term_labels(n_comps, names, maths)
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
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
!
      implicit  none
! 
      integer(kind = kint), parameter, private :: ntrue_SGS = 10
!
!
!>        Field label for true divergence of SGS Maxwell tensor
!!         @f$ e_{ijk}\left(\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_true =    'SGS_Lorentz_true'
      type(field_def), parameter :: SGS_Lorentz_true                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_Lorentz_true',                        &
     &                math = '$ e_{ijk} (\widetilde{J_{j}B_{k}}'        &
     &                     // ' - \tilde{J}_{j}\tilde{B}_{k}) $')
!>        Field label for true divergence
!>            of SGS magnetic induction tensor
!!         @f$ e_{ijk} \partual_{j} e_{klm} (\widetilde{u_{l}B_{m}}
!!            - \tilde{u}_{l}\tilde{B}_{m} ) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_mag_induct_true = 'SGS_mag_induction_true'
      type(field_def), parameter :: SGS_mag_induction_true              &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_mag_induction_true',                  &
     &                math = '$ e_{ijk} \partual_{j} e_{klm}'           &
     &                     // ' (\widetilde{u_{l}B_{m}}'                &
     &                     // ' - \tilde{u}_{j}\tilde{B}_{k}) $')
!
!>        Field label for true divergence of SGS momentum flux
!!         @f$ \partial_{i} ( \widetilde{u_{i}u_{j}}
!!             - \tilde{u}_{i}\tilde{u}_{j}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_m_flux_true = 'SGS_div_m_flux_true'
      type(field_def), parameter :: SGS_div_m_flux_true                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'SGS_div_m_flux_true',                     &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}u_{j}}'  &
     &                     // ' - \tilde{u}_{i}\tilde{u}_{j}) $')
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_h_flux_true = 'SGS_div_h_flux_true'
      type(field_def), parameter :: SGS_div_h_flux_true                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_div_h_flux_true',                     &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}T}'      &
     &                     // ' - \tilde{u}_{i}\tilde{T}) $')
!>        Field label for true divergence of SGS heat flux
!!         @f$ \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_div_c_flux_true = 'SGS_div_c_flux_true'
      type(field_def), parameter :: SGS_div_c_flux_true                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_div_c_flux_true',                     &
     &                math = '$ \partial_{i} ( \widetilde{u_{i}C}'      &
     &                     // ' - \tilde{u}_{i}\tilde{C}) $')
!
!>        Field label for work of true SGS Reynolds stress
!!         @f$ -u_{i} e_{ijk} (\widetilde{\omega_{j}u_{k}}
!!            - \tilde{\omega}_{j}\tilde{u}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_Reynolds_work_true =  'Reynolds_work_true'
      type(field_def), parameter :: Reynolds_work_true                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Reynolds_work_true',                      &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for work of true SGS Lorentz force
!!         @f$  u_{i} e_{ijk} (\widetilde{J_{j}B_{k}}
!!            - \tilde{J}_{j}\tilde{B}_{k}) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_Lorentz_wk_true = 'SGS_Lorentz_work_true'
      type(field_def), parameter :: SGS_Lorentz_work_true               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_Lorentz_work_true',                   &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for energy flux of true SGS induction
!!         @f$ B_{i} e_{ijk} \partual_{j} e_{klm} 
!!            (\widetilde{u_{l}B_{m}} - \tilde{u}_{l}\tilde{B}_{m} ) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_m_ene_gen_true = 'SGS_induction_flux_true'
      type(field_def), parameter :: SGS_mag_induction_flux_true         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_mag_induction_flux_true',             &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for temperature generation
!!                   by true SGS heat flux
!!         @f$ T \partial_{i} \left( \widetilde{u_{i}T}
!!            - \tilde{u}_{i}\tilde{T} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_temp_gen_true =   'SGS_temp_flux_gen_true'
      type(field_def), parameter :: SGS_temp_flux_gen_true              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_temp_flux_gen_true',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!>        Field label for composition generation
!!                   by true SGS compostion flux
!!         @f$ C \partial_{i} \left( \widetilde{u_{i}C}
!!            - \tilde{u}_{i}\tilde{C} \right) @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_SGS_comp_gen_true =   'SGS_comp_flux_gen_true'
      type(field_def), parameter :: SGS_comp_flux_gen_true              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'SGS_comp_flux_gen_true',                  &
     &                math = '$ \partial_{i} e_{ijk}'                   &
     &                     // ' (\widetilde{\omega_{j}u_{k}}'           &
     &                     // ' - \tilde{\omega}_{j}\tilde{u}_{k}) $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_true_SGS_terms()
      num_true_SGS_terms = ntrue_SGS
      return
      end function num_true_SGS_terms
!
! ----------------------------------------------------------------------
!
      subroutine set_true_SGS_term_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ntrue_SGS)
      character(len = kchara), intent(inout) :: names(ntrue_SGS)
      character(len = kchara), intent(inout) :: maths(ntrue_SGS)
!
!
      call set_field_labels(SGS_Lorentz_true,                           &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(SGS_mag_induction_true,                     &
     &    n_comps( 2), names( 2), maths( 2))
!
      call set_field_labels(SGS_div_m_flux_true,                        &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(SGS_div_h_flux_true,                        &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(SGS_div_c_flux_true,                        &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(Reynolds_work_true,                         &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(SGS_Lorentz_work_true,                      &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(SGS_mag_induction_flux_true,                &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(SGS_temp_flux_gen_true,                     &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(SGS_comp_flux_gen_true,                     &
     &    n_comps(10), names(10), maths(10))
!
      end subroutine set_true_SGS_term_labels
!
! ----------------------------------------------------------------------
!
      end module m_true_SGS_term_labels
