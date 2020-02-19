!>@file   t_explicit_term_labels.f90
!!        module t_explicit_term_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic forces
!!
!!@verbatim
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!      Explicit terms at previous time step
!!    sum_forces       [i_forces]:     Total force
!!    rot_sum_forces   [i_rot_forces]: Rotation of toatl force
!!    div_sum_forces   [i_div_forces]: Divergence of total force
!!
!!      Explicit terms at previous time step
!!    previous_momentum    [i_pre_mom]:       Momentum equation
!!    previous_induction   [i_pre_uxb]:       Induction equation
!!    previous_heat        [i_pre_heat]:      Heat equation
!!    previous_composition [i_pre_composit]:  Composition equation
!!    previous_pressure    [i_pre_press]:     Pressure gradient
!!    previous_potential   [i_pre_potential]: Scalar potential gradient
!!
!!      Check of explicit terms
!!    check_momentum       [i_chk_mom]:       Momentum equation
!!    check_induction      [i_chk_uxb]:       Induction equation
!!    check_heat           [i_chk_heat]:      Heat equation
!!    check_composition    [i_chk_composit]:  Composition equation
!!    check_pressure       [i_chk_press]:     Pressure gradient
!!    check_potential      [i_chk_potential]: Scalar potential gradient
!!
!!      Second check of explicit terms
!!    check_momentum_2     [i_chk_mom]:       Momentum equation
!!    check_induction_2    [i_chk_uxb]:       Induction equation
!!    check_heat_2         [i_chk_heat]:      Heat equation
!!    check_composition_2  [i_chk_composit]:  Composition equation
!!    check_pressure_2     [i_chk_press]:     Pressure gradient
!!    check_potential_2    [i_chk_potential]: Scalar potential gradient
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_explicit_term_labels
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!  arrays for current forces
!
!>        Field label for total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_forces =        'sum_forces'
!>        Field label for curl of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_forces =    'rot_sum_forces'
!>        Field label for divergence of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_div_forces =    'div_sum_forces'
!
!  arrays for previous evolution
!
!>        Field label for explicit term for momentum at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_mom =        'previous_momentum'
!>        Field label for explicit term for induction at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_uxb =        'previous_induction'
!>        Field label for explicit term for heat at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_heat =       'previous_heat'
!>        Field label for explicit term for composition
!!        at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_composit =   'previous_composition'
!>        Field label for explicit term for pressure at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_press =      'previous_pressure'
!>        Field label for explicit term for potential at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_potential =  'previous_potential'
!
!  arrays for evolution check
!
!>        Field label for explicit term for momentum for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom =       'check_momentum'
!>        Field label for explicit term for induction for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb =       'check_induction'
!>        Field label for explicit term for heat for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat =      'check_heat'
!>        Field label for explicit term for composition
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit =  'check_composition'
!>        Field label for explicit term for pressure for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press =     'check_pressure'
!>        Field label for explicit term for electric potential
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential = 'check_potential'
!
!>        Field label for explicit term for momentum for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom_2 =       'check_momentum_2'
!>        Field label for explicit term for induction for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb_2 =       'check_induction_2'
!>        Field label for explicit term for heat for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat_2 =      'check_heat_2'
!>        Field label for explicit term for composition
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit_2 =  'check_composition_2'
!>        Field label for explicit term for pressure for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press_2 =     'check_pressure_2'
!>        Field label for explicit term for electric potential
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential_2 = 'check_potential_2'
!
      end module t_explicit_term_labels
