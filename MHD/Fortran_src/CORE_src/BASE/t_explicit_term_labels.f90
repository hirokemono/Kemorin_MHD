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
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!
!  arrays for current forces
!
!>        Field label for total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_forces =        'sum_forces'
      type(field_def), parameter :: sum_forces                          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sum_forces',                              &
     &                math = '$ F_{i} $')
!>        Field label for curl of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_rot_forces =    'rot_sum_forces'
      type(field_def), parameter :: rot_sum_forces                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_sum_forces',                          &
     &                math = '$ e_{ijk} \partial_{j} F_{k} $')
!>        Field label for divergence of total forces
      character(len=kchara), parameter                                  &
     &             :: fhd_div_forces =    'div_sum_forces'
      type(field_def), parameter :: div_sum_forces                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_sum_forces',                          &
     &                math = '$ \partial_{i} F_{i} $')
!
!  arrays for previous evolution
!
!>        Field label for explicit term for momentum at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_mom =        'previous_momentum'
      type(field_def), parameter :: previous_momentum                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'previous_momentum',                       &
     &                math = '$ F_{u}^{N-1} $')
!>        Field label for explicit term for induction at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_uxb =        'previous_induction'
      type(field_def), parameter :: previous_induction                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'previous_induction',                      &
     &                math = '$ F_{B}^{N-1} $')
!>        Field label for explicit term for heat at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_heat =       'previous_heat'
      type(field_def), parameter :: previous_heat                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_heat',                           &
     &                math = '$ F_{T}^{N-1} $')
!>        Field label for explicit term for composition
!!        at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_composit =   'previous_composition'
      type(field_def), parameter :: previous_composition                &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_composition',                    &
     &                math = '$ F_{C}^{N-1} $')
!>        Field label for explicit term for pressure at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_press =      'previous_pressure'
      type(field_def), parameter :: previous_pressure                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_pressure',                       &
     &                math = '$ P^{N-1} $')
!>        Field label for explicit term for potential at previous step
      character(len=kchara), parameter                                  &
     &             :: fhd_pre_potential =  'previous_potential'
      type(field_def), parameter :: previous_potential                  &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'previous_potential',                      &
     &                math = '$ \varphi^{N-1} $')
!
!  arrays for evolution check
!
!>        Field label for explicit term for momentum for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom =       'check_momentum'
      type(field_def), parameter :: check_momentum                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_momentum',                          &
     &                math = '$ u_{i}^{N-1} $')
!>        Field label for explicit term for induction for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb =       'check_induction'
      type(field_def), parameter :: check_induction                     &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_induction',                         &
     &                math = '$ B_{i}^{N-1} $')
!>        Field label for explicit term for heat for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat =      'check_heat'
      type(field_def), parameter :: check_heat                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_heat',                              &
     &                math = '$ T^{N-1} $')
!>        Field label for explicit term for composition
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit =  'check_composition'
      type(field_def), parameter :: check_composition                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_composition',                       &
     &                math = '$ C^{N-1} $')
!>        Field label for explicit term for pressure for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press =     'check_pressure'
      type(field_def), parameter :: check_pressure                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_pressure',                          &
     &                math = '$ P^{N-1} $')
!>        Field label for explicit term for electric potential
!!        for check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential = 'check_potential'
      type(field_def), parameter :: check_potential                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_potential',                         &
     &                math = '$ \varphi^{N-1} $')
!
!>        Field label for explicit term for momentum for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_mom_2 =       'check_momentum_2'
      type(field_def), parameter :: check_momentum_2                    &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_momentum_2',                        &
     &                math = '$ u_{i}^{N-2} $')
!>        Field label for explicit term for induction for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_uxb_2 =       'check_induction_2'
      type(field_def), parameter :: check_induction_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'check_induction_2',                       &
     &                math = '$ B_{i}^{N-2} $')
!>        Field label for explicit term for heat for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_heat_2 =      'check_heat_2'
      type(field_def), parameter :: check_heat_2                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_heat_2',                            &
     &                math = '$ T^{N-2} $')
!>        Field label for explicit term for composition
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_composit_2 =  'check_composition_2'
      type(field_def), parameter :: check_composition_2                 &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_composition_2',                     &
     &                math = '$ C^{N-2} $')
!>        Field label for explicit term for pressure for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_press_2 =     'check_pressure_2'
      type(field_def), parameter :: check_pressure_2                    &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_pressure_2',                        &
     &                math = '$ P^{N-2} $')
!>        Field label for explicit term for electric potential
!!        for 2nd check
      character(len=kchara), parameter                                  &
     &             :: fhd_chk_potential_2 = 'check_potential_2'
      type(field_def), parameter :: check_potential_2                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'check_potential_2',                       &
     &                math = '$ \varphi^{N-2} $')
!
!
!  arrays for current forces
!>      Work area for explicit terms
      type explicit_term_address
!>        start address for total forces
        integer (kind=kint) :: i_forces =       izero
!>        start address for curl of total forces
        integer (kind=kint) :: i_rot_forces =   izero
!>        start address for divergence of total forces
        integer (kind=kint) :: i_div_forces =   izero
!
!>        start address for explicit term for momentum at previous step
        integer (kind=kint) :: i_pre_mom =      izero
!>        start address for explicit term for induction at previous step
        integer (kind=kint) :: i_pre_uxb =      izero
!>        start address for explicit term for heat at previous step
        integer (kind=kint) :: i_pre_heat =     izero
!>        start address for explicit term for composition
!!        at previous step
        integer (kind=kint) :: i_pre_composit = izero
!>        start address for explicit term for pressure at previous step
        integer (kind=kint) :: i_pre_press =    izero
      end type explicit_term_address
!
!
      end module t_explicit_term_labels
