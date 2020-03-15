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
!!      logical function check_vector_work_field(field_name)
!!      logical function check_scalar_work_field(field_name)
!!      logical function check_vector_check_field(field_name)
!!      logical function check_scalar_check_field(field_name)
!!      subroutine set_work_field_addresses                             &
!!     &         (i_phys, field_name, exp_work, flag)
!!        type(explicit_term_address), intent(inout) :: exp_work
!!      subroutine set_check_field_addresses                            &
!!     &         (i_phys, field_name, check_fld1, check_fld2, flag)
!!        type(explicit_term_address), intent(inout) :: check_fld1
!!        type(explicit_term_address), intent(inout) :: check_fld2
!!
!!      integer(kind = kint) function num_work_4_explicit()
!!      integer(kind = kint) function num_check_fields()
!!      subroutine set_work_4_explicit_labels(n_comps, names, maths)
!!      subroutine set_check_fields_labels(n_comps, names, maths)
!!
!! !!!!!  force include SGS terms names  !!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!      Explicit terms at previous time step
!!    sum_forces       [exp_work%i_forces]:     Total force
!!    rot_sum_forces   [exp_work%i_rot_forces]: Rotation of toatl force
!!    div_sum_forces   [exp_work%i_div_forces]: Divergence of total force
!!
!!      Explicit terms at previous time step
!!    previous_momentum    [exp_work%i_pre_mom]
!!    previous_induction   [exp_work%i_pre_uxb]
!!    previous_heat        [exp_work%i_pre_heat]
!!    previous_composition [exp_work%i_pre_composit]
!!    previous_pressure    [exp_work%i_pre_press]
!!    previous_potential   [exp_work%i_pre_phi]
!!
!!      Check of explicit terms
!!    check_momentum       [check_fld1%i_pre_mom]
!!    check_induction      [check_fld1%i_pre_uxb]
!!    check_heat           [check_fld1%i_pre_heat]
!!    check_composition    [check_fld1%i_pre_composit]
!!    check_pressure       [check_fld1%i_pre_press]
!!    check_potential      [check_fld1%i_pre_phi]
!!
!!      Second check of explicit terms
!!    check_momentum_2     [check_fld2%i_pre_mom]
!!    check_induction_2    [check_fld2%i_pre_uxb]
!!    check_heat_2         [check_fld2%i_pre_heat]
!!    check_composition_2  [check_fld2%i_pre_composit]
!!    check_pressure_2     [check_fld2%i_pre_press]
!!    check_potential_2    [check_fld2%i_pre_phi]
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
      integer(kind = kint), parameter, private :: nexp_work =   9
      integer(kind = kint), parameter, private :: ncheck_fld = 12
!
!  arrays for current forces
!
!>        Field label for total forces
      type(field_def), parameter :: sum_forces                          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sum_forces',                              &
     &                math = '$ F_{i} $')
!>        Field label for curl of total forces
      type(field_def), parameter :: rot_sum_forces                      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_sum_forces',                          &
     &                math = '$ e_{ijk} \partial_{j} F_{k} $')
!>        Field label for divergence of total forces
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
!>        start address for explicit term for potential at previous step
        integer (kind=kint) :: i_pre_phi =    izero
      end type explicit_term_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_vector_work_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_work_field                                           &
     &   =    (field_name .eq. sum_forces%name)                         &
     &   .or. (field_name .eq. rot_sum_forces%name)                     &
!
     &   .or. (field_name .eq. previous_momentum%name)                  &
     &   .or. (field_name .eq. previous_induction%name)
!
      end function check_vector_work_field
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_work_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_work_field                                           &
     &   =    (field_name .eq. div_sum_forces%name)                     &
!
     &   .or. (field_name .eq. previous_heat%name)                      &
     &   .or. (field_name .eq. previous_composition%name)               &
     &   .or. (field_name .eq. previous_pressure%name)                  &
     &   .or. (field_name .eq. previous_potential%name)
!
      end function check_scalar_work_field
!
! ----------------------------------------------------------------------
!
      logical function check_vector_check_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_check_field                                          &
     &   =    (field_name .eq. check_momentum%name)                     &
     &   .or. (field_name .eq. check_induction%name)                    &
!
     &   .or. (field_name .eq. check_momentum_2%name)                   &
     &   .or. (field_name .eq. check_induction_2%name)
!
      end function check_vector_check_field
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_check_field(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_check_field                                          &
     &   =    (field_name .eq. check_heat%name)                         &
     &   .or. (field_name .eq. check_composition%name)                  &
     &   .or. (field_name .eq. check_pressure%name)                     &
     &   .or. (field_name .eq. check_potential%name)                    &
!
     &   .or. (field_name .eq. check_heat_2%name)                       &
     &   .or. (field_name .eq. check_composition_2%name)                &
     &   .or. (field_name .eq. check_pressure_2%name)                   &
     &   .or. (field_name .eq. check_potential_2%name)
!
      end function check_scalar_check_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_work_field_addresses                               &
     &         (i_phys, field_name, exp_work, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(explicit_term_address), intent(inout) :: exp_work
      logical, intent(inout) :: flag
!
!
      flag =   check_vector_work_field(field_name)                      &
     &    .or. check_scalar_work_field(field_name)
      if(flag) then
        if (field_name .eq. sum_forces%name) then
          exp_work%i_forces =       i_phys
        else if (field_name .eq. rot_sum_forces%name) then
          exp_work%i_rot_forces =   i_phys
        else if (field_name .eq. div_sum_forces%name) then
          exp_work%i_div_forces =   i_phys
!
        else if (field_name .eq. previous_momentum%name) then
          exp_work%i_pre_mom =      i_phys
        else if (field_name .eq. previous_induction%name) then
          exp_work%i_pre_uxb =      i_phys
!
        else if (field_name .eq. previous_heat%name) then
          exp_work%i_pre_heat =     i_phys
        else if (field_name .eq. previous_composition%name) then
          exp_work%i_pre_composit = i_phys
        else if (field_name .eq. previous_pressure%name) then
          exp_work%i_pre_press =    i_phys
        else if (field_name .eq. previous_potential%name) then
          exp_work%i_pre_phi =      i_phys
        end if
      end if  
!
      end subroutine set_work_field_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_check_field_addresses                              &
     &         (i_phys, field_name, check_fld1, check_fld2, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(explicit_term_address), intent(inout) :: check_fld1
      type(explicit_term_address), intent(inout) :: check_fld2
      logical, intent(inout) :: flag
!
!
      flag =   check_vector_check_field(field_name)                     &
     &    .or. check_scalar_check_field(field_name)
      if(flag) then
        if (field_name .eq. check_momentum%name) then
          check_fld1%i_pre_mom =      i_phys
        else if (field_name .eq. check_induction%name) then
          check_fld1%i_pre_uxb =      i_phys
!
        else if (field_name .eq. check_heat%name) then
          check_fld1%i_pre_heat =     i_phys
        else if (field_name .eq. check_composition%name) then
          check_fld1%i_pre_composit = i_phys
        else if (field_name .eq. check_pressure%name) then
          check_fld1%i_pre_press =    i_phys
        else if (field_name .eq. check_potential%name) then
          check_fld1%i_pre_phi =      i_phys
!
        else if (field_name .eq. check_momentum_2%name) then
          check_fld2%i_pre_mom =      i_phys
        else if (field_name .eq. check_induction_2%name) then
          check_fld2%i_pre_uxb =      i_phys
!
        else if (field_name .eq. check_heat_2%name) then
          check_fld2%i_pre_heat =     i_phys
        else if (field_name .eq. check_composition_2%name) then
          check_fld2%i_pre_composit = i_phys
        else if (field_name .eq. check_pressure_2%name) then
          check_fld2%i_pre_press =    i_phys
        else if (field_name .eq. check_potential_2%name) then
          check_fld2%i_pre_phi =      i_phys
        end if
      end if  
!
      end subroutine set_check_field_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_work_4_explicit()
      num_work_4_explicit = nexp_work
      return
      end function num_work_4_explicit
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_check_fields()
      num_check_fields = ncheck_fld
      return
      end function num_check_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_work_4_explicit_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(nexp_work)
      character(len = kchara), intent(inout) :: names(nexp_work)
      character(len = kchara), intent(inout) :: maths(nexp_work)
!
!
      call set_field_labels(sum_forces,                                 &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(rot_sum_forces,                             &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_sum_forces,                             &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(previous_momentum,                          &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(previous_induction,                         &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(previous_heat,                              &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(previous_composition,                       &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(previous_pressure,                          &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(previous_potential,                         &
     &    n_comps( 9), names( 9), maths( 9))
!
      end subroutine set_work_4_explicit_labels
!
! ----------------------------------------------------------------------
!
      subroutine set_check_fields_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout) :: n_comps(ncheck_fld)
      character(len = kchara), intent(inout) :: names(ncheck_fld)
      character(len = kchara), intent(inout) :: maths(ncheck_fld)
!
!
      call set_field_labels(check_momentum,                             &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(check_induction,                            &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(check_heat,                                 &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(check_composition,                          &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(check_pressure,                             &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(check_potential,                            &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(check_momentum_2,                           &
     &    n_comps( 7), names( 7), maths( 7))
!
      call set_field_labels(check_induction_2,                          &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(check_heat_2,                               &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(check_composition_2,                        &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(check_pressure_2,                           &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(check_potential_2,                          &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_check_fields_labels
!
! ----------------------------------------------------------------------
!
      end module t_explicit_term_labels
