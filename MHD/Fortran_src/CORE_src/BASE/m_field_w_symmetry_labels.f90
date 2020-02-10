!>@file   m_field_w_symmetry_labels.f90
!!        module m_field_w_symmetry_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields with equatorial symmetry
!!
!!@verbatim
!!      logical function check_base_vector_symmetry(field_name)
!!      logical function check_base_scalar_w_symmetry(field_name)
!!      subroutine base_vector_w_sym_addresses                          &
!!     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!!      subroutine base_scalar_w_sym_addresses                          &
!!     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!!        type(base_field_address), intent(inout) :: sym_base_fld
!!        type(base_field_address), intent(inout) :: asym_base_fld
!!      integer(kind = kint) function check_base_field_w_sym_id         &
!!     &                            (i_field, field_name, base_fld,     &
!!     &                             sym_base_fld, asym_base_fld)
!!        type(base_field_address), intent(in) :: base_fld
!!        type(base_field_address), intent(in) :: sym_base_fld
!!        type(base_field_address), intent(in) :: asym_base_fld
!!
!!      subroutine base_vec_w_sym_monitor_address                       &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_sym_base, ave_sym_base,                           &
!!     &          rms_asym_base, ave_asym_base, flag)
!!      subroutine base_scl_w_sym_monitor_address                       &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_sym_base, ave_sym_base,                           &
!!     &          rms_asym_base, ave_asym_base, flag)
!!        type(base_field_address), intent(inout) :: rms_sym_base
!!        type(base_field_address), intent(inout) :: ave_sym_base
!!        type(base_field_address), intent(inout) :: rms_asym_base
!!        type(base_field_address), intent(inout) :: ave_asym_base
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   sym_velocity,           asym_velocity:           velocity    u
!!   sym_temperature,        asym_temperature:        temperature T
!!   sym_pressure,           asym_pressure:           pressure    P
!!   sym_density,            asym_density:            density     \rho
!!   sym_vorticity,          asym_vorticity:          vorticity   \omega
!!   sym_vector_potential,   asym_vector_potential:   vector potential A
!!   sym_magnetic_field,     asym_magnetic_field:     magnetic field B
!!   sym_current_density,    asym_current_density:    current density  J
!!   sym_magnetic_potential, asym_magnetic_potential: potential \phi
!!   sym_composition,        asym_composition:        Composition  C
!!   sym_entropy,            asym_entropy:            Entropy S
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_field_w_symmetry_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!>        Field label for symmetric velocity @f$ u_{sym} @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_velo = 'sym_velocity'
!>        Field label for symmetric vorticity
!!         @f$ \omega_{sym} = e_{ijk} \partial_{j} u_{asym} @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_vort = 'sym_vorticity'
!>        Field label for symmetric pressure @f$ p @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_press = 'sym_pressure'
!!
!>        Field label for symmetric magnetic field @f$ B_{sym} @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_magne = 'sym_magnetic_field'
!>        Field label for symmetric  magnetic vector potential
!!         @f$ B_{asym} = e_{ijk} \partial_{j} A_{sym} @f$
      character(len=kchara), parameter                                  &
     &         ::  fhd_sym_vecp = 'sym_vector_potential'
!>        Field label for symmetric current density
!!         @f$ J_{sym} = e_{ijk} \partial_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_current = 'sym_current_density'
!>        Field label for symmetric  magnetic potential @f$ W @f$
      character(len=kchara), parameter                                  &
     &           :: fhd_sym_mag_potential = 'sym_magnetic_potential'
!>        Field label for symmetric  electric potential @f$ \varphi @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_scalar_potential = 'sym_scalar_potential'
!!
!>        Field label for symmetric density @f$ \rho @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_density =  'sym_density'
!>        Field label for symmetric temperature @f$ T @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_temp =  'sym_temperature'
!>        Field label for symmetric  compostiion variation @f$ C @f$
      character(len=kchara), parameter                                  &
     &           :: fhd_sym_light = 'sym_composition'
!>        Field label for symmetric entropy @f$ S @f$
      character(len=kchara), parameter                                  &
     &         :: fhd_sym_entropy =  'sym_entropy'
!
!>        Field label for symmetric perturbation of density
!!         @f$  \rho - \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_sym_per_density = 'sym_perturbation_density'
!>        Field label for symmetric perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      character(len=kchara), parameter                                  &
     &          :: fhd_sym_part_temp = 'sym_perturbation_temp'
!>        Field label for symmetric perturbation of composition
!!         @f$  C - C_{0} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_sym_part_light = 'sym_parturbation_composition'
!>        Field label for symmetric perturbation of entropy
!!         @f$  S - S_{0} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_sym_per_entropy = 'sym_perturbation_entropy'
!
!!
!>        Field label for asymmetric velocity @f$ u_{asym} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_velo = 'asym_velocity'
!>        Field label for asymmetric vorticity
!!         @f$ \omega_{asym} = e_{ijk} \partial_{j} u_{sym} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_vort = 'asym_vorticity'
!>        Field label for asymmetric pressure @f$ p @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_press = 'asym_pressure'
!!
!>        Field label for asymmetric magnetic field @f$ B_{asym} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_magne = 'asym_magnetic_field'
!>        Field label for asymmetric magnetic vector potential
!!         @f$ B_{sym} = e_{ijk} \partial_{j} A_{asym} @f$
      character(len=kchara), parameter                                  &
     &        ::  fhd_asym_vecp = 'asym_vector_potential'
!>        Field label for asymmetric current density
!!         @f$ J_{asym} = e_{ijk} \partial_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_current = 'asym_current_density'
!>        Field label for asymmetric magnetic potential @f$ W @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_mag_potential = 'asym_magnetic_potential'
!>        Field label for asymmetric electric potential @f$ \varphi @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_scalar_potential = 'asym_scalar_potential'
!!
!>        Field label for asymmetric density @f$ \rho @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_density =  'asym_density'
!>        Field label for asymmetric temperature @f$ T @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_temp =  'asym_temperature'
!>        Field label for asymmetric compostiion variation @f$ C @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_light = 'asym_composition'
!>        Field label for asymmetric entropy @f$ S @f$
      character(len=kchara), parameter                                  &
     &        :: fhd_asym_entropy =  'asym_entropy'
!
!>        Field label for asymmetric perturbation of density
!!         @f$  \rho - \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_per_density = 'asym_perturbation_density'
!>        Field label for asymmetric perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_part_temp = 'asym_perturbation_temp'
!>        Field label for asymmetric perturbation of composition
!!         @f$  C - C_{0} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_part_light = 'asym_parturbation_composition'
!>        Field label for asymmetric perturbation of entropy
!!         @f$  S - S_{0} @f$
      character(len=kchara), parameter                                  &
     &       :: fhd_asym_per_entropy = 'asym_perturbation_entropy'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_base_vector_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_vector_symmetry = .FALSE.
      if (    (field_name .eq. fhd_sym_velo)                            &
     &   .or. (field_name .eq. fhd_sym_vort)                            &
     &   .or. (field_name .eq. fhd_sym_magne)                           &
     &   .or. (field_name .eq. fhd_sym_vecp)                            &
     &   .or. (field_name .eq. fhd_sym_current)                         &
     &   .or. (field_name .eq. fhd_asym_velo)                           &
     &   .or. (field_name .eq. fhd_asym_vort)                           &
     &   .or. (field_name .eq. fhd_asym_magne)                          &
     &   .or. (field_name .eq. fhd_asym_vecp)                           &
     &   .or. (field_name .eq. fhd_asym_current)                        &
     &       )   check_base_vector_symmetry = .TRUE.
!
      end function check_base_vector_symmetry
!
! ----------------------------------------------------------------------
!
      logical function check_base_scalar_w_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_scalar_w_symmetry = .FALSE.
      if (    (field_name .eq. fhd_sym_press)                           &
     &   .or. (field_name .eq. fhd_sym_mag_potential)                   &
     &   .or. (field_name .eq. fhd_sym_scalar_potential)                &
     &   .or. (field_name .eq. fhd_sym_density)                         &
     &   .or. (field_name .eq. fhd_sym_temp)                            &
     &   .or. (field_name .eq. fhd_sym_light)                           &
     &   .or. (field_name .eq. fhd_sym_entropy)                         &
     &   .or. (field_name .eq. fhd_sym_per_density)                     &
     &   .or. (field_name .eq. fhd_sym_part_temp)                       &
     &   .or. (field_name .eq. fhd_sym_part_light)                      &
     &   .or. (field_name .eq. fhd_sym_per_entropy)                     &
     &   .or. (field_name .eq. fhd_asym_press)                          &
     &   .or. (field_name .eq. fhd_asym_mag_potential)                  &
     &   .or. (field_name .eq. fhd_asym_scalar_potential)               &
     &   .or. (field_name .eq. fhd_asym_density)                        &
     &   .or. (field_name .eq. fhd_asym_temp)                           &
     &   .or. (field_name .eq. fhd_asym_light)                          &
     &   .or. (field_name .eq. fhd_asym_entropy)                        &
     &   .or. (field_name .eq. fhd_asym_per_density)                    &
     &   .or. (field_name .eq. fhd_asym_part_temp)                      &
     &   .or. (field_name .eq. fhd_asym_part_light)                     &
     &   .or. (field_name .eq. fhd_asym_per_entropy)                    &
     &      )   check_base_scalar_w_symmetry = .TRUE.
!
      end function check_base_scalar_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine base_vector_w_sym_addresses                            &
     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: sym_base_fld
      type(base_field_address), intent(inout) :: asym_base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_vector_symmetry(field_name)
      if(flag) then
        if (field_name .eq. fhd_sym_velo) then
          sym_base_fld%i_velo = i_phys
        else if (field_name .eq. fhd_sym_vort) then
          sym_base_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_sym_magne) then
          sym_base_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_sym_vecp) then
          sym_base_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_sym_current) then
          sym_base_fld%i_current =  i_phys
!
        else if (field_name .eq. fhd_asym_velo) then
          asym_base_fld%i_velo = i_phys
        else if (field_name .eq. fhd_asym_vort) then
          asym_base_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_asym_magne) then
          asym_base_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_asym_vecp) then
          asym_base_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_asym_current) then
          asym_base_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine base_vector_w_sym_addresses
!
! ----------------------------------------------------------------------
!
      subroutine base_scalar_w_sym_addresses                            &
     &         (i_phys, field_name, sym_base_fld, asym_base_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: sym_base_fld
      type(base_field_address), intent(inout) :: asym_base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_scalar_w_symmetry(field_name)
      if(flag) then
        if (field_name .eq. fhd_sym_press) then
          sym_base_fld%i_press = i_phys
        else if (field_name .eq. fhd_sym_mag_potential) then
          sym_base_fld%i_mag_p =    i_phys
        else if (field_name .eq. fhd_sym_scalar_potential) then
          sym_base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. fhd_sym_temp) then
          sym_base_fld%i_temp =           i_phys
        else if (field_name .eq. fhd_sym_light) then
          sym_base_fld%i_light =          i_phys
        else if (field_name .eq. fhd_sym_density) then
          sym_base_fld%i_density =        i_phys
        else if (field_name .eq. fhd_sym_entropy) then
          sym_base_fld%i_entropy =        i_phys
!
        else if (field_name .eq. fhd_sym_part_temp) then
          sym_base_fld%i_par_temp =           i_phys
        else if (field_name .eq. fhd_sym_part_light) then
          sym_base_fld%i_par_light =          i_phys
        else if (field_name .eq. fhd_sym_per_density) then
          sym_base_fld%i_par_density =        i_phys
        else if (field_name .eq. fhd_sym_per_entropy) then
          sym_base_fld%i_par_entropy =        i_phys
!
        else if (field_name .eq. fhd_asym_press) then
          asym_base_fld%i_press = i_phys
        else if (field_name .eq. fhd_asym_mag_potential) then
          asym_base_fld%i_mag_p =    i_phys
        else if (field_name .eq. fhd_asym_scalar_potential) then
          asym_base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. fhd_asym_temp) then
          asym_base_fld%i_temp =           i_phys
        else if (field_name .eq. fhd_asym_light) then
          asym_base_fld%i_light =          i_phys
        else if (field_name .eq. fhd_asym_density) then
          asym_base_fld%i_density =        i_phys
        else if (field_name .eq. fhd_asym_entropy) then
          asym_base_fld%i_entropy =        i_phys
!
        else if (field_name .eq. fhd_asym_part_temp) then
          asym_base_fld%i_par_temp =           i_phys
        else if (field_name .eq. fhd_asym_part_light) then
          asym_base_fld%i_par_light =          i_phys
        else if (field_name .eq. fhd_asym_per_density) then
          asym_base_fld%i_par_density =        i_phys
        else if (field_name .eq. fhd_asym_per_entropy) then
          asym_base_fld%i_par_entropy =        i_phys
        end if
      end if  
!
      end subroutine base_scalar_w_sym_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function check_base_field_w_sym_id           &
     &                            (i_field, field_name, base_fld,       &
     &                             sym_base_fld, asym_base_fld)
!
      integer(kind = kint), intent(in) :: i_field
      character(len = kchara), intent(in) :: field_name
      type(base_field_address), intent(in) :: base_fld
      type(base_field_address), intent(in) :: sym_base_fld
      type(base_field_address), intent(in) :: asym_base_fld
!
      integer(kind = kint) :: iflag
!
!
      iflag = 0
      if(      (i_field .eq. sym_base_fld%i_velo)                       &
     &    .or. (i_field .eq. asym_base_fld%i_velo)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_velo, fhd_velo)
      else if( (i_field .eq. sym_base_fld%i_vort)                       &
     &    .or. (i_field .eq. asym_base_fld%i_vort)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vort, fhd_vort)
      else if( (i_field .eq. sym_base_fld%i_magne)                      &
     &    .or. (i_field .eq. asym_base_fld%i_magne)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_magne, fhd_magne)
      else if( (i_field .eq. sym_base_fld%i_vecp)                       &
     &    .or. (i_field .eq. asym_base_fld%i_vecp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_vecp, fhd_vecp)
      else if( (i_field .eq. sym_base_fld%i_current)                    &
     &    .or. (i_field .eq. asym_base_fld%i_current)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_current, fhd_current)
!
      else if( (i_field .eq. sym_base_fld%i_press)                      &
     &    .or. (i_field .eq. asym_base_fld%i_press)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_press, fhd_press)
      else if( (i_field .eq. sym_base_fld%i_mag_p)                      &
     &    .or. (i_field .eq. asym_base_fld%i_mag_p)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_mag_p, fhd_mag_potential)
      else if( (i_field .eq. sym_base_fld%i_scalar_p)                   &
     &    .or. (i_field .eq. asym_base_fld%i_scalar_p)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_scalar_p, fhd_scalar_potential)
!
      else if( (i_field .eq. sym_base_fld%i_temp)                       &
     &    .or. (i_field .eq. asym_base_fld%i_temp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_temp, fhd_temp)
      else if( (i_field .eq. sym_base_fld%i_light)                      &
     &    .or. (i_field .eq. asym_base_fld%i_light)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_light, fhd_light)
      else if( (i_field .eq. sym_base_fld%i_density)                    &
     &    .or. (i_field .eq. asym_base_fld%i_density)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_density, fhd_density)
      else if( (i_field .eq. sym_base_fld%i_entropy)                    &
     &    .or. (i_field .eq. asym_base_fld%i_entropy)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_entropy, fhd_entropy)
!
      else if( (i_field .eq. sym_base_fld%i_par_temp)                   &
     &    .or. (i_field .eq. asym_base_fld%i_par_temp)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_temp, fhd_temp)
      else if( (i_field .eq. sym_base_fld%i_par_light)                  &
     &    .or. (i_field .eq. asym_base_fld%i_par_light)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_light, fhd_light)
      else if( (i_field .eq. sym_base_fld%i_par_density)                &
     &    .or. (i_field .eq. asym_base_fld%i_par_density)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_density, fhd_density)
      else if( (i_field .eq. sym_base_fld%i_par_entropy)                &
     &    .or. (i_field .eq. asym_base_fld%i_par_entropy)) then 
        iflag = iflag + missing_field(i_field, field_name,              &
     &                 base_fld%i_par_entropy, fhd_entropy)
      end if
      check_base_field_w_sym_id = iflag
      return
!
      end function check_base_field_w_sym_id
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine base_vec_w_sym_monitor_address                         &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_sym_base, ave_sym_base,                             &
     &          rms_asym_base, ave_asym_base, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_sym_base
      type(base_field_address), intent(inout) :: ave_sym_base
      type(base_field_address), intent(inout) :: rms_asym_base
      type(base_field_address), intent(inout) :: ave_asym_base
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call base_vector_w_sym_addresses                                  &
     &   ((numrms+1), field_name, rms_sym_base, rms_asym_base, flag_r)
      call base_vector_w_sym_addresses                                  &
     &   ((numave+1), field_name, ave_sym_base, ave_asym_base, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine base_vec_w_sym_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine base_scl_w_sym_monitor_address                         &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_sym_base, ave_sym_base,                             &
     &          rms_asym_base, ave_asym_base, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_sym_base
      type(base_field_address), intent(inout) :: ave_sym_base
      type(base_field_address), intent(inout) :: rms_asym_base
      type(base_field_address), intent(inout) :: ave_asym_base
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call base_scalar_w_sym_addresses                                  &
     &   ((numrms+1), field_name, rms_sym_base, rms_asym_base, flag_r)
      call base_scalar_w_sym_addresses                                  &
     &   ((numave+1), field_name, ave_sym_base, ave_asym_base, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine base_scl_w_sym_monitor_address
!
! ----------------------------------------------------------------------
!
      end module m_field_w_symmetry_labels
