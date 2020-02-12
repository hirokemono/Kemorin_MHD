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
!!      integer(kind = kint) function num_fields_w_symmetry()
!!      subroutine set_field_w_symmetry_names(field_names)
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
!
      integer(kind = kint), parameter, private :: nfld_w_sym = 2 * 16
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
     &           :: fhd_sym_entropy =  'sym_entropy'
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
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_fields_w_symmetry()
      num_fields_w_symmetry = nfld_w_sym
      return
      end function num_fields_w_symmetry
!
! ----------------------------------------------------------------------
!
      subroutine set_field_w_symmetry_names(field_names)
!
      character(len = kchara), intent(inout) :: field_names(nfld_w_sym)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_sym_velo), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_asym_velo), CHAR(0)
      write(field_names( 3),'(a,a1)') trim(fhd_sym_vort), CHAR(0)
      write(field_names( 4),'(a,a1)') trim(fhd_asym_vort), CHAR(0)
      write(field_names( 5),'(a,a1)') trim(fhd_sym_press), CHAR(0)
      write(field_names( 6),'(a,a1)') trim(fhd_asym_press), CHAR(0)
!
      write(field_names( 7),'(a,a1)') trim(fhd_sym_magne), CHAR(0)
      write(field_names( 8),'(a,a1)') trim(fhd_asym_magne), CHAR(0)
      write(field_names( 9),'(a,a1)') trim(fhd_sym_vecp), CHAR(0)
      write(field_names(10),'(a,a1)') trim(fhd_asym_vecp), CHAR(0)
      write(field_names(11),'(a,a1)') trim(fhd_sym_current), CHAR(0)
      write(field_names(12),'(a,a1)') trim(fhd_asym_current), CHAR(0)
      write(field_names(13),'(a,a1)')                                   &
     &                      trim(fhd_sym_mag_potential), CHAR(0)
      write(field_names(14),'(a,a1)')                                   &
     &                      trim(fhd_asym_mag_potential), CHAR(0)
      write(field_names(15),'(a,a1)')                                   &
     &                      trim(fhd_sym_scalar_potential), CHAR(0)
      write(field_names(16),'(a,a1)')                                   &
     &                      trim(fhd_asym_scalar_potential), CHAR(0)
!
      write(field_names(17),'(a,a1)') trim(fhd_sym_temp), CHAR(0)
      write(field_names(18),'(a,a1)') trim(fhd_asym_temp), CHAR(0)
      write(field_names(19),'(a,a1)') trim(fhd_sym_light), CHAR(0)
      write(field_names(20),'(a,a1)') trim(fhd_asym_light), CHAR(0)
      write(field_names(21),'(a,a1)') trim(fhd_sym_density), CHAR(0)
      write(field_names(22),'(a,a1)') trim(fhd_asym_density), CHAR(0)
      write(field_names(23),'(a,a1)') trim(fhd_sym_entropy), CHAR(0)
      write(field_names(24),'(a,a1)') trim(fhd_asym_entropy), CHAR(0)
!
      write(field_names(25),'(a,a1)') trim(fhd_sym_part_temp), CHAR(0)
      write(field_names(26),'(a,a1)') trim(fhd_asym_part_temp), CHAR(0)
      write(field_names(27),'(a,a1)') trim(fhd_sym_part_light), CHAR(0)
      write(field_names(28),'(a,a1)')                                   &
     &                      trim(fhd_asym_part_light), CHAR(0)
!
      write(field_names(29),'(a,a1)')                                   &
     &                      trim(fhd_sym_per_density), CHAR(0)
      write(field_names(30),'(a,a1)')                                   &
     &                      trim(fhd_asym_per_density), CHAR(0)
      write(field_names(31),'(a,a1)')                                   &
     &                      trim(fhd_sym_per_entropy), CHAR(0)
      write(field_names(32),'(a,a1)')                                   &
     &                      trim(fhd_asym_per_entropy), CHAR(0)
!
      end subroutine set_field_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_field_w_symmetry_labels
