!>@file   t_base_field_labels.f90
!!        module t_base_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      integer(kind = kint) function missing_field                     &
!!     &                   (iphys_tgt, target_name, iphys_ref, ref_name)
!!
!!      logical function check_base_vector(field_name)
!!      logical function check_base_scalar(field_name)
!!      subroutine set_base_vector_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!      subroutine set_base_scalar_addresses                            &
!!     &         (i_phys, field_name, base_fld, flag)
!!        type(base_field_address), intent(inout) :: base_fld
!!
!!      subroutine base_vector_monitor_address                          &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_base, ave_base, flag)
!!      subroutine base_scalar_monitor_address                          &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_base, ave_base, flag)
!!        type(base_field_address), intent(inout) :: rms_base
!!        type(base_field_address), intent(inout) :: ave_base
!!
!!      integer(kind = kint) function num_base_fields()
!!      subroutine set_base_field_names(field_names)
!!
!! !!!!!  Base field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!   velocity [i_velo]:     velocity    u
!!   pressure [i_press]:     pressure    P
!!   vorticity [i_vort]:    vorticity   \omega = \nabra \times v
!!
!!   vector_potential [i_vecp] :   vector potential \nabla \times A = B
!!   magnetic_field [i_magne]:     magnetic field   B
!!   current_density [i_current]:    current density  J = \nabla \times B
!!   magnetic_potential [i_mag_p]:   potential       \phi
!!   scalar_potential [i_scalar_p]:  scalar potential   \phi
!!   electric_field []:     electric field   E
!!   poynting_flux []:      Poynting flux    S = E \times B
!!
!!   temperature [i_temp]:  temperature T
!!   composition [i_light]:  Composition anormally C
!!   density [i_density]:      density     \rho
!!   entropy [i_entropy]:      Entropy               S
!!
!!   reference_temperature [i_ref_t]:   T_0
!!   reference_composition [i_ref_c]:   C_0
!!   reference_density [i_ref_density]:       \rho_0
!!   reference_entropy [i_ref_entropy]:       S_0
!!
!!   perturbation_temp [i_par_temp]:         \Theta = T - T_0
!!   parturbation_composition [i_par_light]:  C - C_0
!!   perturbation_density [i_par_density]:      \rho - \rho_0
!!   perturbation_entropy [i_par_entropy]:      S - S_0
!!
!!   heat_source [i_heat_source]:            heat source          q_{T}
!!   composition_source [i_light_source]:     compositoin source  q_{C}
!!   entropy_source [i_entropy_source]:         entropy source    q_{S}
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module t_base_field_labels
!
      use m_precision
      use m_constants
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_base = 23
!
!>        Field label for velocity
!!         @f$ u_{i} @f$
      character(len=kchara), parameter :: fhd_velo = 'velocity' 
!>        Field label for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
      character(len=kchara), parameter :: fhd_vort = 'vorticity'
!>        Field label for pressure @f$ p @f$
      character(len=kchara), parameter :: fhd_press = 'pressure'
!!
!>        Field label for magnetic field
!!         @f$ B_{i} @f$
      character(len=kchara), parameter :: fhd_magne = 'magnetic_field'
!>        Field label for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
      character(len=kchara), parameter :: fhd_vecp = 'vector_potential'
!>        Field label for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_current = 'current_density'
!>        Field label for magnetic potential
!!         @f$ W @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_mag_potential =     'magnetic_potential'
!>        Field label for electric potential
!!         @f$ \varphi @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_scalar_potential =  'scalar_potential'
!!
!>        Field label for density
!!         @f$ \rho @f$
      character(len=kchara), parameter :: fhd_density =  'density'
!>        Field label for perturbation of density
!!         @f$  \rho - \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_per_density = 'perturbation_density'
!>        Field label for reference density
!!         @f$  \rho_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_density =  'reference_density'
!
!>        Field label for temperature
!!         @f$ T @f$
      character(len=kchara), parameter :: fhd_temp =  'temperature'
!>        Field label for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_temp = 'perturbation_temp'
!>        Field label for reference temperature
!!         @f$  T_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_temp =  'reference_temperature'
!
!>        Field label for compostiion variation
!!         @f$ C @f$
      character(len=kchara), parameter :: fhd_light = 'composition'
!>        Field label for perturbation of composition
!!         @f$  C - C_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_part_light = 'parturbation_composition'
!>        Field label for reference composition
!!         @f$  C_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_light =  'reference_composition'
!
!>        Field label for entropy
!!         @f$ S @f$
      character(len=kchara), parameter :: fhd_entropy =  'entropy'
!>        Field label for perturbation of entropy
!!         @f$  S - S_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_per_entropy = 'perturbation_entropy'
!>        Field label for reference entropy
!!         @f$  S_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_ref_entropy =  'reference_entropy'
!
!>        Field label for heat source
!!         @f$ q_{T} @f$
      character(len=kchara), parameter                                  &
     &              :: fhd_heat_source =  'heat_source'
!>        Field label for composion source
!!         @f$ q_{C} @f$
      character(len=kchara), parameter                                  &
     &              :: fhd_light_source =  'composition_source'
!>        Field label for entropysource
!!         @f$ q_{S} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_entropy_source =  'entropy_source'
!!
!!
!>       Structure for start address for base fields
      type base_field_address
!>        Start address for velocity
!!         \f$ u_{i} \f$
        integer (kind=kint) :: i_velo  =           izero
!>        Start address for vorticity
!!         @f$ \omega_{i} = e_{ijk} \partial_{j} u_{k} @f$
        integer (kind=kint) :: i_vort  =           izero
!>        Start address for pressure
!!         @f$ p @f$
        integer (kind=kint) :: i_press =           izero
!
!>        start address for magnetic field
!!         @f$ B_{i} @f$
        integer (kind=kint) :: i_magne =           izero
!>        start address for magnetic vector potential
!!         @f$ B_{i} = e_{ijk} \partial_{j} A_{k} @f$
        integer (kind=kint) :: i_vecp =            izero
!>        start address for current density
!!         @f$ J_{i} = e_{ijk} \partial_{j} B_{k} @f$
        integer (kind=kint) :: i_current =         izero
!>        start address for magnetic potential
!!         @f$ W @f$
        integer (kind=kint) :: i_mag_p =           izero
!>        start address for electric potential
!!         @f$ \varphi @f$
        integer (kind=kint) :: i_scalar_p =        izero
!
!>        start address for temperature
!!         @f$ T @f$
        integer (kind=kint) :: i_temp  =           izero
!>        start address for perturbation of temperature
!!         @f$ \Theta = T - T_{0} @f$
        integer (kind=kint) :: i_par_temp =        izero
!>        start address for reference temperature
!!         @f$  T_{0} @f$
        integer (kind=kint) :: i_ref_t =           izero
!
!>        start address for compostiion variation
!!         @f$ C @f$
        integer (kind=kint) :: i_light =           izero
!>        start address for perturbation of composition
!!         @f$  C - C_{0} @f$
        integer (kind=kint) :: i_par_light =       izero
!>        start address for reference temperature
!!         @f$  C_{0} @f$
        integer (kind=kint) :: i_ref_c =           izero
!
!>        start address for density
!!         @f$ \rho @f$
        integer (kind=kint) :: i_density =         izero
!>        start address for perturbation of density
!!         @f$  \rho - \rho_{0} @f$
        integer (kind=kint) :: i_par_density =     izero
!>        start address for reference density
!!         @f$  \rho_{0} @f$
        integer (kind=kint) :: i_ref_density =     izero
!
!>        start address for entropy
!!         @f$ S @f$
        integer (kind=kint) :: i_entropy =         izero
!>        start address for perturbation of entropy
!!         @f$  S - S_{0} @f$
        integer (kind=kint) :: i_par_entropy =     izero
!>        start address for reference entropy
!!         @f$  S_{0} @f$
        integer (kind=kint) :: i_ref_entropy =     izero
!
!>        start address for heat source
!!         @f$ q_{T} @f$
        integer (kind=kint) :: i_heat_source =     izero
!>        start address for composion source
!!         @f$ q_{C} @f$
        integer (kind=kint) :: i_light_source =    izero
!>        start address for entropysource
!!         @f$ q_{S} @f$
        integer (kind=kint) :: i_entropy_source =  izero
      end type base_field_address
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function missing_field                       &
     &                   (iphys_tgt, target_name, iphys_ref, ref_name)
!
      integer(kind = kint), intent(in) :: iphys_tgt, iphys_ref
      character(len = kchara), intent(in) :: target_name, ref_name
!
      missing_field = 0
      if(iphys_ref .gt. 0) return
      write(*,*) iphys_tgt, ': Following fields are required for ',     &
     &          trim(target_name), ': ', trim(ref_name)
      missing_field = 1
      return
!
      end function missing_field
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      logical function check_base_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_vector = .FALSE.
      if (    (field_name .eq. fhd_velo)                                &
     &   .or. (field_name .eq. fhd_vort)                                &
     &   .or. (field_name .eq. fhd_magne)                               &
     &   .or. (field_name .eq. fhd_vecp)                                &
     &   .or. (field_name .eq. fhd_current)                             &
     &      )   check_base_vector = .TRUE.
!
      end function check_base_vector
!
! ----------------------------------------------------------------------
!
      logical function check_base_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_base_scalar = .FALSE.
      if (    (field_name .eq. fhd_press)                               &
     &   .or. (field_name .eq. fhd_mag_potential)                       &
     &   .or. (field_name .eq. fhd_scalar_potential)                    &
!
     &   .or. (field_name .eq. fhd_density)                             &
     &   .or. (field_name .eq. fhd_per_density)                         &
     &   .or. (field_name .eq. fhd_ref_density)                         &
!
     &   .or. (field_name .eq. fhd_temp)                                &
     &   .or. (field_name .eq. fhd_part_temp)                           &
     &   .or. (field_name .eq. fhd_ref_temp)                            &
!
     &   .or. (field_name .eq. fhd_light)                               &
     &   .or. (field_name .eq. fhd_part_light)                          &
     &   .or. (field_name .eq. fhd_ref_light)                           &
!
     &   .or. (field_name .eq. fhd_entropy)                             &
     &   .or. (field_name .eq. fhd_per_entropy)                         &
     &   .or. (field_name .eq. fhd_ref_entropy)                         &
!
     &   .or. (field_name .eq. fhd_heat_source)                         &
     &   .or. (field_name .eq. fhd_light_source)                        &
     &   .or. (field_name .eq. fhd_entropy_source)                      &
     &      )   check_base_scalar = .TRUE.
!
      end function check_base_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_base_vector_addresses                              &
     &         (i_phys, field_name, base_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_vector(field_name)
      if(flag) then
        if (field_name .eq. fhd_velo) then
          base_fld%i_velo = i_phys
        else if (field_name .eq. fhd_vort) then
          base_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_magne) then
          base_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_vecp) then
          base_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_current) then
          base_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine set_base_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_base_scalar_addresses                              &
     &         (i_phys, field_name, base_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: base_fld
      logical, intent(inout) :: flag
!
!
      flag = check_base_scalar(field_name)
      if(flag) then
        if (field_name .eq. fhd_press) then
          base_fld%i_press = i_phys
        else if (field_name .eq. fhd_mag_potential) then
          base_fld%i_mag_p =    i_phys
        else if (field_name .eq. fhd_scalar_potential) then
          base_fld%i_scalar_p = i_phys
!
        else if (field_name .eq. fhd_temp) then
          base_fld%i_temp =            i_phys
        else if (field_name .eq. fhd_part_temp) then
          base_fld%i_par_temp =        i_phys
        else if (field_name .eq. fhd_ref_temp) then
          base_fld%i_ref_t =           i_phys
!
        else if (field_name .eq. fhd_light) then
          base_fld%i_light =          i_phys
        else if (field_name .eq. fhd_part_light) then
          base_fld%i_par_light =      i_phys
        else if (field_name .eq. fhd_ref_light) then
          base_fld%i_ref_c =          i_phys
!
        else if (field_name .eq. fhd_density) then
          base_fld%i_density =        i_phys
        else if (field_name .eq. fhd_per_density) then
          base_fld%i_par_density =    i_phys
        else if (field_name .eq. fhd_ref_density) then
          base_fld%i_ref_density =    i_phys
!
        else if (field_name .eq. fhd_entropy) then
          base_fld%i_entropy =        i_phys
        else if (field_name .eq. fhd_per_entropy) then
          base_fld%i_par_entropy =    i_phys
        else if (field_name .eq. fhd_ref_entropy) then
          base_fld%i_ref_entropy =    i_phys
!
        else if (field_name .eq. fhd_heat_source) then
          base_fld%i_heat_source =    i_phys
        else if (field_name .eq. fhd_light_source) then
          base_fld%i_light_source =   i_phys
        else if (field_name .eq. fhd_entropy_source) then
          base_fld%i_entropy_source = i_phys
        end if
      end if  
!
      end subroutine set_base_scalar_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine base_vector_monitor_address                            &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_base, ave_base, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_base
      type(base_field_address), intent(inout) :: ave_base
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_base_vector_addresses                                    &
     &   ((numrms+1), field_name, rms_base, flag_r)
      call set_base_vector_addresses                                    &
     &   ((numave+1), field_name, ave_base, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine base_vector_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine base_scalar_monitor_address                            &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_base, ave_base, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_base
      type(base_field_address), intent(inout) :: ave_base
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_base_scalar_addresses                                    &
     &   ((numrms+1), field_name, rms_base, flag_r)
      call set_base_scalar_addresses                                    &
     &   ((numave+1), field_name, ave_base, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine base_scalar_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_base_fields()
      num_base_fields = nfld_base
      return
      end function num_base_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names(field_names)
!
      character(len = kchara), intent(inout) :: field_names(nfld_base)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_velo), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_vort), CHAR(0)
      write(field_names( 3),'(a,a1)') trim(fhd_press), CHAR(0)
!
      write(field_names( 4),'(a,a1)') trim(fhd_magne), CHAR(0)
      write(field_names( 5),'(a,a1)') trim(fhd_vecp), CHAR(0)
      write(field_names( 6),'(a,a1)') trim(fhd_current), CHAR(0)
      write(field_names( 7),'(a,a1)') trim(fhd_mag_potential), CHAR(0)
      write(field_names( 8),'(a,a1)')                                   &
     &                            trim(fhd_scalar_potential), CHAR(0)
!
      write(field_names( 9),'(a,a1)') trim(fhd_temp), CHAR(0)
      write(field_names(10),'(a,a1)') trim(fhd_part_temp), CHAR(0)
      write(field_names(11),'(a,a1)') trim(fhd_ref_temp), CHAR(0)
      write(field_names(12),'(a,a1)') trim(fhd_light), CHAR(0)
      write(field_names(13),'(a,a1)') trim(fhd_part_light), CHAR(0)
      write(field_names(14),'(a,a1)') trim(fhd_ref_light), CHAR(0)
      write(field_names(15),'(a,a1)') trim(fhd_entropy), CHAR(0)
      write(field_names(16),'(a,a1)') trim(fhd_per_entropy), CHAR(0)
      write(field_names(17),'(a,a1)') trim(fhd_ref_entropy), CHAR(0)
      write(field_names(18),'(a,a1)') trim(fhd_density), CHAR(0)
      write(field_names(19),'(a,a1)') trim(fhd_per_density), CHAR(0)
      write(field_names(20),'(a,a1)') trim(fhd_ref_density), CHAR(0)
!
      write(field_names(21),'(a,a1)') trim(fhd_heat_source), CHAR(0)
      write(field_names(22),'(a,a1)') trim(fhd_light_source), CHAR(0)
      write(field_names(23),'(a,a1)') trim(fhd_entropy_source), CHAR(0)
!
      end subroutine set_base_field_names
!
! ----------------------------------------------------------------------
!
      end module t_base_field_labels
