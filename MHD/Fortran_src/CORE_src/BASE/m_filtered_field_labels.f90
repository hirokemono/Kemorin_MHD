!>@file   m_filtered_field_labels.f90
!!        module m_filtered_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_filter_vector(field_name)
!!      logical function check_filter_scalar(field_name)
!!      subroutine set_filter_vector_addresses                          &
!!     &         (i_phys, field_name, filter_fld, flag)
!!      subroutine set_filter_scalar_addresses                          &
!!     &         (i_phys, field_name, filter_fld, flag)
!!        type(base_field_address), intent(inout) :: filter_fld
!!
!!      subroutine filter_vector_monitor_address                        &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_filter_fld, ave_filter_fld, flag)
!!      subroutine filter_scalar_monitor_address                        &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_filter_fld, ave_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: rms_filter_fld
!!        type(base_field_address), intent(inout) :: ave_filter_fld
!!
!!      integer(kind = kint) function num_filter_fields()
!!      subroutine set_filter_field_names(field_names)
!!
!! !!!!!  Filtered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names (Single filtered, wide filtered, double filtered)
!!
!!   filter_velo   [i_velo]:     filtered velocity    u
!!   filter_vorticity   [i_vort]: 
!!            filtered vorticity   \omega = \nabra \times v
!!
!!   filter_vecp  [i_vecp]: 
!!            filtered vector potential \nabla \times A = B
!!   filter_magne  [i_magne]:     filtered magnetic field   B
!!   filter_current   [i_current]: 
!!            filtered current density  J = \nabla \times B
!!
!!   filter_temp    [i_temp]:  filtered temperature              T
!!   filter_composition  [i_light]:   filtered Composition anormally   C
!!   filter_density    [i_density]: filtered density              \rho
!!   filter_entropy    [i_entropy]: filtered Entropy               S
!!
!!   filter_pert_temp            [i_par_temp]:    \Theta = T - T_0
!!   filter_pert_comp            [i_par_light]:    C - C_0
!!   filter_pert_density         [i_par_density]:  \rho - \rho_0
!!   filter_pert_entropy         [i_par_entropy]:  S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_field_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_filter = 13
!
!  filtered field
!
!>        Field label for filtered velocity
!!         @f$ \bar{u}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_velo = 'filter_velo'
!>        Field label for filtered velocity
!!         @f$ \bar{\omega}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_vort = 'filter_vorticity'
!!
!>        Field label for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_magne = 'filter_magne'
!>        Field label for filtered magnetic field
!!         @f$ \bar{B}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_current = 'filter_current'
!>        Field label for filtered vetor potential
!!         @f$ \bar{A}_{i} @f$
      character(len=kchara), parameter                                  &
     &            :: fhd_filter_vecp = 'filter_vecp'
!
!
!>        Field label for filtered temperature
!!         @f$ \bar{T} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_temp =       'filter_temp'
!>        Field label for filtered perturbation of temperature
!!         @f$ \bar{\Theta} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_pert_temp =  'filter_pert_temp'
!
!>        Field label for filtered conposition
!!         @f$ \bar{C} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_comp =       'filter_composition'
!>        Field label for filtered conposition
!!         @f$ \bar{C} - C_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_pert_comp =  'filter_pert_comp'
!
!>        Field label for filtered conposition
!!         @f$ \bar{\rho} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_density =       'filter_density'
!>        Field label for filtered conposition
!!         @f$ \bar{\rho} - rho_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_pert_density =  'filter_pert_density'
!
!>        Field label for filtered entropy
!!         @f$ \bar{S} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_entropy =       'filter_entropy'
!>        Field label for filtered perturbation of entropy
!!         @f$ \bar{S} - S_{0} @f$
      character(len=kchara), parameter                                  &
     &             :: fhd_filter_pert_entropy =  'filter_pert_entropy'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filter_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_vector = .FALSE.
      if (    (field_name .eq. fhd_filter_velo)                         &
     &   .or. (field_name .eq. fhd_filter_vort)                         &
     &   .or. (field_name .eq. fhd_filter_magne)                        &
     &   .or. (field_name .eq. fhd_filter_vecp)                         &
     &   .or. (field_name .eq. fhd_filter_current)                      &
     &      )   check_filter_vector = .TRUE.
!
      end function check_filter_vector
!
! ----------------------------------------------------------------------
!
      logical function check_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_scalar = .FALSE.
      if (    (field_name .eq. fhd_filter_temp)                         &
     &   .or. (field_name .eq. fhd_filter_comp)                         &
     &   .or. (field_name .eq. fhd_filter_density)                      &
     &   .or. (field_name .eq. fhd_filter_entropy)                      &
!
     &   .or. (field_name .eq. fhd_filter_pert_temp)                    &
     &   .or. (field_name .eq. fhd_filter_pert_comp)                    &
     &   .or. (field_name .eq. fhd_filter_pert_density)                 &
     &   .or. (field_name .eq. fhd_filter_pert_entropy)                 &
     &      )   check_filter_scalar = .TRUE.
!
      end function check_filter_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_vector_addresses                            &
     &         (i_phys, field_name, filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_filter_vector(field_name)
      if(flag) then
        if (field_name .eq. fhd_filter_velo) then
          filter_fld%i_velo = i_phys
        else if (field_name .eq. fhd_filter_vort) then
          filter_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_filter_magne) then
          filter_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_filter_vecp) then
          filter_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_filter_current) then
          filter_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine set_filter_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_scalar_addresses                            &
     &         (i_phys, field_name, filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_filter_scalar(field_name)
      if(flag) then
        if      (field_name .eq. fhd_filter_temp) then
          filter_fld%i_temp =            i_phys
        else if (field_name .eq. fhd_filter_pert_temp) then
          filter_fld%i_par_temp =        i_phys
!
        else if (field_name .eq. fhd_filter_comp) then
          filter_fld%i_light =          i_phys
        else if (field_name .eq. fhd_filter_pert_comp) then
          filter_fld%i_par_light =      i_phys
!
        else if (field_name .eq. fhd_filter_density) then
          filter_fld%i_density =        i_phys
        else if (field_name .eq. fhd_filter_pert_density) then
          filter_fld%i_par_density =    i_phys
!
        else if (field_name .eq. fhd_filter_entropy) then
          filter_fld%i_entropy =        i_phys
        else if (field_name .eq. fhd_filter_pert_entropy) then
          filter_fld%i_par_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_filter_scalar_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine filter_vector_monitor_address                          &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_filter_fld, ave_filter_fld, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_filter_fld
      type(base_field_address), intent(inout) :: ave_filter_fld
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_filter_vector_addresses                                  &
     &   ((numrms+1), field_name, rms_filter_fld, flag_r)
      call set_filter_vector_addresses                                  &
     &   ((numave+1), field_name, ave_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine filter_vector_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine filter_scalar_monitor_address                          &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_filter_fld, ave_filter_fld, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_filter_fld
      type(base_field_address), intent(inout) :: ave_filter_fld
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_filter_scalar_addresses                                  &
     &   ((numrms+1), field_name, rms_filter_fld, flag_r)
      call set_filter_scalar_addresses                                  &
     &   ((numave+1), field_name, ave_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine filter_scalar_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_filter_fields()
      num_filter_fields = nfld_filter
      return
      end function num_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_filter_field_names(field_names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: field_names(nfld_filter)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_filter_velo), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_filter_vort), CHAR(0)
!
      write(field_names( 3),'(a,a1)') trim(fhd_filter_magne), CHAR(0)
      write(field_names( 4),'(a,a1)') trim(fhd_filter_vecp), CHAR(0)
      write(field_names( 5),'(a,a1)') trim(fhd_filter_current), CHAR(0)
!
      write(field_names( 6),'(a,a1)') trim(fhd_filter_temp), CHAR(0)
      write(field_names( 7),'(a,a1)') trim(fhd_filter_comp), CHAR(0)
      write(field_names( 8),'(a,a1)') trim(fhd_filter_density), CHAR(0)
      write(field_names( 9),'(a,a1)') trim(fhd_filter_entropy), CHAR(0)
!
      write(field_names(10),'(a,a1)')                                   &
     &                   trim(fhd_filter_pert_temp), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                   trim(fhd_filter_pert_comp), CHAR(0)
      write(field_names(12),'(a,a1)')                                   &
     &                   trim(fhd_filter_pert_density), CHAR(0)
      write(field_names(13),'(a,a1)')                                   &
     &                   trim(fhd_filter_pert_entropy), CHAR(0)
!
      end subroutine set_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_filtered_field_labels
