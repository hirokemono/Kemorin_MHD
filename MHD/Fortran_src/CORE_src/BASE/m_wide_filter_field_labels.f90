!>@file   m_wide_filter_field_labels.f90
!!        module m_wide_filter_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_wide_fil_vector(field_name)
!!      logical function check_wide_fil_scalar(field_name)
!!      subroutine set_wide_fil_vector_addresses                        &
!!     &         (i_phys, field_name, filter_fld, flag)
!!      subroutine set_wide_fil_scaler_addresses                        &
!!     &         (i_phys, field_name, filter_fld, flag)
!!        type(base_field_address), intent(inout) :: filter_fld
!!
!!      subroutine wide_fil_vec_monitor_address                         &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_filter_fld, ave_filter_fld, flag)
!!      subroutine wide_fil_scl_monitor_address                         &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_filter_fld, ave_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: rms_filter_fld
!!        type(base_field_address), intent(inout) :: ave_filter_fld
!!
!!      integer(kind = kint) function num_wide_filter_fields()
!!      subroutine set_wide_filter_field_names(field_names)
!!
!! !!!!!  Wide fitered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!   wide_filter_velo
!!       [i_velo]:     filtered velocity    u
!!   wide_filter_vorticity
!!       [i_vort]:    filtered vorticity   \omega = \nabra \times v
!!
!!   wide_filter_vecp
!!       [i_vecp] :   filtered vector potential \nabla \times A = B
!!   wide_filter_magne
!!       [i_magne]:     filtered magnetic field   B
!!   wide_filter_current
!!       [i_current]:    filtered current density  J = \nabla \times B
!!
!!   wide_filter_temp
!!       [i_temp]:  filtered temperature              T
!!   wide_filter_composition
!!       [i_light]:  filtered Composition anormally   C
!!   wide_filter_density
!!       [i_density]:      filtered density              \rho
!!   wide_filter_entropy
!!       [i_entropy]:      filtered Entropy               S
!!
!!   wide_filter_part_temp      [i_par_temp]:         \Theta = T - T_0
!!    wide_filter_part_comp     [i_par_light]:        C - C_0
!!    wide_filter_part_density  [i_par_density]:      \rho - \rho_0
!!    wide_filter_part_entropy, [i_par_entropy]:      S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_wide_filter_field_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_w_filter = 13
!
!  wider filtered field
!
!>        Field label for filtered velocity by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_velo = 'wide_filter_velo'
!>        Field label for filtered vorticity by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_vort = 'wide_filter_vorticity'
!>        Field label for filtered magnetic vector potential
!!        by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_vecp = 'wide_filter_vecp'
!>        Field label for filtered magnetic field by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_magne = 'wide_filter_magne'
!>        Field label for filtered current density by wider filter
      character(len=kchara), parameter                                  &
     &             :: fhd_w_filter_current = 'wide_filter_current'
!
!>        Field label for filtered temperature by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_temp =      'wide_filter_temp'
!>        Field label for filtered perturbation of temperature
!!                                                  by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_part_temp = 'wide_filter_part_temp'
!
!>        Field label for filtered compostiion by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_comp =    'wide_filter_composition'
!>        Field label for filtered perturbation of composition
!!                                                  by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_part_comp = 'wide_filter_part_comp'
!
!>        Field label for filtered density by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_density =    'wide_filter_density'
!>        Field label for filtered perturbation of density
!!                                                  by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_part_density = 'wide_filter_part_density'
!
!>        Field label for filtered entropy by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_entropy =    'wide_filter_entropy'
!>        Field label for filtered perturbation of entropy
!!                                                  by wider filter
      character(len=kchara), parameter                                  &
     &      :: fhd_w_filter_part_entropy = 'wide_filter_part_entropy'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_wide_fil_vector(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_fil_vector = .FALSE.
      if (    (field_name .eq. fhd_w_filter_velo)                       &
     &   .or. (field_name .eq. fhd_w_filter_vort)                       &
     &   .or. (field_name .eq. fhd_w_filter_vecp)                       &
     &   .or. (field_name .eq. fhd_w_filter_magne)                      &
     &   .or. (field_name .eq. fhd_w_filter_current)                    &
     &      )   check_wide_fil_vector = .TRUE.
!
      end function check_wide_fil_vector
!
! ----------------------------------------------------------------------
!
      logical function check_wide_fil_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_wide_fil_scalar = .FALSE.
      if (    (field_name .eq. fhd_w_filter_temp)                       &
     &   .or. (field_name .eq. fhd_w_filter_comp)                       &
     &   .or. (field_name .eq. fhd_w_filter_density)                    &
     &   .or. (field_name .eq. fhd_w_filter_entropy)                    &
!
     &   .or. (field_name .eq. fhd_w_filter_part_temp)                  &
     &   .or. (field_name .eq. fhd_w_filter_part_comp)                  &
     &   .or. (field_name .eq. fhd_w_filter_part_density)               &
     &   .or. (field_name .eq. fhd_w_filter_part_entropy)               &
     &      )   check_wide_fil_scalar = .TRUE.
!
      end function check_wide_fil_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_vector_addresses                          &
     &         (i_phys, field_name, filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_wide_fil_vector(field_name)
      if(flag) then
        if (field_name .eq. fhd_w_filter_velo) then
          filter_fld%i_velo = i_phys
        else if (field_name .eq. fhd_w_filter_vort) then
          filter_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_w_filter_magne) then
          filter_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_w_filter_vecp) then
          filter_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_w_filter_current) then
          filter_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine set_wide_fil_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_scaler_addresses                          &
     &         (i_phys, field_name, filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_wide_fil_scalar(field_name)
      if(flag) then
        if      (field_name .eq. fhd_w_filter_temp) then
          filter_fld%i_temp =            i_phys
        else if (field_name .eq. fhd_w_filter_part_temp) then
          filter_fld%i_par_temp =        i_phys
!
        else if (field_name .eq. fhd_w_filter_comp) then
          filter_fld%i_light =          i_phys
        else if (field_name .eq. fhd_w_filter_part_comp) then
          filter_fld%i_par_light =      i_phys
!
        else if (field_name .eq. fhd_w_filter_density) then
          filter_fld%i_density =        i_phys
        else if (field_name .eq. fhd_w_filter_part_density) then
          filter_fld%i_par_density =    i_phys
!
        else if (field_name .eq. fhd_w_filter_entropy) then
          filter_fld%i_entropy =        i_phys
        else if (field_name .eq. fhd_w_filter_part_entropy) then
          filter_fld%i_par_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_wide_fil_scaler_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine wide_fil_vec_monitor_address                           &
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
      call set_wide_fil_vector_addresses                                &
     &   ((numrms+1), field_name, rms_filter_fld, flag_r)
      call set_wide_fil_vector_addresses                                &
     &   ((numave+1), field_name, ave_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine wide_fil_vec_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine wide_fil_scl_monitor_address                           &
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
      call set_wide_fil_scaler_addresses                                &
     &   ((numrms+1), field_name, rms_filter_fld, flag_r)
      call set_wide_fil_scaler_addresses                                &
     &   ((numave+1), field_name, ave_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine wide_fil_scl_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_wide_filter_fields()
      num_wide_filter_fields = nfld_w_filter
      return
      end function num_wide_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_filter_field_names(field_names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: field_names(nfld_w_filter)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_w_filter_velo), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_w_filter_vort), CHAR(0)
!
      write(field_names( 3),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_magne), CHAR(0)
      write(field_names( 4),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_vecp), CHAR(0)
      write(field_names( 5),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_current), CHAR(0)
!
      write(field_names( 6),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_temp), CHAR(0)
      write(field_names( 7),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_comp), CHAR(0)
      write(field_names( 8),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_density), CHAR(0)
      write(field_names( 9),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_entropy), CHAR(0)
!
      write(field_names(10),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_part_temp), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_part_comp), CHAR(0)
      write(field_names(12),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_part_density), CHAR(0)
      write(field_names(13),'(a,a1)')                                   &
     &                   trim(fhd_w_filter_part_entropy), CHAR(0)
!
      end subroutine set_wide_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_wide_filter_field_labels
