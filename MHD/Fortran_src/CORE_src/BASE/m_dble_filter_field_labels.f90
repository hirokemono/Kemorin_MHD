!>@file   m_dble_filter_field_labels.f90
!!        module m_dble_filter_field_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      logical function check_double_vector_scalar(field_name)
!!      logical function check_double_filter_scalar(field_name)
!!      subroutine set_dble_fil_vector_addresses                        &
!!     &         (i_phys, field_name, dbl_filter_fld, flag)
!!      subroutine set_dble_fil_scaler_addresses                        &
!!     &         (i_phys, field_name, dbl_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: dbl_filter_fld
!!
!!      subroutine dble_fil_vec_monitor_address                         &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_d_filter_fld, ave_d_filter_fld, flag)
!!      subroutine dble_fil_scl_monitor_address                         &
!!     &         (field_name, i_field, numrms, numave,                  &
!!     &          rms_d_filter_fld, ave_d_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: rms_d_filter_fld
!!        type(base_field_address), intent(inout) :: ave_d_filter_fld
!!
!!      integer(kind = kint) function num_double_filter_fields()
!!      subroutine set_double_filter_field_names(field_names)
!!
!! !!!!!  Double filterd field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!   double_filter_velo
!!       [i_velo]:     filtered velocity    u
!!   double_filter_vorticity
!!       [i_vort]:    filtered vorticity   \omega = \nabra \times v
!!
!!   double_filter_vecp 
!!       [i_vecp] :   filtered vector potential \nabla \times A = B
!!   double_filter_magne
!!       [i_magne]:     filtered magnetic field   B
!!   double_filter_current
!!       [i_current]:    filtered current density  J = \nabla \times B
!!
!!   double_filter_temp
!!       [i_temp]:  filtered temperature              T
!!   double_filter_composition
!!       [i_light]:  filtered Composition anormally   C
!!   double_filter_density
!!       [i_density]:      filtered density              \rho
!!   double_filter_entropy
!!       [i_entropy]:      filtered Entropy               S
!!
!!   double_filter_pert_temp  [i_par_temp]:         \Theta = T - T_0
!!   double_filter_pert_comp  [i_par_light]:         C - C_0
!!   double_filter_pert_density [i_par_density]:      \rho - \rho_0
!!   double_filter_pert_entropy,  [i_par_entropy]:      S - S_0
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_dble_filter_field_labels
!
      use m_precision
      use m_constants
      use t_base_field_labels
!
      implicit  none
! 
!
      integer(kind = kint), parameter, private :: nfld_d_filter = 13
!
!  double filtered field
!
!>        Field label for filtered velocity by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_velo = 'double_filter_velo'
!>        Field label for filtered vorticity by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_vort = 'double_filter_vorticity'
!>        Field label for filtered magnetic vector potential
!!        by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_vecp = 'double_filter_vecp'
!>        Field label for filtered magnetic field by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_magne = 'double_filter_magne'
!>        Field label for filtered current density by double filtering
      character(len=kchara), parameter                                  &
     &             :: fhd_d_filter_current = 'double_filter_current'
!
!>        Field label for filtered temperature by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_temp =      'double_filter_temp'
!>        Field label for filtered perturbation of temperature
!!        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_pert_temp = 'double_filter_pert_temp'
!
!>        Field label for filtered compostiion by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_comp =    'double_filter_composition'
!>        Field label for filtered perturbation of composition
!>        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_pert_comp = 'double_filter_pert_comp'
!
!>        Field label for filtered density by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_density =    'double_filter_density'
!>        Field label for filtered perturbation of density
!>        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_pert_density = 'double_filter_pert_density'
!
!>        Field label for filtered entropy by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_entropy =    'double_filter_entropy'
!>        Field label for filtered perturbation of entropy
!>        by double filtering
      character(len=kchara), parameter                                  &
     &      :: fhd_d_filter_pert_entropy = 'double_filter_pert_entropy'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_double_vector_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_vector_scalar = .FALSE.
      if (    (field_name .eq. fhd_d_filter_velo)                       &
     &   .or. (field_name .eq. fhd_d_filter_vort)                       &
     &   .or. (field_name .eq. fhd_d_filter_magne)                      &
     &   .or. (field_name .eq. fhd_d_filter_vecp)                       &
     &   .or. (field_name .eq. fhd_d_filter_current)                    &
     &      )   check_double_vector_scalar = .TRUE.
!
      end function check_double_vector_scalar
!
! ----------------------------------------------------------------------
!
      logical function check_double_filter_scalar(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_double_filter_scalar = .FALSE.
      if (    (field_name .eq. fhd_d_filter_temp)                       &
     &   .or. (field_name .eq. fhd_d_filter_comp)                       &
     &   .or. (field_name .eq. fhd_d_filter_density)                    &
     &   .or. (field_name .eq. fhd_d_filter_entropy)                    &
!
     &   .or. (field_name .eq. fhd_d_filter_pert_temp)                  &
     &   .or. (field_name .eq. fhd_d_filter_pert_comp)                  &
     &   .or. (field_name .eq. fhd_d_filter_pert_density)               &
     &   .or. (field_name .eq. fhd_d_filter_pert_entropy)               &
     &      )   check_double_filter_scalar = .TRUE.
!
      end function check_double_filter_scalar
!
! ----------------------------------------------------------------------
!
      subroutine set_dble_fil_vector_addresses                          &
     &         (i_phys, field_name, dbl_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: dbl_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_double_vector_scalar(field_name)
      if(flag) then
        if (field_name .eq. fhd_d_filter_velo) then
          dbl_filter_fld%i_velo = i_phys
        else if (field_name .eq. fhd_d_filter_vort) then
          dbl_filter_fld%i_vort = i_phys
!
        else if (field_name .eq. fhd_d_filter_magne) then
          dbl_filter_fld%i_magne =    i_phys
        else if (field_name .eq. fhd_d_filter_vecp) then
          dbl_filter_fld%i_vecp =     i_phys
        else if (field_name .eq. fhd_d_filter_current) then
          dbl_filter_fld%i_current =  i_phys
        end if
      end if
!
      end subroutine set_dble_fil_vector_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_dble_fil_scaler_addresses                          &
     &         (i_phys, field_name, dbl_filter_fld, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: dbl_filter_fld
      logical, intent(inout) :: flag
!
!
      flag = check_double_filter_scalar(field_name)
      if(flag) then
        if      (field_name .eq. fhd_d_filter_temp) then
          dbl_filter_fld%i_temp =            i_phys
        else if (field_name .eq. fhd_d_filter_pert_temp) then
          dbl_filter_fld%i_par_temp =        i_phys
!
        else if (field_name .eq. fhd_d_filter_comp) then
          dbl_filter_fld%i_light =          i_phys
        else if (field_name .eq. fhd_d_filter_pert_comp) then
          dbl_filter_fld%i_par_light =      i_phys
!
        else if (field_name .eq. fhd_d_filter_density) then
          dbl_filter_fld%i_density =        i_phys
        else if (field_name .eq. fhd_d_filter_pert_density) then
          dbl_filter_fld%i_par_density =    i_phys
!
        else if (field_name .eq. fhd_d_filter_entropy) then
          dbl_filter_fld%i_entropy =        i_phys
        else if (field_name .eq. fhd_d_filter_pert_entropy) then
          dbl_filter_fld%i_par_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_dble_fil_scaler_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dble_fil_vec_monitor_address                           &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_d_filter_fld, ave_d_filter_fld, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_d_filter_fld
      type(base_field_address), intent(inout) :: ave_d_filter_fld
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_dble_fil_vector_addresses                                &
     &   ((numrms+1), field_name, rms_d_filter_fld, flag_r)
      call set_dble_fil_vector_addresses                                &
     &   ((numave+1), field_name, ave_d_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine dble_fil_vec_monitor_address
!
! ----------------------------------------------------------------------
!
      subroutine dble_fil_scl_monitor_address                           &
     &         (field_name, i_field, numrms, numave,                    &
     &          rms_d_filter_fld, ave_d_filter_fld, flag)
!
      character(len = kchara), intent(in):: field_name
      integer(kind = kint), intent(in) :: i_field
      integer(kind = kint), intent(in) :: numrms, numave
!
      type(base_field_address), intent(inout) :: rms_d_filter_fld
      type(base_field_address), intent(inout) :: ave_d_filter_fld
      logical, intent(inout) :: flag
!
      logical :: flag_a, flag_r
!
!
      flag = .FALSE.
!
      if(i_field .eq. 0) return
      call set_dble_fil_scaler_addresses                                &
     &   ((numrms+1), field_name, rms_d_filter_fld, flag_r)
      call set_dble_fil_scaler_addresses                                &
     &   ((numave+1), field_name, ave_d_filter_fld, flag_a)
      flag = (flag_r .and. flag_a)
!
      end subroutine dble_fil_scl_monitor_address
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_double_filter_fields()
      num_double_filter_fields = nfld_d_filter
      return
      end function num_double_filter_fields
!
! ----------------------------------------------------------------------
!
      subroutine set_double_filter_field_names(field_names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: field_names(nfld_d_filter)
!
!
      write(field_names( 1),'(a,a1)') trim(fhd_d_filter_velo), CHAR(0)
      write(field_names( 2),'(a,a1)') trim(fhd_d_filter_vort), CHAR(0)
!
      write(field_names( 3),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_magne), CHAR(0)
      write(field_names( 4),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_vecp), CHAR(0)
      write(field_names( 5),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_current), CHAR(0)
!
      write(field_names( 6),'(a,a1)') trim(fhd_d_filter_temp), CHAR(0)
      write(field_names( 7),'(a,a1)') trim(fhd_d_filter_comp), CHAR(0)
      write(field_names( 8),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_density), CHAR(0)
      write(field_names( 9),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_entropy), CHAR(0)
      write(field_names(10),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_pert_temp), CHAR(0)
      write(field_names(11),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_pert_comp), CHAR(0)
      write(field_names(12),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_pert_density), CHAR(0)
      write(field_names(13),'(a,a1)')                                   &
     &                   trim(fhd_d_filter_pert_entropy), CHAR(0)
!
      end subroutine set_double_filter_field_names
!
! ----------------------------------------------------------------------
!
      end module m_dble_filter_field_labels
