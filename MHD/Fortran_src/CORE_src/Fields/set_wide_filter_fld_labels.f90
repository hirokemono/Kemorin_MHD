!>@file   set_wide_filter_fld_labels.f90
!!        module set_wide_filter_fld_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for wide and double filttered fields
!!
!!@verbatim
!!      subroutine set_wide_fil_field_addresses                         &
!!     &         (i_phys, field_name, wide_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: wide_filter_fld
!!      subroutine set_dble_fil_field_addresses                         &
!!     &         (i_phys, field_name, dbl_filter_fld, flag)
!!        type(base_field_address), intent(inout) :: dbl_filter_fld
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  Wide fitered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!   wide_filter_velocity          [wide_filter_fld%i_velo]
!!   wide_filter_vorticity         [wide_filter_fld%i_vort]
!!
!!   wide_filter_magne             [wide_filter_fld%i_magne]
!!   wide_filter_vector_potential  [wide_filter_fld%i_vecp]
!!   wide_filter_current           [wide_filter_fld%i_current]
!!
!!   wide_filter_temp              [wide_filter_fld%i_temp]
!!   wide_filter_composition       [wide_filter_fld%i_light]
!!   wide_filter_density           [wide_filter_fld%i_density]
!!   wide_filter_entropy           [wide_filter_fld%i_entropy]
!!
!!   wide_filter_pert_temp         [wide_filter_fld%i_per_temp]
!!   wide_filter_pert_comp         [wide_filter_fld%i_per_light]
!!   wide_filter_pert_density      [wide_filter_fld%i_per_density]
!!   wide_filter_pert_entropy      [wide_filter_fld%i_per_entropy]
!!
!!    wide_filter_grad_temp        [wide_filter_grad%i_grad_temp]
!!    wide_filter_grad_composition [wide_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  Double filterd field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!   double_filter_velocity          [dbl_filter_fld%i_velo]
!!   double_filter_vorticity         [dbl_filter_fld%i_vort]
!!
!!   double_filter_magne             [dbl_filter_fld%i_magne]
!!   double_filter_vector_potential  [dbl_filter_fld%i_vecp]
!!   double_filter_current           [dbl_filter_fld%i_current]
!!
!!   double_filter_temp              [dbl_filter_fld%i_temp]
!!   double_filter_composition       [dbl_filter_fld%i_light]
!!   double_filter_density           [dbl_filter_fld%i_density]
!!   double_filter_entropy           [dbl_filter_fld%i_entropy]
!!
!!   double_filter_pert_temp         [dbl_filter_fld%i_per_temp]
!!   double_filter_pert_comp         [dbl_filter_fld%i_per_light]
!!   double_filter_pert_density      [dbl_filter_fld%i_per_density]
!!   double_filter_pert_entropy      [dbl_filter_fld%i_per_entropy]
!!
!!   double_filter_grad_temp [dbl_filter_grad%i_grad_temp]
!!   double_filter_grad_comp [dbl_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module set_wide_filter_fld_labels
!
      use m_precision
      use m_phys_constants
      use t_base_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_field_addresses                           &
     &         (i_phys, field_name, wide_filter_fld, flag)
!
      use m_wide_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: wide_filter_fld
      logical, intent(inout) :: flag
!
!
      flag =    check_wide_filter_vector(field_name)                    &
     &     .or. check_wide_filter_scalar(field_name)
      if(flag) then
        if (field_name .eq. wide_filter_velocity%name) then
          wide_filter_fld%i_velo =    i_phys
        else if(field_name .eq. wide_filter_vorticity%name) then
          wide_filter_fld%i_vort =    i_phys
!
        else if(field_name .eq. wide_filter_magne%name) then
          wide_filter_fld%i_magne =   i_phys
        else if(field_name .eq. wide_filter_vector_potential%name)    &
     &   then
          wide_filter_fld%i_vecp =    i_phys
        else if(field_name .eq. wide_filter_current%name) then
          wide_filter_fld%i_current = i_phys
!
        else if(field_name .eq. wide_filter_temp%name) then
          wide_filter_fld%i_temp =           i_phys
        else if (field_name .eq. wide_filter_pert_temp%name) then
          wide_filter_fld%i_per_temp =       i_phys
!
        else if (field_name .eq. wide_filter_composition%name) then
          wide_filter_fld%i_light =          i_phys
        else if (field_name .eq. wide_filter_pert_comp%name) then
          wide_filter_fld%i_per_light =      i_phys
!
        else if (field_name .eq. wide_filter_density%name) then
          wide_filter_fld%i_density =        i_phys
        else if (field_name .eq. wide_filter_pert_density%name) then
          wide_filter_fld%i_per_density =    i_phys
!
        else if (field_name .eq. wide_filter_entropy%name) then
          wide_filter_fld%i_entropy =        i_phys
        else if (field_name .eq. wide_filter_pert_entropy%name) then
          wide_filter_fld%i_per_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_wide_fil_field_addresses
!
! -----------------------------------------------------------------------
!
      subroutine set_dble_fil_field_addresses                           &
     &         (i_phys, field_name, dbl_filter_fld, flag)
!
      use m_dble_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_field_address), intent(inout) :: dbl_filter_fld
      logical, intent(inout) :: flag
!
!
      flag =    check_double_filter_scalar(field_name)                  &
     &     .or. check_double_filter_vector(field_name)
      if(flag) then
        if (field_name .eq. double_filter_velocity%name) then
          dbl_filter_fld%i_velo =     i_phys
        else if(field_name .eq. double_filter_vorticity%name) then
          dbl_filter_fld%i_vort =    i_phys
!
        else if(field_name .eq. double_filter_magne%name) then
          dbl_filter_fld%i_magne =   i_phys
        else if(field_name .eq. double_filter_vector_potential%name)    &
     &   then
          dbl_filter_fld%i_vecp =    i_phys
        else if(field_name .eq. double_filter_current%name) then
          dbl_filter_fld%i_current = i_phys
!
        else if(field_name .eq. double_filter_temp%name) then
          dbl_filter_fld%i_temp =           i_phys
        else if(field_name .eq. double_filter_pert_temp%name) then
          dbl_filter_fld%i_per_temp =       i_phys
!
        else if(field_name .eq. double_filter_composition%name) then
          dbl_filter_fld%i_light =          i_phys
        else if (field_name .eq. double_filter_pert_comp%name) then
          dbl_filter_fld%i_per_light =      i_phys
!
        else if(field_name .eq. double_filter_density%name) then
          dbl_filter_fld%i_density =        i_phys
        else if(field_name .eq. double_filter_pert_density%name) then
          dbl_filter_fld%i_per_density =    i_phys
!
        else if(field_name .eq. double_filter_entropy%name) then
          dbl_filter_fld%i_entropy =        i_phys
        else if(field_name .eq. double_filter_pert_entropy%name) then
          dbl_filter_fld%i_per_entropy =    i_phys
        end if
      end if  
!
      end subroutine set_dble_fil_field_addresses
!
! ----------------------------------------------------------------------
!
      end module set_wide_filter_fld_labels
