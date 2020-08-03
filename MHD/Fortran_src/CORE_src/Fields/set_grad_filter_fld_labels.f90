!>@file   set_grad_filter_fld_labels.f90
!!       module set_grad_filter_fld_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields of gradient of filtered field
!!
!!@verbatim
!!      subroutine set_grad_filter_field_addresses                      &
!!     &         (i_phys, field_name, grad_fil_fld, flag)
!!        type(gradient_field_address), intent(inout) :: grad_fil_fld
!!      subroutine set_wide_fil_grad_addresses                          &
!!     &         (i_phys, field_name, wide_filter_grad, flag)
!!        type(gradient_field_address), intent(inout) :: wide_filter_grad
!!      subroutine set_dble_fil_grad_addresses                          &
!!     &         (i_phys, field_name, dbl_filter_grad, flag)
!!        type(gradient_field_address), intent(inout) :: dbl_filter_grad
!!
!!      subroutine set_diff_filter_vect_addresses                       &
!!     &         (i_phys, field_name, diff_fil_vect, flag)
!!        type(diff_vector_address), intent(inout) :: diff_fil_vect
!!
!! !!!!! gradient of filtered field !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names  [Address]
!!
!!  div_filtered_velo               [grad_fil_fld%i_div_v]
!!  div_filtered_magne              [grad_fil_fld%i_div_b]
!!  div_filtered_vector_potential   [grad_fil_fld%i_div_a]
!!
!!  grad_filtered_temp            [grad_fil_fld%i_grad_t]
!!  grad_filtered_pert_temp       [grad_fil_fld%i_grad_per_t]
!!
!!  grad_filtered_comp            [grad_fil_fld%i_grad_composit]
!!  grad_filtered_pert_comp       [grad_fil_fld%i_grad_per_c]
!!
!!  grad_filtered_density          [grad_fil_fld%i_grad_density]
!!  grad_filtered_pert_density     [grad_fil_fld%i_grad_per_density]
!!
!!  grad_filtered_entropy          [grad_fil_fld%i_grad_entropy]
!!  grad_filtered_pert_entropy     [grad_fil_fld%i_grad_per_entropy]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  Wide fitered field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!    wide_filter_grad_temp        [wide_filter_grad%i_grad_temp]
!!    wide_filter_grad_composition [wide_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! !!!!!  Double filterd field names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      Field label  [Address]
!!
!!   double_filter_grad_temp [dbl_filter_grad%i_grad_temp]
!!   double_filter_grad_comp [dbl_filter_grad%i_grad_composit]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! !!!!!  diffrence of filtered vector fields !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names [Address]
!!
!!   grad_filtered_v_1, grad_filtered_v_2, grad_filtered_v_3
!!           [diff_fil_vect%i_grad_vx], [diff_fil_vect%i_grad_vy],
!!           [diff_fil_vect%i_grad_vz]:       difference of velocity
!!   grad_filtered_w_1, grad_filtered_w_2, grad_filtered_w_3
!!           [diff_fil_vect%i_grad_wx], [diff_fil_vect%i_grad_wy],
!!           [diff_fil_vect%i_grad_wz]:       difference of vorticity
!!   grad_filtered_b_1, grad_filtered_b_2, grad_filtered_b_3
!!           [diff_fil_vect%i_grad_bx], [diff_fil_vect%i_grad_by],
!!           [diff_fil_vect%i_grad_bz]:    difference of magnetic field
!!   grad_filtered_a_1, grad_filtered_a_2, grad_filtered_a_3
!!           [diff_fil_vect%i_grad_ax], [diff_fil_vect%i_grad_ay],
!!           [diff_fil_vect%i_grad_az]:    difference of vector potential
!!   grad_filtered_j_1, grad_filtered_j_2, grad_filtered_j_3
!!           [diff_fil_vect%i_grad_jx], [diff_fil_vect%i_grad_jy],
!!           [diff_fil_vect%i_grad_jz]:    difference of current density
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module set_grad_filter_fld_labels
!
      use m_precision
      use m_phys_constants
      use t_grad_field_labels
      use t_diff_vector_labels
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_grad_filter_field_addresses                        &
     &         (i_phys, field_name, grad_fil_fld, flag)
!
      use m_grad_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: grad_fil_fld
      logical, intent(inout) :: flag
!
!
      flag = check_div_filter_field(field_name)                         &
     &      .or. check_grad_filter_field(field_name)
      if(flag) then
        if     (field_name .eq. div_filtered_velo%name) then
          grad_fil_fld%i_div_v =    i_phys
        else if(field_name .eq. div_filtered_magne%name) then
          grad_fil_fld%i_div_b =    i_phys
        else if(field_name .eq. div_filtered_vector_potential%name)     &
     &   then
          grad_fil_fld%i_div_a =    i_phys
!
        else if(field_name .eq. grad_filtered_temp%name) then
          grad_fil_fld%i_grad_temp =  i_phys
        else if(field_name .eq. grad_filtered_pert_temp%name) then
          grad_fil_fld%i_grad_per_t = i_phys
!
        else if(field_name .eq. grad_filtered_comp%name) then
          grad_fil_fld%i_grad_composit = i_phys
        else if (field_name .eq. grad_filtered_pert_comp%name) then
          grad_fil_fld%i_grad_per_c =    i_phys
!
        else if (field_name .eq. grad_filtered_density%name) then
          grad_fil_fld%i_grad_density =     i_phys
        else if (field_name .eq. grad_filtered_pert_density%name) then
          grad_fil_fld%i_grad_per_density = i_phys
!
        else if (field_name .eq. grad_filtered_entropy%name) then
          grad_fil_fld%i_grad_entropy =     i_phys
        else if (field_name .eq. grad_filtered_pert_entropy%name) then
          grad_fil_fld%i_grad_per_entropy = i_phys
        end if
      end if
!
      end subroutine set_grad_filter_field_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_wide_fil_grad_addresses                            &
     &         (i_phys, field_name, wide_filter_grad, flag)
!
      use m_wide_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: wide_filter_grad
      logical, intent(inout) :: flag
!
!
      flag = check_wide_filter_grad(field_name)
      if(flag) then
        if(field_name .eq. wide_filter_grad_temp%name) then
          wide_filter_grad%i_grad_temp =      i_phys
        else if(field_name .eq. wide_filter_grad_composition%name) then
          wide_filter_grad%i_grad_composit =  i_phys
        end if
      end if
!
      end subroutine set_wide_fil_grad_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_dble_fil_grad_addresses                            &
     &         (i_phys, field_name, dbl_filter_grad, flag)
!
      use m_dble_filter_field_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(gradient_field_address), intent(inout) :: dbl_filter_grad
      logical, intent(inout) :: flag
!
!
      flag = check_double_filter_grad(field_name)
      if(flag) then
        if (field_name .eq. double_filter_grad_temp%name) then
          dbl_filter_grad%i_grad_temp =      i_phys
        else if (field_name .eq. double_filter_grad_comp%name) then
          dbl_filter_grad%i_grad_composit =  i_phys
        end if
      end if
!
      end subroutine set_dble_fil_grad_addresses
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_filter_vect_addresses                         &
     &         (i_phys, field_name, diff_fil_vect, flag)
!
      use m_diff_filter_vect_labels
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(diff_vector_address), intent(inout) :: diff_fil_vect
      logical, intent(inout) :: flag
!
!
      flag = check_diff_filter_vectors(field_name)
      if(flag) then
        if     (field_name .eq. grad_filtered_v_1%name) then
          diff_fil_vect%i_grad_vx =   i_phys
        else if(field_name .eq. grad_filtered_v_2%name) then
          diff_fil_vect%i_grad_vy =   i_phys
        else if(field_name .eq. grad_filtered_v_3%name) then
          diff_fil_vect%i_grad_vz =   i_phys
!
        else if(field_name .eq. grad_filtered_w_1%name) then
          diff_fil_vect%i_grad_wx =   i_phys
        else if(field_name .eq. grad_filtered_w_2%name) then
          diff_fil_vect%i_grad_wy =   i_phys
        else if(field_name .eq. grad_filtered_w_3%name) then
          diff_fil_vect%i_grad_wz =   i_phys
!
        else if(field_name .eq. grad_filtered_b_1%name) then
          diff_fil_vect%i_grad_bx =   i_phys
        else if(field_name .eq. grad_filtered_b_2%name) then
          diff_fil_vect%i_grad_by =   i_phys
        else if(field_name .eq. grad_filtered_b_3%name) then
          diff_fil_vect%i_grad_bz =   i_phys
!
        else if (field_name .eq. grad_filtered_a_1%name) then
          diff_fil_vect%i_grad_ax =   i_phys
        else if (field_name .eq. grad_filtered_a_2%name) then
          diff_fil_vect%i_grad_ay =   i_phys
        else if (field_name .eq. grad_filtered_a_3%name) then
          diff_fil_vect%i_grad_az =   i_phys
!
        else if (field_name .eq. grad_filtered_j_1%name) then
          diff_fil_vect%i_grad_jx =   i_phys
        else if (field_name .eq. grad_filtered_j_2%name) then
          diff_fil_vect%i_grad_jy =   i_phys
        else if (field_name .eq. grad_filtered_j_3%name) then
          diff_fil_vect%i_grad_jz =   i_phys
        end if
      end if
!
      end subroutine set_diff_filter_vect_addresses
!
! ----------------------------------------------------------------------
!
      end module set_grad_filter_fld_labels
