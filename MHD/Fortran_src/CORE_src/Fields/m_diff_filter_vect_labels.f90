!>@file   m_diff_filter_vect_labels.f90
!!       module m_diff_filter_vect_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of difference of vectors
!!
!!@verbatim
!!      logical function check_diff_filter_vectors(field_name)
!!
!!      subroutine set_diff_filter_vect_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
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
      module m_diff_filter_vect_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit none
!
!  difference of field
!>        Field label for gradient of filtered velocity
!!             @f$ \partial_{i} \tilde{u}_{x} @f$
      type(field_def), parameter :: grad_filtered_v_1                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_v_1',                       &
     &                math = '$ \partial_{i} \tilde{u}_{1} $')
!>        Field label for gradient of filtered velocity
!!             @f$ \partial_{i} \tilde{u}_{y} @f$
      type(field_def), parameter :: grad_filtered_v_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_v_2',                       &
     &                math = '$ \partial_{i} \tilde{u}_{2} $')
!>        Field label for gradient of filtered velocity
!!             @f$ \partial_{i} \tilde{u}_{z} @f$
      type(field_def), parameter :: grad_filtered_v_3                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_v_3',                       &
     &                math = '$ \partial_{i} \tilde{u}_{3} $')
!
!>        Field label for gradient of filtered vorticity
!!             @f$ \partial_{i} \tilde{\omega}_{x} @f$
      type(field_def), parameter :: grad_filtered_w_1                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_w_1',                       &
     &                math = '$ \partial_{i} \tilde{\omega}_{1} $')
!>        Field label for gradient of filtered vorticity
!!             @f$ \partial_{i} \tilde{\omega}_{y} @f$
      type(field_def), parameter :: grad_filtered_w_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_w_2',                       &
     &                math = '$ \partial_{i} \tilde{\omega}_{2} $')
!>        Field label for gradient of filtered vorticity
!!             @f$ \partial_{i} \tilde{\omega}_{z} @f$
      type(field_def), parameter :: grad_filtered_w_3                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_w_3',                       &
     &                math = '$ \partial_{i} \tilde{\omega}_{3} $')
!
!>        Field label for gradient of filtered vector potential
!!             @f$ \partial_{i} \tilde{A}_{x} @f$
      type(field_def), parameter :: grad_filtered_a_1                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_a_1',                       &
     &                math = '$ \partial_{i} \tilde{A}_{1} $')
!>        Field label for gradient of filtered vector potential
!!             @f$ \partial_{i} \tilde{A}_{y} @f$
      type(field_def), parameter :: grad_filtered_a_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_a_2',                       &
     &                math = '$ \partial_{i} \tilde{A}_{2} $')
!>        Field label for gradient of filtered vector potential
!!             @f$ \partial_{i} \tilde{A}_{z} @f$
      type(field_def), parameter :: grad_filtered_a_3                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_a_3',                       &
     &                math = '$ \partial_{i} \tilde{A}_{3} $')
!
!>        Field label for gradient of filtered magnetic field
!!             @f$ \partial_{i} \tilde{B}_{x} @f$
      type(field_def), parameter :: grad_filtered_b_1                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_b_1',                       &
     &                math = '$ \partial_{i} \tilde{B}_{1} $')
!>        Field label for gradient of filtered magnetic field
!!             @f$ \partial_{i} \tilde{B}_{y} @f$
      type(field_def), parameter :: grad_filtered_b_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_b_2',                       &
     &                math = '$ \partial_{i} \tilde{B}_{2} $')
!>        Field label for gradient of filtered magnetic field
!!             @f$ \partial_{i} \tilde{B}_{z} @f$
      type(field_def), parameter :: grad_filtered_b_3                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_b_3',                       &
     &                math = '$ \partial_{i} \tilde{B}_{3} $')
!
!>        Field label for gradient of filtered current density
!!             @f$ \partial_{i} \tilde{J}_{x} @f$
      type(field_def), parameter :: grad_filtered_j_1                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_j_1',                       &
     &                math = '$ \partial_{i} \tilde{J}_{1} $')
!>        Field label for gradient of filtered current density
!!             @f$ \partial_{i} \tilde{J}_{y} @f$
      type(field_def), parameter :: grad_filtered_j_2                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_j_2',                       &
     &                math = '$ \partial_{i} \tilde{J}_{2} $')
!>        Field label for gradient of filtered current density
!!             @f$ \partial_{i} \tilde{J}_{z} @f$
      type(field_def), parameter :: grad_filtered_j_3                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'grad_filtered_j_3',                       &
     &                math = '$ \partial_{i} \tilde{J}_{3} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_diff_filter_vectors(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_diff_filter_vectors                                         &
     &   =    (field_name .eq. grad_filtered_v_1%name)                  &
     &   .or. (field_name .eq. grad_filtered_v_2%name)                  &
     &   .or. (field_name .eq. grad_filtered_v_3%name)                  &
!
     &   .or. (field_name .eq. grad_filtered_w_1%name)                  &
     &   .or. (field_name .eq. grad_filtered_w_2%name)                  &
     &   .or. (field_name .eq. grad_filtered_w_3%name)                  &
!
     &   .or. (field_name .eq. grad_filtered_a_1%name)                  &
     &   .or. (field_name .eq. grad_filtered_a_2%name)                  &
     &   .or. (field_name .eq. grad_filtered_a_3%name)                  &
!
     &   .or. (field_name .eq. grad_filtered_b_1%name)                  &
     &   .or. (field_name .eq. grad_filtered_b_2%name)                  &
     &   .or. (field_name .eq. grad_filtered_b_3%name)                  &
!
     &   .or. (field_name .eq. grad_filtered_j_1%name)                  &
     &   .or. (field_name .eq. grad_filtered_j_2%name)                  &
     &   .or. (field_name .eq. grad_filtered_j_3%name)
!
      end function check_diff_filter_vectors
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diff_filter_vect_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(grad_filtered_v_1,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_v_2,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_v_3,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_w_1,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_w_2,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_w_3,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_b_1,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_b_2,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_b_3,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_a_1,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_a_2,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_a_3,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_j_1,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_j_2,  array_c2i)
      call set_field_label_to_ctl(grad_filtered_j_3,  array_c2i)
!
      end subroutine set_diff_filter_vect_names
!
! ----------------------------------------------------------------------
!
      end module m_diff_filter_vect_labels
