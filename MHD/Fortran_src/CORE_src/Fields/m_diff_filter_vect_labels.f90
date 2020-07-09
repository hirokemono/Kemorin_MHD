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
!!      subroutine set_diff_filter_vect_addresses                       &
!!     &         (i_phys, field_name, diff_fil_vect, flag)
!!        type(diff_vector_address), intent(inout) :: diff_fil_vect
!!
!!      integer(kind = kint) function num_diff_filter_vector()
!!      subroutine set_diff_filter_vect_labels(n_comps, names, maths)
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
      use t_diff_vector_labels
!
      implicit none
!
      integer(kind = kint), parameter, private :: ngrad_fil_vect = 15
!
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
!
      subroutine set_diff_filter_vect_addresses                         &
     &         (i_phys, field_name, diff_fil_vect, flag)
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
! ----------------------------------------------------------------------
! 
      integer(kind = kint) function num_diff_filter_vector()
      num_diff_filter_vector = ngrad_fil_vect
      return
      end function num_diff_filter_vector
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_filter_vect_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout) :: n_comps(ngrad_fil_vect)
      character(len = kchara), intent(inout) :: names(ngrad_fil_vect)
      character(len = kchara), intent(inout) :: maths(ngrad_fil_vect)
!
!
      call set_field_labels(grad_filtered_v_1,                          &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(grad_filtered_v_2,                          &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(grad_filtered_v_3,                          &
     &    n_comps( 3), names( 3), maths( 3))
!
      call set_field_labels(grad_filtered_w_1,                          &
     &    n_comps( 4), names( 4), maths( 4))
      call set_field_labels(grad_filtered_w_2,                          &
     &    n_comps( 5), names( 5), maths( 5))
      call set_field_labels(grad_filtered_w_3,                          &
     &    n_comps( 6), names( 6), maths( 6))
!
      call set_field_labels(grad_filtered_b_1,                          &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(grad_filtered_b_2,                          &
     &    n_comps( 8), names( 8), maths( 8))
      call set_field_labels(grad_filtered_b_3,                          &
     &    n_comps( 9), names( 9), maths( 9))
!
      call set_field_labels(grad_filtered_a_1,                          &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(grad_filtered_a_2,                          &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(grad_filtered_a_3,                          &
     &    n_comps(12), names(12), maths(12))
!
      call set_field_labels(grad_filtered_j_1,                          &
     &    n_comps(13), names(13), maths(13))
      call set_field_labels(grad_filtered_j_2,                          &
     &    n_comps(14), names(14), maths(14))
      call set_field_labels(grad_filtered_j_3,                          &
     &    n_comps(15), names(15), maths(15))
!
      end subroutine set_diff_filter_vect_labels
!
! ----------------------------------------------------------------------
!
      end module m_diff_filter_vect_labels
