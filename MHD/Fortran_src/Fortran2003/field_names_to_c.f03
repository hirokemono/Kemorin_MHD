!
      module field_names_to_c
!
      use m_precision
      use iso_c_binding
      use t_base_field_labels
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      integer(c_int) function lengthchara_f()  &
     &       bind(c, name='lengthchara_f')
!
      lengthchara_f = kchara
      return
      end function lengthchara_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_base_fields_f()  &
     &       bind(c, name='num_base_fields_f')
!
      num_base_fields_f = num_base_fields()
      return
      end function num_base_fields_f
!
! ----------------------------------------------------------------------
!
      subroutine set_base_field_names_f(field_name_c) &
     &          bind(c, name='set_base_field_names_f')
!
      use t_base_field_labels
!
      character(C_CHAR), intent(inout) :: field_name_c(*)
!
      call set_base_field_names(field_name_c(1))
!
      end subroutine set_base_field_names_f
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(c_int) function num_diff_filtered_forces_f()              &
     &              bind(c, name='num_diff_filtered_forces_f')
!
      use m_diff_filtered_force_labels
!
      num_diff_filtered_forces_f = num_diff_filtered_forces()
      return
      end function num_diff_filtered_forces_f
!
! ----------------------------------------------------------------------
!
      subroutine diff_filtered_force_labels_f                           &
     &         (n_comps_c, field_name_c, field_math_c)                  &
     &           bind(c, name='diff_filtered_force_labels_f')
!
      use m_diff_filtered_force_labels
!
      integer(c_int), intent(inout) :: n_comps_c(*)
      character(C_CHAR), intent(inout) :: field_name_c(*)
      character(C_CHAR), intent(inout) :: field_math_c(*)
!
      call set_diff_filtered_force_labels                               &
     &   (n_comps_c(1), field_name_c(1), field_math_c(1))
!
      end subroutine diff_filtered_force_labels_f
!
! ----------------------------------------------------------------------
!
      end module field_names_to_c

