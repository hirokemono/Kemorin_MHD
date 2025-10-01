!>@file  m_rot_filtered_force_labels.f90
!!       module m_rot_filtered_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!> @brief Labels and addresses for rotation of forces by filtered field
!!
!!@verbatim
!!      logical function check_rot_fil_force(field_name)
!!
!!      subroutine set_rot_filtered_force_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  rotation of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   rot_inertia_by_filtered           [rot_frc_by_filter%i_m_advect]
!!   rot_Lorentz_force_by_filtered     [rot_frc_by_filter%i_lorentz]
!!   rot_filtered_buoyancy             [rot_frc_by_filter%i_buoyancy]
!!   rot_filtered_comp_buoyancy        [rot_frc_by_filter%i_comp_buo]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_rot_filtered_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
!  rotation of momentum equations
!>        Field label for curl of advection
!!         @f$-e_{ijk} \partial_{j}
!!            \left(e_{klm} \tilde{\omega}_{l} \tilde{u}_{m} \right) @f$
      type(field_def), parameter :: rot_inertia_by_filtered             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_inertia_by_filtered',                 &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &               // ' (e_{kkm} \tilde{\omega}_{l} \tilde{u}_{m})$')
!>        Field label for curl of Lorentz force
!!         @f$ e_{ijk} \partial_{j}
!!            \left(e_{klm} \tilde{J}_{l} \tilde{B}_{m} \right) @f$
      type(field_def), parameter :: rot_Lorentz_force_by_filtered       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_Lorentz_force_by_filtered',           &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &                    // '(e_{kkm} \tilde{J}_{l} \tilde{B}_{m}) $')
!
!>        Field label for curl of filtered buoyancy
!!        @f$ -e_{ijk} \partial_{j} \alpha_{T} \tilde{T} g_{k}$') @f$
      type(field_def), parameter :: rot_filtered_buoyancy               &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_filtered_buoyancy',                   &
     &                math = '$-e_{ijk} \partial_{j} \alpha_{T}'        &
     &                    // ' \tilde{T} g_{k}$')
!>        Field label for curl of compositional buoyancy
!!        @f$ -e_{ijk} \partial_{j} \alpha_{C} \tilde{C} g_{k}$') @f$
      type(field_def), parameter :: rot_filtered_comp_buoyancy          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_filtered_comp_buoyancy',              &
     &                math = '$-e_{ijk} \partial_{j} \alpha_{C}'        &
     &                    // ' \tilde{C} g_{k}$')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_rot_fil_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_rot_fil_force                                               &
     &   =    (field_name .eq. rot_inertia_by_filtered%name)            &
     &   .or. (field_name .eq. rot_Lorentz_force_by_filtered%name)      &
     &   .or. (field_name .eq. rot_filtered_buoyancy%name)              &
     &   .or. (field_name .eq. rot_filtered_comp_buoyancy%name)
!
      end function check_rot_fil_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_filtered_force_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(rot_inertia_by_filtered, array_c2i)
      call set_field_label_to_ctl(rot_Lorentz_force_by_filtered,        &
     &                            array_c2i)
      call set_field_label_to_ctl(rot_filtered_buoyancy,   array_c2i)
      call set_field_label_to_ctl(rot_filtered_comp_buoyancy,           &
     &                            array_c2i)
!
      end subroutine set_rot_filtered_force_names
!
! ----------------------------------------------------------------------
!
      end module m_rot_filtered_force_labels
