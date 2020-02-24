!>@file  m_rot_filtered_force_labels.f90
!!       module m_rot_filtered_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!!      logical function check_rot_fil_force(field_name)
!!      subroutine set_rot_fil_force_addresses                          &
!!     &         (i_phys, field_name, rot_frc_by_filter, flag)
!!        type(base_force_address), intent(inout) :: rot_frc_by_filter
!!
!!      integer(kind = kint) function num_rot_filtered_forces()
!!      subroutine set_rot_filtered_force_labels(n_comps, names, maths)
!!
!! !!!!!  difference of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   rot_inertia_by_filtered           [rot_frc_by_filter%i_m_advect]
!!   rot_Lorentz_force_by_filtered     [rot_frc_by_filter%i_lorentz]
!!   rot_filtered_buoyancy             [rot_frc_by_filter%i_buoyancy]
!!   rot_filtered_comp_buoyancy        [rot_frc_by_filter%i_comp_buo]
!!   magnetic_induction_by_filtered    [rot_frc_by_filter%i_induction]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_rot_filtered_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_base_force_labels
!
!>      Number of field labels
      integer(kind = kint), parameter, private                          &
     &                     :: nrot_filter_force = 5
!
!  rotation of momentum equations
!>        Field label for curl of advection
      type(field_def), parameter :: rot_inertia_by_filtered             &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_inertia_by_filtered',                 &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &                // ' (e_{kkm} \tilde{\omega}_{l} \tilde{u}_{m}$')
!>        Field label for curl of Lorentz force
      type(field_def), parameter :: rot_Lorentz_force_by_filtered       &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'rot_Lorentz_force_by_filtered',           &
     &                math = '$ e_{ijk} \partial_{j}'                   &
     &                    // '(e_{kkm} \tilde{J}_{l} \tilde{B}_{m}$')
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
!>        Field label for divergence of induction
!!        @f$ e_{ijk} \partial_{j}
!!            (e_{klm} \tilde{u}_{l} \tilde{B}_{m}$ @f$
      type(field_def), parameter :: magnetic_induction_by_filtered      &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'magnetic_induction_by_filtered',          &
     &                math = '$ e_{ijk} \partial_{j} '                  &
     &                    // '(e_{klm} \tilde{u}_{l} \tilde{B}_{m}$')
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
     &   .or. (field_name .eq. rot_filtered_comp_buoyancy%name)         &
     &   .or. (field_name .eq. magnetic_induction_by_filtered%name)
!
      end function check_rot_fil_force
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_rot_fil_force_addresses                            &
     &         (i_phys, field_name, rot_frc_by_filter, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: rot_frc_by_filter
      logical, intent(inout) :: flag
!
!
      flag = check_rot_fil_force(field_name)
      if(flag) then
        if (field_name .eq. rot_inertia_by_filtered%name) then
          rot_frc_by_filter%i_m_advect =   i_phys
        else if (field_name .eq. rot_Lorentz_force_by_filtered%name)    &
     &   then
          rot_frc_by_filter%i_lorentz =    i_phys
!
        else if (field_name .eq. rot_filtered_buoyancy%name) then
          rot_frc_by_filter%i_buoyancy =   i_phys
        else if (field_name .eq. rot_filtered_comp_buoyancy%name) then
          rot_frc_by_filter%i_comp_buo =   i_phys
!
        else if(field_name .eq. magnetic_induction_by_filtered%name)    &
     &   then
          rot_frc_by_filter%i_induction =  i_phys
        end if
      end if
!
      end subroutine set_rot_fil_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_rot_filtered_forces()
      num_rot_filtered_forces = nrot_filter_force
      return
      end function num_rot_filtered_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_rot_filtered_force_labels(n_comps, names, maths)
!
      integer(kind = kint), intent(inout)                               &
     &                        :: n_comps(nrot_filter_force)
      character(len = kchara), intent(inout)                            &
     &                        :: names(nrot_filter_force)
      character(len = kchara), intent(inout)                            &
     &                        :: maths(nrot_filter_force)
!
!
      call set_field_labels(rot_inertia_by_filtered,                    &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(rot_Lorentz_force_by_filtered,              &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(rot_filtered_buoyancy,                      &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(rot_filtered_comp_buoyancy,                 &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(magnetic_induction_by_filtered,             &
     &    n_comps( 5), names( 5), maths( 5))
!
      end subroutine set_rot_filtered_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_rot_filtered_force_labels
