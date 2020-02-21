!>@file  m_diff_filtered_force_labels.f90
!!       module m_diff_filtered_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!>@brief Labels of fields
!!
!!@verbatim
!!      logical function check_rot_fil_force(field_name)
!!      logical function check_div_fil_force(field_name)
!!      logical function check_div_fil_flux_t(field_name)
!!      logical function check_div_fil_scl_flux(field_name)
!!
!!      subroutine set_rot_fil_force_addresses                          &
!!     &         (i_phys, field_name, rot_frc_by_filter, flag)
!!        type(base_force_address), intent(inout) :: rot_frc_by_filter
!!      subroutine set_div_fil_force_addresses                          &
!!     &         (i_phys, field_name, div_frc_by_filter, flag)
!!        type(base_force_address), intent(inout) :: div_frc_by_filter
!!
!!      integer(kind = kint) function num_diff_filtered_forces()
!!      subroutine set_diff_filtered_force_labels                       &
!!     &         (n_comps, field_names, maths)
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
!!   div_inertia_by_filtered           [div_frc_by_filter%i_m_advect]
!!   div_Lorentz_force_by_filtered     [div_frc_by_filter%i_lorentz]
!!   div_filtered_buoyancy             [div_frc_by_filter%i_buoyancy]
!!   div_filtered_comp_buoyancy        [div_frc_by_filter%i_comp_buo]
!!   div_vecp_induction_by_filtered    [div_frc_by_filter%i_induction]
!!
!!   div_m_flux_by_filtered            [div_frc_by_filter%i_m_flux]
!!   div_maxwell_t_by_filtered         [div_frc_by_filter%i_maxwell]
!!   div_induct_t_by_filtered          [div_frc_by_filter%i_induct_t]
!!
!!   div_h_flux_by_filtered            [div_frc_by_filter%i_h_flux]
!!   div_part_h_flux_by_filtered       [div_frc_by_filter%i_ph_flux]
!!   div_c_flux_by_filtered            [div_frc_by_filter%i_c_flux]
!!   div_part_c_flux_by_filtered       [div_frc_by_filter%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_diff_filtered_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
      use t_base_force_labels
!
!>      Number of field labels
      integer(kind = kint), parameter, private                          &
     &                     :: ndiff_force_filter = 17
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
      character(len=kchara), parameter :: fhd_rot_filter_buo            &
     &                                = 'rot_filtered_buoyancy'
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
!
!  divergence of momentum equations
!>        Field label for divergence of advection
      type(field_def), parameter :: div_inertia_by_filtered             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_inertia_by_filtered',                 &
     &                math = '$ \partial_{i}'                           &
     &                // '(e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}$')
!>        Field label for divergence of Lorentz force
      type(field_def), parameter :: div_Lorentz_force_by_filtered       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Lorentz_force_by_filtered',           &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} \tilde{J}_{j} \tilde{B}_{k}$')
!
!>        Field label for divergence of filtered buoyancy
      character(len=kchara), parameter :: fhd_div_filter_buo            &
     &                                = 'div_filtered_buoyancy'
      type(field_def), parameter :: div_filtered_buoyancy               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_buoyancy',                   &
     &             math = '$-\partial_{i} \alpha_{T} \tilde{T} g_{i}$')
!>        Field label for divergence of filtered compositional buoyancy
      type(field_def), parameter :: div_filtered_comp_buoyancy          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_comp_buoyancy',              &
     &             math = '$-\partial_{i} \alpha_{C} \tilde{C} g_{i}$')
!
!>        Field label for divergence of induction
      type(field_def), parameter :: div_vecp_induction_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_vecp_induction_by_filtered',          &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} \tilde{u}_{j} \tilde{B}_{k}$')
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} \left( u_{i} u_{j} \right) @f$
      type(field_def), parameter :: div_m_flux_by_filtered              &
     &    = field_def(n_comp = n_vector,                                &
     &             name = 'div_m_flux_by_filtered',                     &
     &          math = '$-\partial_{j} (\tilde{B}_{i} \tilde{B}_{j})$')
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} \left( B_{i} B_{j} \right) @f$
      type(field_def), parameter :: div_maxwell_t_by_filtered           &
     &    = field_def(n_comp = n_vector,                                &
     &          name = 'div_maxwell_t_by_filtered',                     &
     &          math = '$ \partial_{j} (\tilde{B}_{i} \tilde{B}_{j})$')
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} \left(u_{i} B_{j} - B_{i} u_{J} \right) @f$
      type(field_def), parameter :: div_induct_t_by_filtered            &
     &    = field_def(n_comp = n_vector,                                &
     &             name = 'div_induct_t_by_filtered',                   &
     &             math = '$ \partial_{i} (\tilde{u}_{i} \tilde{B}_{j}' &
     &                  //' - \tilde{B}_{i} \tilde{u}_{J})$')
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} \left( u_{i} T \right) @f$
      type(field_def), parameter :: div_h_flux_by_filtered              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_h_flux_by_filtered',                  &
     &                math = '$-\partial_{i} \tilde{u}_{i} \tilde{T}$')
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} \left( u_{i} \Theta \right) @f$
      type(field_def), parameter :: div_part_h_flux_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_part_h_flux_by_filtered',             &
     &                math = '$-\partial_{i} \tilde{u}_{i}'             &
     &                       //' \tilde{\Therta}$')
!
!>        Field label for divergence of composition flux
!!         @f$ -\partial_{i} \tilde{u}_{i} \tilde{C} @f$
      type(field_def), parameter :: div_c_flux_by_filtered              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_c_flux_by_filtered',                  &
     &                math = '$-\partial_{i} \tilde{u}_{i} \tilde{C}$')
!>        Field label for divergence of perturbation of compopstion flux
!!         @f$ -\partial_{i} \tilde{u}_{i} (\tilde{C}-C_{0}) @f$
      type(field_def), parameter :: div_part_c_flux_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_part_c_flux_by_filtered',             &
     &                math = '$-\partial_{i} \tilde{u}_{i}'             &
     &                       //' (\tilde{C}-C_{0})$')
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
!
      logical function check_div_fil_force(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_fil_force                                               &
     &   =    (field_name .eq. div_inertia_by_filtered%name)            &
     &   .or. (field_name .eq. div_Lorentz_force_by_filtered%name)      &
     &   .or. (field_name .eq. div_filtered_buoyancy%name)              &
     &   .or. (field_name .eq. div_filtered_comp_buoyancy%name)         &
     &   .or. (field_name .eq. div_vecp_induction_by_filtered%name)
!
      end function check_div_fil_force
!
! ----------------------------------------------------------------------
!
      logical function check_div_fil_flux_t(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_fil_flux_t                                              &
     &   =    (field_name .eq. div_m_flux_by_filtered%name)             &
     &   .or. (field_name .eq. div_maxwell_t_by_filtered%name)          &
     &   .or. (field_name .eq. div_induct_t_by_filtered%name)
!
      end function check_div_fil_flux_t
!
! ----------------------------------------------------------------------
!
      logical function check_div_fil_scl_flux(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_div_fil_scl_flux                                            &
     &   =    (field_name .eq. div_h_flux_by_filtered%name)             &
     &   .or. (field_name .eq. div_part_h_flux_by_filtered%name)        &
     &   .or. (field_name .eq. div_c_flux_by_filtered%name)             &
     &   .or. (field_name .eq. div_part_c_flux_by_filtered%name)
!
      end function check_div_fil_scl_flux
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
!
      subroutine set_div_fil_force_addresses                            &
     &         (i_phys, field_name, div_frc_by_filter, flag)
!
      integer(kind = kint), intent(in) :: i_phys
      character(len = kchara), intent(in) :: field_name
!
      type(base_force_address), intent(inout) :: div_frc_by_filter
      logical, intent(inout) :: flag
!
!
      flag = check_div_fil_force(field_name)                            &
     &      .or. check_div_fil_flux_t(field_name)                       &
     &      .or. check_div_fil_scl_flux(field_name)
      if(flag) then
        if (field_name .eq. div_inertia_by_filtered%name) then
          div_frc_by_filter%i_m_advect =   i_phys
        else if (field_name .eq. div_Lorentz_force_by_filtered%name)    &
     &   then
          div_frc_by_filter%i_lorentz =    i_phys
!
        else if (field_name .eq. div_filtered_buoyancy%name) then
          div_frc_by_filter%i_buoyancy =   i_phys
        else if (field_name .eq. div_filtered_comp_buoyancy%name) then
          div_frc_by_filter%i_comp_buo =   i_phys
!
        else if (field_name .eq. div_vecp_induction_by_filtered%name)   &
     &   then
          div_frc_by_filter%i_induction =  i_phys
!
        else if (field_name .eq. div_h_flux_by_filtered%name) then
          div_frc_by_filter%i_h_flux =    i_phys
        else if (field_name .eq. div_part_h_flux_by_filtered%name) then
          div_frc_by_filter%i_ph_flux =   i_phys
!
        else if (field_name .eq. div_c_flux_by_filtered%name) then
          div_frc_by_filter%i_c_flux =    i_phys
        else if (field_name .eq. div_part_c_flux_by_filtered%name) then
          div_frc_by_filter%i_pc_flux =   i_phys
!
        else if (field_name .eq. div_m_flux_by_filtered%name) then
          div_frc_by_filter%i_m_flux =   i_phys
        else if (field_name .eq. div_maxwell_t_by_filtered%name) then
          div_frc_by_filter%i_maxwell =  i_phys
        else if (field_name .eq. div_induct_t_by_filtered%name) then
          div_frc_by_filter%i_induct_t = i_phys
        end if
      end if
!
      end subroutine set_div_fil_force_addresses
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_diff_filtered_forces()
      num_diff_filtered_forces = ndiff_force_filter
      return
      end function num_diff_filtered_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_diff_filtered_force_labels                         &
     &         (n_comps, field_names, maths)
!
      integer(kind = kint), intent(inout)                               &
     &                        :: n_comps(ndiff_force_filter)
      character(len = kchara), intent(inout)                            &
     &                        :: field_names(ndiff_force_filter)
      character(len = kchara), intent(inout)                            &
     &                        :: maths(ndiff_force_filter)
!
!
      call set_field_labels(rot_inertia_by_filtered,                    &
     &    n_comps( 1), field_names( 1), maths( 1))
      call set_field_labels(rot_Lorentz_force_by_filtered,              &
     &    n_comps( 2), field_names( 2), maths( 2))
      call set_field_labels(rot_filtered_buoyancy,                      &
     &    n_comps( 3), field_names( 3), maths( 3))
      call set_field_labels(rot_filtered_comp_buoyancy,                 &
     &    n_comps( 4), field_names( 4), maths( 4))
!
      call set_field_labels(magnetic_induction_by_filtered,             &
     &    n_comps( 5), field_names( 5), maths( 5))
!
      call set_field_labels(div_inertia_by_filtered,                    &
     &    n_comps( 6), field_names( 6), maths( 6))
      call set_field_labels(div_Lorentz_force_by_filtered,              &
     &    n_comps( 7), field_names( 7), maths( 7))
      call set_field_labels(div_filtered_buoyancy,                      &
     &    n_comps( 8), field_names( 8), maths( 8))
      call set_field_labels(div_filtered_comp_buoyancy,                 &
     &    n_comps( 9), field_names( 9), maths( 9))
!
      call set_field_labels(div_vecp_induction_by_filtered,             &
     &    n_comps(10), field_names(10), maths(10))
!
      call set_field_labels(div_m_flux_by_filtered,                     &
     &    n_comps(11), field_names(11), maths(11))
      call set_field_labels(div_maxwell_t_by_filtered,                  &
     &    n_comps(12), field_names(12), maths(12))
      call set_field_labels(div_induct_t_by_filtered,                   &
     &    n_comps(13), field_names(13), maths(13))
!
      call set_field_labels(div_h_flux_by_filtered,                     &
     &    n_comps(14), field_names(14), maths(14))
      call set_field_labels(div_part_h_flux_by_filtered,                &
     &    n_comps(15), field_names(15), maths(15))
      call set_field_labels(div_c_flux_by_filtered,                     &
     &    n_comps(16), field_names(16), maths(16))
      call set_field_labels(div_part_c_flux_by_filtered,                &
     &    n_comps(17), field_names(17), maths(17))
!
      end subroutine set_diff_filtered_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_diff_filtered_force_labels
