!>@file   m_filtered_ene_flux_labels.f90
!!        module m_filtered_ene_flux_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for energy fluxes by filtered field
!!
!!@verbatim
!!      logical function check_filter_enegy_fluxes(field_name)
!!
!!      subroutine set_filtered_ene_flux_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  List of energy flux by SGS terms  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   inertia_work_by_filtered          [eflux_by_filter%i_m_advect_work]
!!   wk_against_Lorentz_by_filtered    [eflux_by_filter%i_nega_ujb]
!!   Lorentz_work_by_filtered          [eflux_by_filter%i_ujb]
!!   mag_tension_work_by_filtered      [eflux_by_filter%i_m_tension_wk]
!!
!!   filtered_buoyancy_flux            [eflux_by_filter%i_buo_gen]
!!   filtered_comp_buoyancy_flux       [eflux_by_filter%i_c_buo_gen]
!!
!!   mag_ene_generation_by_filtered    [eflux_by_filter%i_me_gen]
!!   mag_stretch_flux_by_filtered
!!                              [eflux_by_filter%i_mag_stretch_flux]
!!
!!   temp_generation_by_filtered       [eflux_by_filter%i_temp_gen]
!!   part_temp_gen_by_filtered         [eflux_by_filter%i_par_t_gen]
!!   comp_generation_by_filtered       [eflux_by_filter%i_comp_gen]
!!   part_comp_gen_by_filtered         [eflux_by_filter%i_par_c_gen]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_filtered_ene_flux_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>        Field label of work of inertia
!!         @f$ u_{i} (\tilde{u}_{j} \partial_{j} \tilde{u}_{i}) @f$,
!!         @f$ u_{i} (e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k}) @f$
      type(field_def), parameter :: inertia_work_by_filtered            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'inertia_work_by_filtered',                &
     &                math = '$ u_{i} (e_{ijk}'                         &
     &                     //  ' \tilde{\omega}_{j} \tilde{u}_{k})$')
!
!>        Field label of work against Lorentz force
!!         @f$ - u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k}) @f$
      type(field_def), parameter :: wk_against_Lorentz_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'wk_against_Lorentz_by_filtered',          &
     &         math = '$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k})$')
!>        Field label of work of Lorentz force
!!         @f$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k}) @f$
      type(field_def), parameter :: Lorentz_work_by_filtered            &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lorentz_work_by_filtered',                &
     &         math = '$ u_{i} (e_{ijk} \tilde{J}_{j} \tilde{B}_{k})$')
!>        Field address of work of magnetic tension
!!         @f$ u_{i} (\tilde{B}_{j} \partial_{j}) \tilde{B}_{i} @f$
      type(field_def), parameter :: mag_tension_work_by_filtered        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_tension_work_by_filtered',            &
     &                math = '$ u_{i} (\tilde{B}_{j} \partial_{j}) '    &
     &                    // ' \tilde{B}_{i} $')
!
!>        Field label for filtered buoyancy flux
!!         @f$ -u_{i} \alpha_{T} g_{i} \tilde{T} @f$
      type(field_def), parameter :: filtered_buoyancy_flux              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filtered_buoyancy_flux',                  &
     &                math = '$ -u_{i} \alpha_{T} g_{i} \tilde{T} $')
!>        Field label of compositional buoyancy flux
!!         @f$ -u_{i} \alpha_{C} g_{i} \tilde{C} @f$
      type(field_def), parameter :: filtered_comp_buoyancy_flux         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'filtered_comp_buoyancy_flux',             &
     &                math = '$ -u_{i} \alpha_{C} g_{i} \tilde{C} $')
!
!>        Field label of magnetic energy flux
!!         @f$ B_{i} e_{ijk} \partial_{j}
!!            (e_{klm} \tilde{u}_{l} \tilde{B}_{m}) @f$
      type(field_def), parameter :: mag_ene_generation_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_ene_generation_by_filtered',          &
     &                math = '$ B_{i} e_{ijk} \partial_{j} '            &
     &                   // ' (e_{klm} \tilde{u}_{l} \tilde{B}_{m}) $')
!>        Field label of energy flux of magnetic stretch term
!!          @f$ B_{i} (\tilde{B}_{j} \partial_{j} \tilde{u}_{i}) @f$
      type(field_def), parameter :: mag_stretch_flux_by_filtered        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'mag_stretch_flux_by_filtered',            &
     &                math = '$ B_{i} '                                 &
     &              // ' (\tilde{B}_{i} \partial_{k} \tilde{u}_{k}) $')
!
!>        Field label of temperature flux
!!         @f$ T (\tilde{u}_{i} \partial_{i}) \tilde{T} @f$
      type(field_def), parameter :: temp_generation_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'temp_generation_by_filtered',             &
     &          math = '$ T (\tilde{u}_{i} \partial_{i}) \tilde{T} $')
!>        Field label of perturbation temperature flux
!!         @f$ \Theta (\tilde{u}_{i} \partial_{i}) \tilde{\Theta} @f$
      type(field_def), parameter :: part_temp_gen_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'part_temp_gen_by_filtered',               &
     &                math = '$ \Theta (\tilde{u}_{i} \partial_{i})'    &
     &                    // ' \tilde{\Theta} $')
!>        Field label of composition flux
!!         @f$ C (\tilde{u}_{i} \partial_{i}) \tilde{C} @f$
      type(field_def), parameter :: comp_generation_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'comp_generation_by_filtered',             &
     &           math = '$ C (\tilde{u}_{i} \partial_{i}) \tilde{C} $')
!>        Field label of perturbation composition flux
!!         @f$ \Theta_{C} (\tilde{u}_{i} \partial_{i})
!!            \tilde{\Theta}_{C} @f$
      type(field_def), parameter :: part_comp_gen_by_filtered           &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'part_comp_gen_by_filtered',               &
     &                math = '$ \Theta_{C}(\tilde{u}_{i} \partial_{i})' &
     &                    // ' \tilde{\Theta}_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_filter_enegy_fluxes(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_filter_enegy_fluxes                                         &
     &   =    (field_name .eq. inertia_work_by_filtered%name)           &
     &   .or. (field_name .eq. wk_against_Lorentz_by_filtered%name)     &
     &   .or. (field_name .eq. Lorentz_work_by_filtered%name)           &
     &   .or. (field_name .eq. mag_tension_work_by_filtered%name)       &
     &   .or. (field_name .eq. filtered_buoyancy_flux%name)             &
     &   .or. (field_name .eq. filtered_comp_buoyancy_flux%name)        &
     &   .or. (field_name .eq. mag_ene_generation_by_filtered%name)     &
     &   .or. (field_name .eq. mag_stretch_flux_by_filtered%name)       &
     &   .or. (field_name .eq. temp_generation_by_filtered%name)        &
     &   .or. (field_name .eq. part_temp_gen_by_filtered%name)          &
     &   .or. (field_name .eq. comp_generation_by_filtered%name)        &
     &   .or. (field_name .eq. part_comp_gen_by_filtered%name)
!
      end function check_filter_enegy_fluxes
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_filtered_ene_flux_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(inertia_work_by_filtered, array_c2i)
      call set_field_label_to_ctl(wk_against_Lorentz_by_filtered,       &
     &                            array_c2i)
      call set_field_label_to_ctl(Lorentz_work_by_filtered, array_c2i)
      call set_field_label_to_ctl(mag_tension_work_by_filtered,         &
     &                            array_c2i)
      call set_field_label_to_ctl(filtered_buoyancy_flux, array_c2i)
      call set_field_label_to_ctl(filtered_comp_buoyancy_flux,          &
     &                            array_c2i)
      call set_field_label_to_ctl(mag_ene_generation_by_filtered,       &
     &                            array_c2i)
      call set_field_label_to_ctl(mag_stretch_flux_by_filtered,         &
     &                            array_c2i)
      call set_field_label_to_ctl(temp_generation_by_filtered,          &
     &                            array_c2i)
      call set_field_label_to_ctl(part_temp_gen_by_filtered, array_c2i)
      call set_field_label_to_ctl(comp_generation_by_filtered,          &
     &                            array_c2i)
      call set_field_label_to_ctl(part_comp_gen_by_filtered,  array_c2i)
!
      end subroutine set_filtered_ene_flux_names
!
! ----------------------------------------------------------------------
!
      end module m_filtered_ene_flux_labels
