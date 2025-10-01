!>@file  m_div_filtered_force_labels.f90
!!       module m_div_filtered_force_labels
!!
!!@author H. Matsui
!!@date   Programmed on June, 2005
!!
!> @brief Labels and addresses for divergence of forces by filtered field
!!
!!@verbatim
!!      logical function check_div_fil_force(field_name)
!!      logical function check_div_fil_flux_t(field_name)
!!      logical function check_div_fil_scl_flux(field_name)
!!
!!      subroutine set_div_filtered_force_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!! !!!!!  divergence of forces by filtered field !!!!!!!!!!!!!!!!!!
!!
!!    Field name [Address]
!!
!!   div_inertia_by_filtered           [div_frc_by_filter%i_m_advect]
!!   div_Lorentz_force_by_filtered     [div_frc_by_filter%i_lorentz]
!!   div_filtered_buoyancy             [div_frc_by_filter%i_buoyancy]
!!   div_filtered_comp_buoyancy        [div_frc_by_filter%i_comp_buo]
!!
!!   div_vecp_induction_by_filtered    [div_frc_by_filter%i_vp_induct]
!!
!!   div_m_flux_by_filtered            [div_frc_by_filter%i_m_flux]
!!   div_maxwell_t_by_filtered         [div_frc_by_filter%i_maxwell]
!!   div_induct_t_by_filtered          [div_frc_by_filter%i_induct_t]
!!
!!   div_h_flux_by_filtered            [div_frc_by_filter%i_h_flux]
!!   div_pert_h_flux_by_filtered       [div_frc_by_filter%i_ph_flux]
!!   div_c_flux_by_filtered            [div_frc_by_filter%i_c_flux]
!!   div_pert_c_flux_by_filtered       [div_frc_by_filter%i_pc_flux]
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_div_filtered_force_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
!  divergence of momentum equations
!>        Field label for divergence of advection
!!         @f$ - \partial_{i} 
!!           \left(e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k} \right) @f$
      type(field_def), parameter :: div_inertia_by_filtered             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_inertia_by_filtered',                 &
     &                math = '$ \partial_{i}'                           &
     &                // '(e_{ijk} \tilde{\omega}_{j} \tilde{u}_{k})$')
!>        Field label for divergence of Lorentz force
!!         @f$ \partial_{i}
!!            \left(e_{ijk} \tilde{J}_{j} \tilde{B}_{k} \right) @f$
      type(field_def), parameter :: div_Lorentz_force_by_filtered       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_Lorentz_force_by_filtered',           &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} \tilde{J}_{j} \tilde{B}_{k})$')
!
!>        Field label for divergence of filtered buoyancy
!!         @f$ -\partial_{i} \alpha_{T} g_{i} \tilde{T} @f$
      type(field_def), parameter :: div_filtered_buoyancy               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_buoyancy',                   &
     &             math = '$-\partial_{i} \alpha_{T} \tilde{T} g_{i}$')
!>        Field label for divergence of filtered compositional buoyancy
!!         @f$ -\partial_{i} \alpha_{C} g_{i} \tilde{C} @f$
      type(field_def), parameter :: div_filtered_comp_buoyancy          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_filtered_comp_buoyancy',              &
     &             math = '$-\partial_{i} \alpha_{C} \tilde{C} g_{i}$')
!
!>        Field label for divergence of induction
!!         @f$ \partial_{j} (e_{ijk} \tilde{u}_{j} \tilde{B}_{k}) @f$
      type(field_def), parameter :: div_vecp_induction_by_filtered      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_vecp_induction_by_filtered',          &
     &                math = '$ \partial_{i}'                           &
     &                    // '(e_{ijk} \tilde{u}_{j} \tilde{B}_{k})$')
!
!>        Field label for divergence of momentum flux
!!         @f$ \partial_{j} (\tilde{u}_{i} \tilde{u}_{j}) @f$
      type(field_def), parameter :: div_m_flux_by_filtered              &
     &    = field_def(n_comp = n_vector,                                &
     &             name = 'div_m_flux_by_filtered',                     &
     &          math = '$-\partial_{j} (\tilde{u}_{i} \tilde{u}_{j})$')
!>        Field label for divergence of Maxwell stress
!!         @f$ \partial_{j} (\tilde{B}_{i} \tilde{B}_{j}) @f$
      type(field_def), parameter :: div_maxwell_t_by_filtered           &
     &    = field_def(n_comp = n_vector,                                &
     &          name = 'div_maxwell_t_by_filtered',                     &
     &          math = '$ \partial_{j} (\tilde{B}_{i} \tilde{B}_{j})$')
!>        Field label for divergence of magnetic induction
!!         @f$ \partial_{i} (\tilde{u}_{i} \tilde{B}_{j}
!!                         - \tilde{B}_{i} \tilde{u}_{J}) @f$
      type(field_def), parameter :: div_induct_t_by_filtered            &
     &    = field_def(n_comp = n_vector,                                &
     &             name = 'div_induct_t_by_filtered',                   &
     &             math = '$ \partial_{i} (\tilde{u}_{i} \tilde{B}_{j}' &
     &                  //' - \tilde{B}_{i} \tilde{u}_{J})$')
!
!>        Field label for divergence of heat flux
!!         @f$ \partial_{i} ( \tilde{u}_{i} \tilde{T} ) @f$
      type(field_def), parameter :: div_h_flux_by_filtered              &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_h_flux_by_filtered',                  &
     &                math = '$-\partial_{i} \tilde{u}_{i} \tilde{T}$')
!>        Field label for divergence of perturbation of heat flux
!!         @f$ \partial_{i} ( \tilde{u}_{i} \tilde{\Theta} ) @f$
      type(field_def), parameter :: div_pert_h_flux_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_h_flux_by_filtered',             &
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
!!         @f$ -\partial_{i} \tilde{u}_{i} \tilde{\Theta}_{C} @f$
      type(field_def), parameter :: div_pert_c_flux_by_filtered         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'div_pert_c_flux_by_filtered',             &
     &                math = '$-\partial_{i} \tilde{u}_{i}'             &
     &                       //' \tilde{\Theta}_{C} $')
!
! ----------------------------------------------------------------------
!
      contains
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
     &   .or. (field_name .eq. div_pert_h_flux_by_filtered%name)        &
     &   .or. (field_name .eq. div_c_flux_by_filtered%name)             &
     &   .or. (field_name .eq. div_pert_c_flux_by_filtered%name)
!
      end function check_div_fil_scl_flux
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_div_filtered_force_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(div_inertia_by_filtered, array_c2i)
      call set_field_label_to_ctl(div_Lorentz_force_by_filtered,        &
     &                            array_c2i)
      call set_field_label_to_ctl(div_filtered_buoyancy, array_c2i)
      call set_field_label_to_ctl(div_filtered_comp_buoyancy, array_c2i)
      call set_field_label_to_ctl(div_vecp_induction_by_filtered,       &
     &                            array_c2i)
      call set_field_label_to_ctl(div_m_flux_by_filtered,    array_c2i)
      call set_field_label_to_ctl(div_maxwell_t_by_filtered, array_c2i)
      call set_field_label_to_ctl(div_induct_t_by_filtered,  array_c2i)
      call set_field_label_to_ctl(div_h_flux_by_filtered,    array_c2i)
      call set_field_label_to_ctl(div_pert_h_flux_by_filtered,          &
     &                            array_c2i)
      call set_field_label_to_ctl(div_c_flux_by_filtered, array_c2i)
      call set_field_label_to_ctl(div_pert_c_flux_by_filtered,          &
     &                            array_c2i)
!
      end subroutine set_div_filtered_force_names
!
! ----------------------------------------------------------------------
!
      end module m_div_filtered_force_labels
