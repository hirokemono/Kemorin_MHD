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
!!      integer(kind = kint) function num_div_filtered_forces()
!!        subroutine set_div_filtered_force_labels(n_comps, names, maths)
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
      use t_base_force_labels
!
!>      Number of field labels
      integer(kind = kint), parameter, private                          &
     &                     :: ndiv_filter_force = 12
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
      integer(kind = kint) function num_div_filtered_forces()
      num_div_filtered_forces = ndiv_filter_force
      return
      end function num_div_filtered_forces
!
! ----------------------------------------------------------------------
!
      subroutine set_div_filtered_force_labels(n_comps, names, maths)
!
      integer(kind = kint_4b), intent(inout)                            &
     &                        :: n_comps(ndiv_filter_force)
      character(len = kchara), intent(inout)                            &
     &                        :: names(ndiv_filter_force)
      character(len = kchara), intent(inout)                            &
     &                        :: maths(ndiv_filter_force)
!
!
      call set_field_labels(div_inertia_by_filtered,                    &
     &    n_comps( 1), names( 1), maths( 1))
      call set_field_labels(div_Lorentz_force_by_filtered,              &
     &    n_comps( 2), names( 2), maths( 2))
      call set_field_labels(div_filtered_buoyancy,                      &
     &    n_comps( 3), names( 3), maths( 3))
      call set_field_labels(div_filtered_comp_buoyancy,                 &
     &    n_comps( 4), names( 4), maths( 4))
!
      call set_field_labels(div_vecp_induction_by_filtered,             &
     &    n_comps( 5), names( 5), maths( 5))
!
      call set_field_labels(div_m_flux_by_filtered,                     &
     &    n_comps( 6), names( 6), maths( 6))
      call set_field_labels(div_maxwell_t_by_filtered,                  &
     &    n_comps( 7), names( 7), maths( 7))
      call set_field_labels(div_induct_t_by_filtered,                   &
     &    n_comps( 8), names( 8), maths( 8))
!
      call set_field_labels(div_h_flux_by_filtered,                     &
     &    n_comps( 9), names( 9), maths( 9))
      call set_field_labels(div_pert_h_flux_by_filtered,                &
     &    n_comps(10), names(10), maths(10))
      call set_field_labels(div_c_flux_by_filtered,                     &
     &    n_comps(11), names(11), maths(11))
      call set_field_labels(div_pert_c_flux_by_filtered,                &
     &    n_comps(12), names(12), maths(12))
!
      end subroutine set_div_filtered_force_labels
!
! ----------------------------------------------------------------------
!
      end module m_div_filtered_force_labels
