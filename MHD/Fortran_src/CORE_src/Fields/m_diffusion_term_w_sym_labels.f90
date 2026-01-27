!>@file   m_diffusion_term_w_sym_labels.f90
!!        module m_diffusion_term_w_sym_labels
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for diffusion terms and diffusivities
!!
!!@verbatim
!!      logical function check_vector_diffusion_w_symmetry(field_name)
!!      logical function check_scalar_diffusion_w_symmetry(field_name)
!!
!!      subroutine set_base_diffusion_w_symmetry_names(array_c2i)
!!        type(ctl_array_c2i), intent(inout) :: array_c2i
!!
!! !!!!!  SGS model coefficients names  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!! field names 
!!
!!    sym_viscous_diffusion,           asym_viscous_diffusion             viscous_diffusion
!!    sym_vorticity_diffusion,         asym_vorticity_diffusion           vorticity_diffusion
!!    sym_vector_potential_diffusion,  asym_vector_potential_diffusion    vector_potential_diffusion
!!    sym_magnetic_diffusion,          asym_magnetic_diffusion            magnetic_diffusion
!!    sym_thermal_diffusion,           asym_thermal_diffusion             thermal_diffusion
!!    sym_composition_diffusion,       asym_composition_diffusion         composition_diffusion
!!
!!    sym_div_viscousity,              asym_div_viscousity                div_viscousity
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!!
      module m_diffusion_term_w_sym_labels
!
      use m_precision
      use m_phys_constants
      use t_field_labels
!
      implicit  none
!
!>        Field label for symmetric viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{symi} @f$
      type(field_def), parameter :: sym_viscous_diffusion                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sym_viscous_diffusion',                       &
     &                math = '$ \nu \partial_{j}\partial_{j} u_{symi} $')
!>        Field label for anti-symmetric viscous diffusion
!!         @f$ \nu \partial_{j}\partial_{j} u_{asymi} @f$
      type(field_def), parameter :: asym_viscous_diffusion                   &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'asym_viscous_diffusion',                       &
     &                math = '$ \nu \partial_{j}\partial_{j} u_{asymi} $')
!
!>        Field label for symmetric diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{symi} @f$
      type(field_def), parameter :: sym_vorticity_diffusion                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sym_vorticity_diffusion',                     &
     &                math = '$ \nu \partial_{j}\partial_{j}'           &
     &                    // ' \omega_{symi} $')
!>        Field label for anti-symmetric diffusion of vorticity
!!         @f$ \nu \partial_{j}\partial_{j} \omega_{asymi} @f$
      type(field_def), parameter :: asym_vorticity_diffusion                 &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'asym_vorticity_diffusion',                     &
     &                math = '$ \nu \partial_{j}\partial_{j}'           &
     &                    // ' \omega_{asymi} $')
!
!>        Field label for symmetric diffusion of vetor potential
!!         @f$ -J_{symi} = \eta \partial_{j}\partial_{j} A_{symi} @f$
      type(field_def), parameter :: sym_vector_potential_diffusion          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sym_vector_potential_diffusion',              &
     &                math = '$ -J_{symi}'                                 &
     &                   // ' = \eta \partial_{j}\partial_{j} A_{symi} $')
!>        Field label for anti-symmetric diffusion of vetor potential
!!         @f$ -J_{symi} = \eta \partial_{j}\partial_{j} A_{asymi} @f$
      type(field_def), parameter :: asym_vector_potential_diffusion          &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'asym_vector_potential_diffusion',              &
     &                math = '$ -J_{asymi}'                                 &
     &                   // ' = \eta \partial_{j}\partial_{j} A_{asymi} $')
!
!>        Field label for symmetric magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{symi} @f$
      type(field_def), parameter :: sym_magnetic_diffusion                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'sym_magnetic_diffusion',                      &
     &                math = '$ \eta \partial_{j}\partial_{j} B_{symi} $')
!>        Field label for anti-symmetric magnetic diffusion
!!         @f$ \nu \partial_{j}\partial_{j} B_{asymi} @f$
      type(field_def), parameter :: asym_magnetic_diffusion                  &
     &    = field_def(n_comp = n_vector,                                &
     &                name = 'asym_magnetic_diffusion',                      &
     &                math = '$ \eta \partial_{j}\partial_{j} B_{asymi} $')
!
!>        Field label for symmetric thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T_{sym} @f$
      type(field_def), parameter :: sym_thermal_diffusion                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'sym_thermal_diffusion',                       &
     &              math = '$ \kappa_{T} \partial_{j}\partial_{j} T_{sym} $')
!>        Field label for anti-symmetric thermal diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} T_{asym} @f$
      type(field_def), parameter :: asym_thermal_diffusion                   &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'asym_thermal_diffusion',                       &
     &              math = '$ \kappa_{T} \partial_{j}\partial_{j} T_{asym} $')
!
!>        Field label for symmetric compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C_{sym} @f$
      type(field_def), parameter :: sym_composition_diffusion               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'sym_composition_diffusion',                   &
     &              math = '$ \kappa_{C} \partial_{j}\partial_{j} C_{sym} $')
!>        Field label for anti-symmetric compositional diffusion
!!         @f$ \kappa \partial_{i}\partial_{i} C_{asym} @f$
      type(field_def), parameter :: asym_composition_diffusion               &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'asym_composition_diffusion',                   &
     &              math = '$ \kappa_{C} \partial_{j}\partial_{j} C_{asym} $')
!
!>        Field label for symmetric divergence of viscousity
!!          @f$ \nu \partial_{i} \partial_{j}\partial_{j} u_{symi} @f$
      type(field_def), parameter :: sym_div_viscousity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'sym_div_viscousity',                          &
     &                math = '$ \nu \partial_{i}'                       &
     &                     // ' \partial_{j}\partial_{j} u_{symi} $')
!>        Field label for anti-symmetric divergence of viscousity
!!          @f$ \nu \partial_{i} \partial_{j}\partial_{j} u_{asymi} @f$
      type(field_def), parameter :: asym_div_viscousity                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'asym_div_viscousity',                          &
     &                math = '$ \nu \partial_{i}'                       &
     &                     // ' \partial_{j}\partial_{j} u_{asymi} $')
!
!   --------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      logical function check_vector_diffusion_w_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_vector_diffusion_w_symmetry                                     &
     &   =    (field_name .eq. sym_viscous_diffusion%name)                  &
     &   .or. (field_name .eq. asym_viscous_diffusion%name)                 &
     &   .or. (field_name .eq. sym_vorticity_diffusion%name)                &
     &   .or. (field_name .eq. asym_vorticity_diffusion%name)               &
     &   .or. (field_name .eq. sym_vector_potential_diffusion%name)         &
     &   .or. (field_name .eq. asym_vector_potential_diffusion%name)        &
     &   .or. (field_name .eq. sym_magnetic_diffusion%name)                 &
     &   .or. (field_name .eq. asym_magnetic_diffusion%name)
!
      end function check_vector_diffusion_w_symmetry
!
! ----------------------------------------------------------------------
!
      logical function check_scalar_diffusion_w_symmetry(field_name)
!
      character(len = kchara), intent(in) :: field_name
!
!
      check_scalar_diffusion_w_symmetry                                            &
     &   =    (field_name .eq. sym_thermal_diffusion%name)                  &
     &   .or. (field_name .eq. asym_thermal_diffusion%name)              &
     &   .or. (field_name .eq. sym_composition_diffusion%name)              &
     &   .or. (field_name .eq. asym_composition_diffusion%name)              &
     &   .or. (field_name .eq. sym_div_viscousity%name)              &
     &   .or. (field_name .eq. asym_div_viscousity%name)
!
      end function check_scalar_diffusion_w_symmetry
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_diffusion_w_symmetry_names(array_c2i)
      use t_control_array_chara2int
      type(ctl_array_c2i), intent(inout) :: array_c2i
!
      array_c2i%array_name = '  '
      array_c2i%num =         0
      call alloc_control_array_c2_i(array_c2i)
!
      call set_field_label_to_ctl(sym_viscous_diffusion,   array_c2i)
      call set_field_label_to_ctl(asym_viscous_diffusion,   array_c2i)
      call set_field_label_to_ctl(sym_vorticity_diffusion, array_c2i)
      call set_field_label_to_ctl(asym_vorticity_diffusion, array_c2i)
      call set_field_label_to_ctl(sym_vector_potential_diffusion,           &
     &                            array_c2i)
      call set_field_label_to_ctl(asym_vector_potential_diffusion,           &
     &                            array_c2i)
      call set_field_label_to_ctl(sym_magnetic_diffusion,    array_c2i)
      call set_field_label_to_ctl(asym_magnetic_diffusion,    array_c2i)
      call set_field_label_to_ctl(sym_thermal_diffusion,     array_c2i)
      call set_field_label_to_ctl(asym_thermal_diffusion,     array_c2i)
      call set_field_label_to_ctl(sym_composition_diffusion, array_c2i)
      call set_field_label_to_ctl(asym_composition_diffusion, array_c2i)
      call set_field_label_to_ctl(sym_div_viscousity,        array_c2i)
      call set_field_label_to_ctl(asym_div_viscousity,        array_c2i)
!
      end subroutine set_diffusion_w_symmetry_names
!
! ----------------------------------------------------------------------
!
      end module m_diffusion_term_w_sym_labels
