!>@file   read_ctl_data_sph_MHD.f90
!!@brief  module read_ctl_data_sph_MHD
!!
!!@author H. Matsui
!>@brief   Control read routine
!!@date   programmed by H.Matsui and H.Okuda
!!@n                                    on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!!@n        Modified by H. Matsui on Oct., 2007
!!@n        Modified by H. Matsui on Oct., 2012
!!
!!@verbatim
!!      subroutine read_sph_mhd_model
!!      subroutine read_sph_mhd_control
!!
!!      subroutine bcast_sph_mhd_model
!!      subroutine bcast_sph_mhd_control
!!@endverbatim
!
      module read_ctl_data_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
      use t_ctl_data_mhd_normalize
      use t_ctl_data_mhd_forces
      use t_ctl_data_temp_model
      use t_ctl_data_SGS_model
!
      use skip_comment_f
!
      implicit none
!
!>      Structure for field information control
      type(field_control), save :: fld_ctl1
!
!>        Structure for evolution fields control
      type(mhd_evolution_control), save :: evo_ctl1
!>        Structure for domain area controls
      type(mhd_evo_area_control), save :: earea_ctl1
!
!>        Structure for nodal boundary conditions
      type(node_bc_control), save :: nbc_ctl1
!>        Structure for surface boundary conditions
      type(surf_bc_control), save :: sbc_ctl1
!
!>        Structure for list of dimensionless numbers
      type(dimless_control), save :: dless_ctl1
!>      Structure for coefficients of governing equations
      type(equations_control) :: eqs_ctl1
!
!>      Structure for force list
      type(forces_control), save :: frc_ctl1
!>      Structure for gravity definistion
      type(gravity_control), save :: g_ctl1
!>      Structure for Coriolis force
      type(coriolis_control), save :: cor_ctl1
!>      Structure for Coriolis force
      type(magneto_convection_control), save :: mcv_ctl1
!
!>      Structures for reference tempearature
      type(reference_temperature_ctl), save :: reft_ctl1
!>      Structures for reference composition
      type(reference_temperature_ctl), save :: refc_ctl1
!>      Structures for SGS controls
      type(SGS_model_control), save :: sgs_ctl1
!
!   2nd level for MHD
!
      character(len=kchara), parameter :: hd_model =   'model'
      character(len=kchara), parameter :: hd_control = 'control'
!
      integer (kind=kint) :: i_model =        0
      integer (kind=kint) :: i_control =      0
!
!    label for entry of group
!
!
!>      label for block
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
!>      Number of field
      integer (kind=kint) :: i_phys_values =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_evo =     'time_evolution_ctl'
      character(len=kchara), parameter :: hd_layers_ctl = 'layers_ctl'
      integer (kind=kint) :: i_time_evo =      0
      integer (kind=kint) :: i_layers_ctl =    0
!
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter                                  &
     &      :: hd_boundary_condition = 'boundary_condition'
      integer (kind=kint) :: i_bc_4_node =     0
!
      character(len=kchara), parameter                                  &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
      integer (kind=kint) :: i_dimless_ctl =   0
      integer (kind=kint) :: i_coef_term_ctl = 0
!
      character(len=kchara), parameter                                  &
     &      :: hd_forces_ctl =   'forces_define'
      character(len=kchara), parameter                                  &
     &      :: hd_gravity_ctl =  'gravity_define'
      character(len=kchara), parameter                                  &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
      character(len=kchara), parameter                                  &
     &      :: hd_magneto_ctl =  'Magneto_convection_def'
      integer (kind=kint) :: i_forces_ctl =    0
      integer (kind=kint) :: i_gravity_ctl =   0
      integer (kind=kint) :: i_coriolis_ctl =  0
      integer (kind=kint) :: i_magneto_ctl =   0
!
      character(len=kchara), parameter                                  &
     &      :: hd_temp_def =     'temperature_define'
      character(len=kchara), parameter                                  &
     &      :: hd_comp_def =     'composition_define'
      character(len=kchara), parameter                                  &
     &      :: hd_bc_4_surf =    'bc_4_surface'
      integer (kind=kint) :: i_temp_def =      0
      integer (kind=kint) :: i_comp_def =      0
      integer (kind=kint) :: i_bc_4_surf =     0
!
      character(len=kchara), parameter :: hd_sgs_ctl = 'SGS_control'
      integer (kind=kint) :: i_sgs_ctl =       0
!
      private :: hd_model, hd_control, i_model, i_control
!
      private :: hd_phys_values, i_phys_values
!
      private :: hd_time_evo, hd_layers_ctl
      private :: i_time_evo,  i_layers_ctl
!
      private :: hd_bc_4_node, hd_boundary_condition, i_bc_4_node
      private :: hd_bc_4_surf, i_bc_4_surf
!
      private :: hd_dimless_ctl, hd_coef_term_ctl
      private :: i_dimless_ctl,  i_coef_term_ctl
!
      private :: hd_forces_ctl, i_forces_ctl
      private :: hd_gravity_ctl, hd_coriolis_ctl, hd_magneto_ctl
      private :: i_gravity_ctl,  i_coriolis_ctl,  i_magneto_ctl
!
      private :: hd_temp_def, i_temp_def
      private :: hd_comp_def, i_comp_def
      private :: hd_sgs_ctl, i_sgs_ctl
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_mhd_model
!
!
      if(right_begin_flag(hd_model) .eq. 0) return
      if (i_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_model, i_model)
        if(i_model .gt. 0) exit
!
        call read_phys_data_control                                     &
     &     (hd_phys_values, i_phys_values, fld_ctl1)
!
        call read_mhd_time_evo_ctl(hd_time_evo, i_time_evo, evo_ctl1)
        call read_mhd_layer_ctl                                         &
     &     (hd_layers_ctl, i_layers_ctl, earea_ctl1)
!
        call read_bc_4_node_ctl                                         &
     &   (hd_boundary_condition, i_bc_4_node, nbc_ctl1)
        call read_bc_4_node_ctl(hd_bc_4_node, i_bc_4_node, nbc_ctl1)
        call read_bc_4_surf_ctl(hd_bc_4_surf, i_bc_4_surf, sbc_ctl1)
!
        call read_forces_ctl(hd_forces_ctl, i_forces_ctl, frc_ctl1)
        call read_dimless_ctl(hd_dimless_ctl, i_dimless_ctl, dless_ctl1)
        call read_coef_term_ctl                                         &
     &     (hd_coef_term_ctl, i_coef_term_ctl, eqs_ctl1)
!
        call read_gravity_ctl(hd_gravity_ctl, i_gravity_ctl, g_ctl1)
        call read_coriolis_ctl                                          &
     &     (hd_coriolis_ctl, i_coriolis_ctl, cor_ctl1)
        call read_magneto_ctl(hd_magneto_ctl, i_magneto_ctl, mcv_ctl1)
        call read_reftemp_ctl(hd_temp_def, i_temp_def, reft_ctl1)
        call read_reftemp_ctl(hd_comp_def, i_comp_def, refc_ctl1)
!
        call read_sgs_ctl(hd_sgs_ctl, i_sgs_ctl, sgs_ctl1)
      end do
!
      end subroutine read_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine read_sph_mhd_control
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_mhd_evo_scheme
!
!
      if(right_begin_flag(hd_control) .eq. 0) return
      if (i_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_control, i_control)
        if(i_control .gt. 0) exit
!
!
        call read_time_step_ctl
        call read_restart_control
!
        call read_time_loop_control
      end do
!
      end subroutine read_sph_mhd_control
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_model
!
      use bcast_4_field_ctl
!
!
      call bcast_phys_data_ctl(fld_ctl1)
      call bcast_mhd_time_evo_ctl(evo_ctl1)
      call bcast_mhd_layer_ctl(earea_ctl1)
!
      call bcast_bc_4_node_ctl(nbc_ctl1)
      call bcast_bc_4_surf_ctl(sbc_ctl1)
!
      call bcast_dimless_ctl(dless_ctl1)
      call bcast_coef_term_ctl(eqs_ctl1)
      call bcast_forces_ctl(frc_ctl1)
      call bcast_gravity_ctl(g_ctl1)
      call bcast_coriolis_ctl(cor_ctl1)
      call bcast_magneto_ctl(mcv_ctl1)
      call bcast_ref_scalar_ctl(reft_ctl1)
      call bcast_ref_scalar_ctl(refc_ctl1)
      call bcast_sgs_ctl(sgs_ctl1)
!
      end subroutine bcast_sph_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_mhd_control
!
      use m_ctl_data_4_time_steps
      use m_ctl_data_mhd_evo_scheme
      use bcast_4_time_step_ctl
!
!
      call bcast_restart_ctl(mr_ctl1)
      call bcast_time_loop_ctl(mevo_ctl1)
      call bcast_ctl_data_4_time_step(tctl1)
!
      end subroutine bcast_sph_mhd_control
!
!   --------------------------------------------------------------------
!
      end module read_ctl_data_sph_MHD
