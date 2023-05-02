!>@file   t_ctl_data_SGS_MHD_model.f90
!!@brief  module t_ctl_data_SGS_MHD_model
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
!!      subroutine read_sph_sgs_mhd_model                               &
!!     &         (id_control, hd_block, model_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_sph_sgs_mhd_model                              &
!!     &         (id_control, hd_block, model_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(mhd_model_control), intent(in) :: model_ctl
!!        integer(kind = kint), intent(inout) :: level
!!      subroutine bcast_sph_sgs_mhd_model(model_ctl)
!!      subroutine dealloc_sph_sgs_mhd_model(model_ctl)
!!        type(mhd_model_control), intent(inout) :: model_ctl
!!@endverbatim
!
      module t_ctl_data_SGS_MHD_model
!
      use m_precision
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_mhd_evo_area
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
      use t_ctl_data_mhd_normalize
      use t_ctl_data_mhd_forces
      use t_ctl_data_coriolis_force
      use t_ctl_data_gravity
      use t_ctl_data_mhd_magne
      use t_ctl_data_magnetic_scale
      use t_ctl_data_temp_model
      use t_ctl_data_SGS_model
      use t_ctl_data_dimless_numbers
!
      use skip_comment_f
!
      implicit none
!
      type mhd_model_control
!>        Structure for field information control
        type(field_control) :: fld_ctl
!
!>        Structure for evolution fields control
        type(mhd_evolution_control) :: evo_ctl
!>        Structure for domain area controls
        type(mhd_evo_area_control) :: earea_ctl
!
!>        Structure for nodal boundary conditions
        type(node_bc_control) :: nbc_ctl
!>        Structure for surface boundary conditions
        type(surf_bc_control) :: sbc_ctl
!
!>        Structure for list of dimensionless numbers
        type(dimless_control) :: dless_ctl
!>        Structure for coefficients of governing equations
        type(equations_control) :: eqs_ctl
!
!>        Structure for force list
        type(forces_control) :: frc_ctl
!>        Structure for gravity definistion
        type(gravity_control) :: g_ctl
!>        Structure for Coriolis force
        type(coriolis_control) :: cor_ctl
!>        Structure for Coriolis force
        type(magneto_convection_control) :: mcv_ctl
!>        Structure for magnetic field scaling
        type(magnetic_field_scale_control) :: bscale_ctl
!
!>        Structures for reference tempearature
        type(reference_temperature_ctl) :: reft_ctl
!>        Structures for reference composition
        type(reference_temperature_ctl) :: refc_ctl
!>        Structures for SGS controls
        type(SGS_model_control) :: sgs_ctl
!
        integer (kind=kint) :: i_model = 0
      end type mhd_model_control
!
!    label for entry of group
!
!
      character(len=kchara), parameter, private                         &
     &      :: hd_phys_values =  'phys_values_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_time_evo =     'time_evolution_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_layers_ctl = 'layers_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_bc_4_node =          'bc_4_node'
      character(len=kchara), parameter, private                         &
     &      :: hd_boundary_condition = 'boundary_condition'
      character(len=kchara), parameter, private                         &
     &      :: hd_bc_4_surf =    'bc_4_surface'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_forces_ctl =   'forces_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_dimless_ctl =  'dimensionless_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_coef_term_ctl ='coefficients_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_gravity_ctl =  'gravity_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_coriolis_ctl = 'Coriolis_define'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_induction_ctl =  'magnetic_induciton_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_magneto_cv_ctl =  'Magneto_convection_def'
      character(len=kchara), parameter, private                         &
     &      :: hd_bscale_ctl =   'magnetic_field_scale_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_temp_def =     'temperature_define'
      character(len=kchara), parameter, private                         &
     &      :: hd_comp_def =     'composition_define'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_sgs_ctl = 'SGS_control'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine read_sph_sgs_mhd_model                                 &
     &         (id_control, hd_block, model_ctl, c_buf)
!
      use ctl_data_temp_model_IO
      use ctl_data_comp_model_IO
      use ctl_data_node_boundary_IO
      use ctl_data_surf_boundary_IO
      use ctl_data_SGS_model_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(mhd_model_control), intent(inout) :: model_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(model_ctl%i_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, model_ctl%fld_ctl, c_buf)
!
        call read_mhd_time_evo_ctl                                      &
     &     (id_control, hd_time_evo, model_ctl%evo_ctl, c_buf)
        call read_mhd_layer_ctl                                         &
     &     (id_control, hd_layers_ctl, model_ctl%earea_ctl, c_buf)
!
        call read_bc_4_node_ctl(id_control, hd_boundary_condition,      &
     &                          model_ctl%nbc_ctl, c_buf)
        call read_bc_4_node_ctl(id_control, hd_bc_4_node,               &
     &                          model_ctl%nbc_ctl, c_buf)
        call read_bc_4_surf_ctl(id_control, hd_bc_4_surf,               &
     &                          model_ctl%sbc_ctl, c_buf)
!
        call read_forces_ctl                                            &
     &     (id_control, hd_forces_ctl, model_ctl%frc_ctl, c_buf)
        call read_dimless_ctl                                           &
     &     (id_control, hd_dimless_ctl, model_ctl%dless_ctl, c_buf)
        call read_coef_term_ctl                                         &
     &     (id_control, hd_coef_term_ctl, model_ctl%eqs_ctl, c_buf)
!
        call read_gravity_ctl                                           &
     &     (id_control, hd_gravity_ctl, model_ctl%g_ctl, c_buf)
        call read_coriolis_ctl                                          &
     &     (id_control, hd_coriolis_ctl, model_ctl%cor_ctl, c_buf)
        call read_magneto_cv_ctl                                        &
     &     (id_control, hd_magneto_cv_ctl, model_ctl%mcv_ctl, c_buf)
        call read_magnetic_scale_ctl                                    &
     &     (id_control, hd_bscale_ctl, model_ctl%bscale_ctl, c_buf)
        call read_reftemp_ctl                                           &
     &     (id_control, hd_temp_def, model_ctl%reft_ctl, c_buf)
        call read_refcomp_ctl                                           &
     &     (id_control, hd_comp_def, model_ctl%refc_ctl, c_buf)
!
        call read_sgs_ctl                                               &
     &     (id_control, hd_sgs_ctl, model_ctl%sgs_ctl, c_buf)
!
        call read_magneto_cv_ctl                                        &
     &     (id_control, hd_induction_ctl, model_ctl%mcv_ctl, c_buf)
      end do
      model_ctl%i_model = 1
!
      end subroutine read_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine write_sph_sgs_mhd_model                                &
     &         (id_control, hd_block, model_ctl, level)
!
      use ctl_data_temp_model_IO
      use ctl_data_comp_model_IO
      use ctl_data_node_boundary_IO
      use ctl_data_surf_boundary_IO
      use ctl_data_SGS_model_IO
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(mhd_model_control), intent(in) :: model_ctl
!
      integer(kind = kint), intent(inout) :: level
!
!
      if(model_ctl%i_model .le. 0) return
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_phys_data_control                                    &
     &   (id_control, hd_phys_values, model_ctl%fld_ctl, level)
!
      call write_mhd_time_evo_ctl                                     &
     &   (id_control, hd_time_evo, model_ctl%evo_ctl, level)
      call write_mhd_layer_ctl                                        &
     &   (id_control, hd_layers_ctl, model_ctl%earea_ctl, level)
!
      call write_bc_4_node_ctl(id_control, hd_boundary_condition,     &
     &                         model_ctl%nbc_ctl, level)
      call write_bc_4_surf_ctl(id_control, hd_bc_4_surf,              &
     &                         model_ctl%sbc_ctl, level)
!
      call write_forces_ctl                                           &
     &   (id_control, hd_forces_ctl, model_ctl%frc_ctl, level)
      call write_dimless_ctl                                          &
     &   (id_control, hd_dimless_ctl, model_ctl%dless_ctl, level)
      call write_coef_term_ctl                                        &
     &   (id_control, hd_coef_term_ctl, model_ctl%eqs_ctl, level)
!
      call write_gravity_ctl                                          &
     &   (id_control, hd_gravity_ctl, model_ctl%g_ctl, level)
      call write_coriolis_ctl                                         &
     &   (id_control, hd_coriolis_ctl, model_ctl%cor_ctl, level)
      call write_magneto_cv_ctl                                       &
     &   (id_control, hd_magneto_cv_ctl, model_ctl%mcv_ctl, level)
      call write_magnetic_scale_ctl                                   &
     &   (id_control, hd_bscale_ctl, model_ctl%bscale_ctl, level)
      call write_reftemp_ctl                                          &
     &   (id_control, hd_temp_def, model_ctl%reft_ctl, level)
      call write_refcomp_ctl                                          &
     &   (id_control, hd_comp_def, model_ctl%refc_ctl, level)
!
      call write_sgs_ctl                                              &
     &   (id_control, hd_sgs_ctl, model_ctl%sgs_ctl, level)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine bcast_sph_sgs_mhd_model(model_ctl)
!
      use calypso_mpi_int
      use bcast_4_field_ctl
      use bcast_ctl_data_mhd_forces
      use bcast_ctl_data_SGS_model
!
      type(mhd_model_control), intent(inout) :: model_ctl
!
!
      call bcast_phys_data_ctl(model_ctl%fld_ctl)
      call bcast_mhd_time_evo_ctl(model_ctl%evo_ctl)
      call bcast_mhd_layer_ctl(model_ctl%earea_ctl)
!
      call bcast_bc_4_node_ctl(model_ctl%nbc_ctl)
      call bcast_bc_4_surf_ctl(model_ctl%sbc_ctl)
!
      call bcast_dimless_ctl(model_ctl%dless_ctl)
      call bcast_coef_term_ctl(model_ctl%eqs_ctl)
      call bcast_forces_ctl(model_ctl%frc_ctl)
      call bcast_gravity_ctl(model_ctl%g_ctl)
      call bcast_coriolis_ctl(model_ctl%cor_ctl)
      call bcast_magneto_ctl(model_ctl%mcv_ctl)
      call bcast_magnetic_scale_ctl(model_ctl%bscale_ctl)
      call bcast_ref_scalar_ctl(model_ctl%reft_ctl)
      call bcast_ref_scalar_ctl(model_ctl%refc_ctl)
      call bcast_sgs_ctl(model_ctl%sgs_ctl)
!
      call calypso_mpi_bcast_one_int(model_ctl%i_model, 0)
!
      end subroutine bcast_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_sgs_mhd_model(model_ctl)
!
      use bcast_4_field_ctl
!
      type(mhd_model_control), intent(inout) :: model_ctl
!
!
      call dealloc_phys_control(model_ctl%fld_ctl)
      call dealloc_t_evo_name_ctl(model_ctl%evo_ctl)
      call dealloc_ele_area_grp_ctl(model_ctl%earea_ctl)
!
      call dealloc_bc_4_node_ctl(model_ctl%nbc_ctl)
      call dealloc_bc_4_surf_ctl(model_ctl%sbc_ctl)
!
      call dealloc_dimless_ctl(model_ctl%dless_ctl)
      call dealloc_coef_term_ctl(model_ctl%eqs_ctl)
      call dealloc_name_force_ctl(model_ctl%frc_ctl)
      call dealloc_gravity_ctl(model_ctl%g_ctl)
      call dealloc_coriolis_ctl(model_ctl%cor_ctl)
      call dealloc_magneto_ctl(model_ctl%mcv_ctl)
      call dealloc_magnetic_scale_ctl(model_ctl%bscale_ctl)
!
      call dealloc_sgs_ctl(model_ctl%sgs_ctl)
!
      model_ctl%i_model = 0
!
      end subroutine dealloc_sph_sgs_mhd_model
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_SGS_MHD_model
