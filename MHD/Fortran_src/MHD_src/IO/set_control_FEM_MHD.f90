!>@file   set_control_FEM_MHD.f90
!!@brief  module set_control_FEM_MHD
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for MHD dynamo simulation from control data
!!
!!@verbatim
!!      subroutine set_control_4_FEM_MHD                                &
!!     &         (mesh_file, udt_org_param, nod_fld)
!!        type(field_IO_params), intent(inout) :: mesh_file
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_control_FEM_MHD
!
      use m_precision
      use t_phys_data
      use t_file_IO_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_FEM_MHD                                  &
     &         (mesh_file, udt_org_param, nod_fld)
!
      use calypso_mpi
      use m_ucd_data
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_ctl_data_4_fields
      use m_ctl_data_node_boundary
      use m_ctl_data_mhd_forces
      use m_ctl_data_SGS_model
      use m_ctl_data_temp_model
      use m_ctl_data_mhd_evolution
      use m_ctl_data_mhd_evo_scheme
!
      use set_control_platform_data
      use set_control_nodal_data_MHD
      use set_ctl_parallel_platform
      use set_ctl_params_2nd_files
      use set_control_4_time_steps
!
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_SGS
      use set_control_4_filtering
      use set_control_4_model
      use set_control_4_scheme
      use set_control_4_solver
      use set_control_evo_layers
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_vect_p
      use set_control_4_magne
      use set_control_4_mag_p
      use set_control_4_current
      use set_control_4_composition
      use set_control_4_infty
      use fem_mhd_rst_IO_control
      use check_read_bc_file
!
      type(field_IO_params), intent(inout) :: mesh_file
      type(field_IO_params), intent(inout) :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank, plt1)
      call check_control_num_domains(plt1)
      call set_control_smp_def(my_rank, plt1)
      call set_control_mesh_def(plt1, mesh_file)
      call set_ctl_restart_4_fem_mhd(plt1)
      call set_control_MHD_field_file
      call set_control_org_udt_file_def(udt_org_param)
!
!   set parameters for general information
!
      call s_set_control_4_model(reft_ctl1, mevo_ctl1, evo_ctl1)
!
!   set element groups for evolution
!
      call s_set_control_evo_layers(earea_ctl1)
!
!   set forces
!
      call s_set_control_4_force(frc_ctl1, g_ctl1, cor_ctl1, mcv_ctl1)
!
!   set parameters for SGS model
!
      call set_control_SGS_model(sgs_ctl1)
      call set_control_FEM_SGS                                          &
     &   (sgs_ctl1%ffile_ctl, sgs_ctl1, sgs_ctl1%elayer_ctl)
!
!   set parameters for filtering operation
!
      call s_set_control_4_filtering(sgs_ctl1%SGS_filter_name_ctl,      &
     &    sgs_ctl1%ffile_ctl, sgs_ctl1%s3df_ctl)
!
!   set fields
!
      call set_control_4_fields(fld_ctl1%field_ctl, nod_fld)
!
!   set control parameters
!
      call s_set_control_4_normalize
!
!   set boundary conditions for temperature
!
      call s_set_control_4_temp(nbc_ctl1%node_bc_T_ctl)
!
!   set boundary conditions for velocity
!
      call s_set_control_4_velo(nbc_ctl1%node_bc_U_ctl)
!
!  set boundary conditions for pressure
!
      call s_set_control_4_press(nbc_ctl1%node_bc_P_ctl)
!
!   set boundary conditions for composition
!
      call s_set_control_4_composition(nbc_ctl1%node_bc_C_ctl)
!
!   set boundary_conditons for magnetic field
!
      call s_set_control_4_magne(nbc_ctl1%node_bc_B_ctl)
!
!   set boundary_conditons for magnetic potential
!
      call s_set_control_4_mag_p(nbc_ctl1%node_bc_MP_ctl)
!
!   set boundary_conditons for vector potential
!
      call s_set_control_4_vect_p(nbc_ctl1%node_bc_A_ctl)
!
!   set boundary_conditons for current density
!
      call s_set_control_4_current(nbc_ctl1%node_bc_J_ctl)
!
!   set boundary_conditons for magnetic potential
!
      call s_set_control_4_infty
!
!   set flag to read boundary condition file
!
      call check_read_boundary_files
!
!   set control parameters
!
      call s_set_control_4_time_steps(tctl1)
      call s_set_control_4_crank(mevo_ctl1)
!
      call s_set_control_4_solver(mevo_ctl1)
      call set_control_4_FEM_params(mevo_ctl1)
!
      end subroutine set_control_4_FEM_MHD
!
! -----------------------------------------------------------------------
!
      end module set_control_FEM_MHD
