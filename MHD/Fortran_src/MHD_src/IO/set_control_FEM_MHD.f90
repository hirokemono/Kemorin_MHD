!>@file   set_control_FEM_MHD.f90
!!@brief  module set_control_FEM_MHD
!!
!!@author H. Matsui
!!@date Programmed in 2002
!
!> @brief Set parameters for MHD dynamo simulation from control data
!!
!!@verbatim
!!     subroutine set_control_4_FEM_MHD(udt_org_param, nod_fld)
!!        type(field_IO_params), intent(inout) :: udt_org_param
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module set_control_FEM_MHD
!
      use m_precision
      use t_phys_data
      use t_field_data_IO
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_control_4_FEM_MHD(udt_org_param, nod_fld)
!
      use calypso_mpi
      use m_ucd_data
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
      type(field_IO_params), intent(inout) :: udt_org_param
      type(phys_data), intent(inout) :: nod_fld
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call check_control_num_domains
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_ctl_restart_4_fem_mhd
      call set_control_MHD_field_file
      call set_control_org_udt_file_def(udt_org_param)
!
!   set parameters for general information
!
      call s_set_control_4_model
!
!   set element groups for evolution
!
      call s_set_control_evo_layers
!
!   set forces
!
      call s_set_control_4_force
!
!   set parameters for SGS model
!
      call set_control_SGS_model
      call set_control_FEM_SGS
!
!   set parameters for filtering operation
!
      call s_set_control_4_filtering
!
!   set fields
!
      call set_control_4_fields(nod_fld)
!
!   set control parameters
!
      call s_set_control_4_normalize
!
!   set boundary conditions for temperature
!
      call s_set_control_4_temp
!
!   set boundary conditions for velocity
!
      call s_set_control_4_velo
!
!  set boundary conditions for pressure
!
      call s_set_control_4_press
!
!   set boundary conditions for composition
!
      call s_set_control_4_composition
!
!   set boundary_conditons for magnetic field
!
      call s_set_control_4_magne
!
!   set boundary_conditons for magnetic potential
!
      call s_set_control_4_mag_p
!
!   set boundary_conditons for vector potential
!
      call s_set_control_4_vect_p
!
!   set boundary_conditons for current density
!
      call s_set_control_4_current
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
      call s_set_control_4_time_steps
      call s_set_control_4_crank
!
      call s_set_control_4_solver
      call set_control_4_FEM_params
!
      end subroutine set_control_4_FEM_MHD
!
! -----------------------------------------------------------------------
!
      end module set_control_FEM_MHD
