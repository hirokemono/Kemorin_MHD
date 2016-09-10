!>@file   set_control_sph_mhd.f90
!!@brief  module set_control_sph_mhd
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Set control data for spherical transform MHD dynamo simulation
!!
!!@verbatim
!!      subroutine set_control_4_SPH_MHD                                &
!!     &         (sph_gen, rj_fld, sph_file_param, sph_fst_IO, pwr)
!!        type(sph_grids), intent(inout) :: sph_gen
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO_params), intent(inout) :: sph_file_param
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!        type(sph_mean_squares), intent(inout) :: pwr
!!@endverbatim
!
      module set_control_sph_mhd
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_MHD                                  &
     &         (sph_gen, rj_fld, sph_file_param, sph_fst_IO, pwr)
!
      use m_control_params_2nd_files
      use m_spheric_global_ranks
      use m_ucd_data
      use m_read_ctl_gen_sph_shell
      use sph_mhd_rms_IO
!
      use t_spheric_parameter
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_field_data_IO
!
      use gen_sph_grids_modes
      use set_control_platform_data
      use set_ctl_parallel_platform
      use set_control_4_model
      use set_control_sph_data_MHD
      use set_control_4_force
      use set_control_4_normalize
      use set_control_4_time_steps
!
      use set_control_4_velo
      use set_control_4_press
      use set_control_4_temp
      use set_control_4_magne
      use set_control_4_composition
      use set_control_4_pickup_sph
      use set_ctl_gen_shell_grids
      use check_read_bc_file
!
      use check_dependency_for_MHD
!
      type(sph_grids), intent(inout) :: sph_gen
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO_params), intent(inout) :: sph_file_param
      type(field_IO), intent(inout) :: sph_fst_IO
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: ierr
!
!
!   set parameters for data files
!
      call turn_off_debug_flag_by_ctl(my_rank)
      call check_control_num_domains
      call set_control_smp_def(my_rank)
      call set_control_mesh_def
      call set_FEM_mesh_switch_4_SPH(iflag_output_mesh)
      call set_control_sph_mesh(sph_file_param)
      call set_control_restart_file_def(sph_fst_IO)
      call set_control_MHD_field_file
      call set_control_org_sph_mesh
      call set_control_org_rst_file_def
      call set_control_org_udt_file_def
!
      call s_set_control_4_model
!
!   set spherical shell parameters
!
      iflag_make_SPH = i_sph_shell
      if(iflag_make_SPH .gt. 0) then
        if (iflag_debug.gt.0) write(*,*) 'set_control_4_shell_grids'
        call set_control_4_shell_grids(nprocs, sph_gen, ierr)
      end if
!
!   set forces
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_force'
      call s_set_control_4_force
!
!   set parameters for general information
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_sph_data_MHD'
      call s_set_control_sph_data_MHD(rj_fld)
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_normalize'
      call s_set_control_4_normalize
!
!   set boundary conditions for temperature
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_temp'
      call s_set_control_4_temp
!
!   set boundary conditions for velocity
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_velo'
      call s_set_control_4_velo
!
!  set boundary conditions for pressure
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_press'
      call s_set_control_4_press
!
!   set boundary conditions for composition variation
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_composition'
      call s_set_control_4_composition
!
!   set boundary_conditons for magnetic field
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_magne'
      call s_set_control_4_magne
!
!   set flag to read boundary condition file
!
      if (iflag_debug.gt.0) write(*,*) 'check_read_boundary_files'
      call check_read_boundary_files
!
!   set control parameters
!
      if (iflag_debug.gt.0) write(*,*) 's_set_control_4_time_steps'
      call s_set_control_4_time_steps
      call s_set_control_4_crank
!
!  check dependencies
!
      call check_dependencies(rj_fld%num_phys, rj_fld%phys_name)
!
!   set_pickup modes
!
      call set_ctl_params_pick_sph                                      &
     &   (pwr, pickup_sph_head, pick_list1, pick1)
      call set_ctl_params_pick_gauss                                    &
     &   (gauss_coefs_file_head, gauss_list1, gauss1)
      call set_ctl_params_no_heat_Nu(rj_fld, Nu_type1)
!
      end subroutine set_control_4_SPH_MHD
!
! ----------------------------------------------------------------------
!
      end module set_control_sph_mhd
