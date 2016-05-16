!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_mesh                               &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,    &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,         &
!!     &          radial_rj_grp, sphere_rj_grp, rj_fld,                 &
!!     &          mesh, group, ele_mesh)
!!      subroutine input_control_4_SPH_MHD_nosnap                       &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,    &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,         &
!!     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!!
!!      subroutine input_control_4_SPH_make_init                        &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,    &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,         &
!!     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!!      subroutine input_control_SPH_dynamobench                        &
!!     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,        &
!!     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,    &
!!     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,         &
!!     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) ::  sph_rj
!!        type(sph_comm_tbl), intent(inout) :: comm_rtp
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rj
!!        type(group_data), intent(inout) :: bc_rtp_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!        type(group_data), intent(inout) :: zonal_rtp_grp
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!@endverbatim
!
!
      module input_control_sph_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_mesh_data
      use t_phys_data
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
!
      implicit none
!
      private :: set_control_4_SPH_to_FEM
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_mesh                                 &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,      &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,           &
     &          radial_rj_grp, sphere_rj_grp, rj_fld,                   &
     &          mesh, group, ele_mesh)
!
      use m_control_parameter
!      use m_spheric_parameter
!      use m_sph_trans_comm_table
!      use m_group_data_sph_specr
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      type(phys_data), intent(inout) :: rj_fld
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(rj_fld)
      call set_control_4_SPH_to_FEM(sph_params, rj_fld)
!
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh                                   &
     &   (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,                &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,            &
     &    radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,                 &
     &    radial_rj_grp, sphere_rj_grp, mesh, group, ele_mesh)
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
        call read_boundary_spectr_file
      end if
!
      end subroutine input_control_SPH_mesh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_MHD_nosnap                         &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,      &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,           &
     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,                &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,            &
     &    radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,                 &
     &    radial_rj_grp, sphere_rj_grp)
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        if (iflag_debug.eq.1) write(*,*) 'read_boundary_spectr_file'
        call read_boundary_spectr_file
      end if
!
      end subroutine input_control_4_SPH_MHD_nosnap
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_4_SPH_make_init                          &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,      &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,           &
     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(rj_fld)
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,                &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,            &
     &    radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,                 &
     &    radial_rj_grp, sphere_rj_grp)
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench                          &
     &         (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,          &
     &          comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,      &
     &          radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,           &
     &          radial_rj_grp, sphere_rj_grp, rj_fld)
!
      use m_control_parameter
      use set_control_sph_mhd
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) ::  sph_rj
!
      type(sph_comm_tbl), intent(inout) :: comm_rtp
      type(sph_comm_tbl), intent(inout) :: comm_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rj
!
      type(group_data), intent(inout) :: bc_rtp_grp
      type(group_data), intent(inout) :: radial_rtp_grp
      type(group_data), intent(inout) :: theta_rtp_grp
      type(group_data), intent(inout) :: zonal_rtp_grp
!
      type(group_data), intent(inout) :: radial_rj_grp
      type(group_data), intent(inout) :: sphere_rj_grp
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD(rj_fld)
      call set_control_4_SPH_to_FEM(sph_params, rj_fld)
      call set_ctl_params_dynamobench
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh                                           &
     &   (sph_params, sph_rtp, sph_rtm, sph_rlm, sph_rj,                &
     &    comm_rtp, comm_rtm, comm_rlm, comm_rj, bc_rtp_grp,            &
     &    radial_rtp_grp, theta_rtp_grp, zonal_rtp_grp,                 &
     &    radial_rj_grp, sphere_rj_grp)
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_to_FEM(sph_params, rj_fld)
!
      use m_node_phys_data
      use m_ctl_data_4_sphere_model
!
      use ordering_field_by_viz
      use node_monitor_IO
      use set_controls_4_sph_shell
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_FEM_mesh_mode_4_SPH(sph_params%iflag_shell_mode)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'copy_rj_spec_name_to_nod_fld'
      call copy_field_name_type(rj_fld, nod_fld1)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     call check_nodal_field_name_type(6, nod_fld1)
!
      call count_field_4_monitor                                        &
     &   (rj_fld%num_phys, rj_fld%num_component,                        &
     &    rj_fld%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_4_SPH_to_FEM
!
! -----------------------------------------------------------------------
!
      end module input_control_sph_MHD
