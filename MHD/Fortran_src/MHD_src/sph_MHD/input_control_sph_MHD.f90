!>@file   input_control_sph_MHD.f90
!!@brief  module input_control_sph_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_mesh(mesh, group, ele_mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!      subroutine input_control_4_SPH_MHD_nosnap
!!
!!      subroutine input_control_4_SPH_make_init
!!      subroutine input_control_SPH_dynamobench
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
      subroutine input_control_SPH_mesh(mesh, group, ele_mesh)
!
      use m_control_parameter
      use m_spheric_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD
      call set_control_4_SPH_to_FEM
!
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_mesh'
      call load_para_SPH_and_FEM_mesh(mesh, group, ele_mesh)
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
      subroutine input_control_4_SPH_MHD_nosnap
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
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
      subroutine input_control_4_SPH_make_init
!
      use m_control_parameter
      use m_sph_boundary_input_data
      use set_control_sph_mhd
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      end subroutine input_control_4_SPH_make_init
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamobench
!
      use m_control_parameter
      use set_control_sph_mhd
      use set_control_sph_data_MHD
      use parallel_load_data_4_sph
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_MHD'
      call set_control_4_SPH_MHD
      call set_control_4_SPH_to_FEM
      call set_ctl_params_dynamobench
!
      if (iflag_debug.eq.1) write(*,*) 'load_para_sph_mesh'
      call load_para_sph_mesh
!
      end subroutine input_control_SPH_dynamobench
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_4_SPH_to_FEM
!
      use m_node_phys_data
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_ctl_data_4_sphere_model
!
      use t_phys_data
!
      use ordering_field_by_viz
      use node_monitor_IO
      use set_controls_4_sph_shell
!
!
      call set_FEM_mesh_mode_4_SPH(iflag_shell_mode)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     write(*,*) 'copy_rj_spec_name_to_nod_fld'
      call copy_field_name_type(rj_fld1, nod_fld1)
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &     call check_nodal_field_name_type(6, nod_fld1)
!
      call count_field_4_monitor                                        &
     &   (rj_fld1%num_phys, rj_fld1%num_component,                      &
     &    rj_fld1%iflag_monitor, num_field_monitor, ntot_comp_monitor)
!
      end subroutine set_control_4_SPH_to_FEM
!
! -----------------------------------------------------------------------
!
      end module input_control_sph_MHD
