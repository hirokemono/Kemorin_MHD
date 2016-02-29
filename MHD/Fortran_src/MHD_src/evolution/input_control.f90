!>@file   input_control.f90
!!@brief  module input_control
!!
!!@author H.Matsui and H.Okuda
!!@date     Programmed by H.Matsui and H.Okuda on July 2000 (ver 1.1)
!!@n        Modified by H. Matsui on July, 2006
!!@n        Modified by H. Matsui on May, 2007
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_4_MHD(mesh, group, ele_mesh)
!!      subroutine input_control_4_snapshot(mesh, group, ele_mesh)
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::   group
!!        type(element_geometry), intent(inout) :: ele_mesh
!!@endverbatim
!
!
      module input_control
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
      private :: input_meshes_4_MHD
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_MHD(mesh, group, ele_mesh)
!
      use m_ctl_data_fem_MHD
      use m_solver_djds_MHD
      use m_iccg_parameter
      use m_flags_4_solvers
      use set_control_FEM_MHD
      use load_mesh_data
      use input_MG_data
      use skip_comment_f
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_MHD'
      call read_control_4_fem_MHD
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD
!
!  --  load FEM mesh data
      call input_mesh(my_rank, mesh, group,                             &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(mesh, group)
!
      if(cmp_no_case(method_4_solver, cflag_mgcg)) then
        call alloc_MHD_MG_DJDS_mat(num_MG_level, MHD1_matrices)
        call input_MG_mesh
        call input_MG_itp_tables(MHD1_matrices%MG_interpolate)
      else
        call alloc_MHD_MG_DJDS_mat(izero, MHD1_matrices)
      end if
!
      end subroutine input_control_4_MHD
!
! ----------------------------------------------------------------------
!
      subroutine input_control_4_snapshot(mesh, group, ele_mesh)
!
      use m_ctl_data_fem_MHD
      use set_control_FEM_MHD
      use load_mesh_data
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::   group
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'read_control_4_fem_snap'
      call read_control_4_fem_snap
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_FEM_MHD'
      call set_control_4_FEM_MHD
!
!  --  load FEM mesh data
      call input_mesh(my_rank, mesh, group,                             &
     &    ele_mesh%surf%nnod_4_surf, ele_mesh%edge%nnod_4_edge)
!
      call input_meshes_4_MHD(mesh, group)
!
      end subroutine input_control_4_snapshot
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine input_meshes_4_MHD(mesh, group)
!
      use m_machine_parameter
      use m_control_parameter
      use m_read_mesh_data
!
      use element_IO_select
      use surface_IO_select
      use edge_IO_select
      use set_3d_filtering_group_id
      use read_filtering_data
      use set_surface_data_4_IO
      use set_edge_data_4_IO
      use node_monitor_IO
      use read_bc_values_file
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
!
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 'set_local_node_id_4_monitor'
      call set_local_node_id_4_monitor(mesh%node, group%nod_grp)
!
! ----  open data file for boundary data
!
      if (iflag_boundary_file .eq. id_read_boundary_file) then
        call s_read_bc_values_file                                      &
     &     (my_rank, group%nod_grp, group%surf_grp)
      end if
!
! ---------------------------------
!
      if (iflag_debug .ge. iflag_routine_msg)                           &
     &      write(*,*) 's_read_filtering_data'
      call s_read_filtering_data(mesh%node, mesh%ele)
!
      if     (iflag_SGS_filter .eq. id_SGS_3D_FILTERING                 &
     &   .or. iflag_SGS_filter .eq. id_SGS_3D_EZ_FILTERING              &
     &   .or. iflag_SGS_filter .eq. id_SGS_3D_SMP_FILTERING             &
     &   .or. iflag_SGS_filter .eq. id_SGS_3D_EZ_SMP_FILTERING ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &       write(*,*) 's_set_3d_filtering_group_id'
        call s_set_3d_filtering_group_id
!
        if (iflag_SGS_model .eq. id_SGS_similarity                      &
     &       .and. iflag_dynamic_SGS .eq. id_SGS_DYNAMIC_ON) then
          if (iflag_debug .ge. iflag_routine_msg)                       &
     &         write(*,*) 's_set_w_filtering_group_id'
          call s_set_w_filtering_group_id
        end if
      end if
!
! ---------------------------------
!
      end subroutine input_meshes_4_MHD
!
! ----------------------------------------------------------------------
!
      end module input_control
