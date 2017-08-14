!>@file   t_FEM_MHD_boundary_data.f90
!!@brief  module t_FEM_MHD_boundary_data
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2009
!
!>    @brief flux boundary condition lists for MHD dynamo model
!!
!!@verbatim
!!      subroutine set_boundary_data                                    &
!!     &         (time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,       &
!!     &          MHD_prop, MHD_BC, iphys, nod_fld, FEM_MHD_BCs)
!!        type(time_data), intent(in) :: time_d
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(MHD_BC_lists), intent(in) :: MHD_BC
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
!!@endverbatim
!
      module t_FEM_MHD_boundary_data
!
      use m_precision
      use t_bc_data_MHD
      use t_surface_bc_data_MHD
!
      implicit  none
!
      type FEM_MHD_BC_data
!         Nodal boundary condition data for FEM_MHD
        type(nodal_boundarty_conditions) :: nod_bcs
!         Surface boundary condition data for FEM_MHD
        type(surface_boundarty_conditions) :: surf_bcs
      end type FEM_MHD_BC_data
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine set_boundary_data                                      &
     &         (time_d, IO_bc, mesh, ele_mesh, MHD_mesh, group,         &
     &          MHD_prop, MHD_BC, iphys, nod_fld, FEM_MHD_BCs)
!
      use m_machine_parameter
!
      use t_time_data
      use t_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
      use t_boundary_field_IO
      use t_physical_property
      use t_reference_scalar_param
      use t_bc_data_MHD
      use t_bc_data_list
!
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use set_surface_values
      use set_normal_field
!
      type(time_data), intent(in) :: time_d
      type(IO_boundary), intent(in) :: IO_bc
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(mesh_groups), intent(in) ::   group
      type(phys_address), intent(in) :: iphys
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(MHD_BC_lists), intent(in) :: MHD_BC
!
      type(phys_data), intent(inout) :: nod_fld
      type(FEM_MHD_BC_data), intent(inout) :: FEM_MHD_BCs
!
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_id_data'
      call set_bc_id_data(time_d%dt, IO_bc, mesh, group, MHD_mesh,      &
     &    MHD_prop, MHD_BC, FEM_MHD_BCs%nod_bcs)
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_fields'
      call set_bc_fields(time_d%time, mesh,                             &
     &    MHD_prop%fl_prop, MHD_prop%cd_prop,                           &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    MHD_prop%ref_param_T, MHD_prop%ref_param_C,                   &
     &    iphys, nod_fld, FEM_MHD_BCs%nod_bcs)
!
      call set_bc_surface_data                                          &
     &   (IO_bc, mesh%node, mesh%ele, ele_mesh%surf,                    &
     &    group%surf_grp, group%surf_nod_grp, group%surf_grp_geom,      &
     &    MHD_prop, MHD_BC, FEM_MHD_BCs%surf_bcs)
!
!     set normal velocity
      call set_normal_velocity                                          &
     &   (group%surf_grp, group%surf_nod_grp, MHD_prop%fl_prop,         &
     &    FEM_MHD_BCs%surf_bcs%Vsf_bcs%normal, iphys%i_velo, nod_fld)
!
      end subroutine set_boundary_data
!
!  ---------------------------------------------------------------------
!
      end module t_FEM_MHD_boundary_data
