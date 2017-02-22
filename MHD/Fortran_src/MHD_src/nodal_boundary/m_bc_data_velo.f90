!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_boundary_data(IO_bc, mesh, ele_mesh, MHD_mesh,   &
!!     &          group, fl_prop, iphys, nod_fld)
!!        type(IO_boundary), intent(in) :: IO_bc
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!
      module m_bc_data_velo
!
      use m_precision
      use t_bc_data_MHD
      use t_MHD_boundary_data
!
      implicit  none
!
      type(nodal_boundarty_conditions), save :: nod1_bcs
!
      type(surface_boundarty_conditions), save :: sf1_bcs
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_boundary_data(IO_bc, mesh, ele_mesh, MHD_mesh,     &
     &          group, fl_prop, iphys, nod_fld)
!
      use m_machine_parameter
!
      use m_control_parameter
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
      use t_boundary_field_IO
      use t_physical_property
      use t_bc_data_MHD
!
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use set_surface_values
      use set_normal_field
!
      type(IO_boundary), intent(in) :: IO_bc
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(mesh_groups), intent(in) ::   group
      type(phys_address), intent(in) :: iphys
      type(fluid_property), intent(in) :: fl_prop
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_id_data'
      call set_bc_id_data                                               &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp,                    &
     &    IO_bc, mesh, group, MHD_mesh, fl_prop, nod1_bcs)
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_fields'
      call set_bc_fields                                                &
     &   (evo_magne, evo_vect_p, evo_temp, evo_comp,                    &
     &    mesh, fl_prop, iphys, nod_fld, nod1_bcs)
!
      call set_bc_surface_data                                          &
     &   (IO_bc, mesh%node, mesh%ele, ele_mesh%surf,                    &
     &    group%surf_grp, group%surf_nod_grp, group%surf_grp_geom,      &
     &    fl_prop, sf1_bcs)
!
!     set normal velocity
      call set_normal_velocity                                          &
     &   (group%surf_grp, group%surf_nod_grp, fl_prop,                  &
     &    sf1_bcs%Vsf_bcs%normal, iphys%i_velo, nod_fld)
!
      end subroutine set_boundary_data
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_velo
