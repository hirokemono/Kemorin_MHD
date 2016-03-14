!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_boundary_data(mesh, ele_mesh, MHD_mesh, group,   &
!!     &          iphys, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(mesh_data_MHD), intent(in) :: MHD_mesh
!!        type(mesh_groups), intent(in) ::   group
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
      subroutine set_boundary_data(mesh, ele_mesh, MHD_mesh, group,     &
     &          iphys, nod_fld)
!
      use m_machine_parameter
!
      use t_mesh_data
      use t_geometry_data_MHD
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
!
      use set_nodal_bc_id_data
      use set_surface_id_MHD
      use set_surface_values
      use set_normal_field
!
      type(mesh_geometry), intent(in) :: mesh
      type(element_geometry), intent(in) :: ele_mesh
      type(mesh_data_MHD), intent(in) :: MHD_mesh
      type(mesh_groups), intent(in) ::   group
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      if (iflag_debug.eq.1) write(*,*)' set_bc_id_data'
      call set_bc_id_data(mesh%node, mesh%ele, group%nod_grp,           &
     &    MHD_mesh, iphys, nod_fld, nod1_bcs)
!
      call set_bc_surface_data(mesh%node, mesh%ele, ele_mesh%surf,      &
     &    group%surf_grp, group%surf_nod_grp, group%surf_grp_geom,      &
     &    sf1_bcs)
!
!     set normal velocity
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_normal_velocity(group%surf_grp, group%surf_nod_grp,    &
     &      sf1_bcs%Vsf_bcs%normal, iphys%i_velo, nod_fld)
      end if
!
      end subroutine set_boundary_data
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_velo
