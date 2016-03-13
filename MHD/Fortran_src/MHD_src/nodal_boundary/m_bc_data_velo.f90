!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!!      subroutine set_surf_bc_data                                     &
!!     &         (node, ele, surf, sf_grp, sf_grp_nod, sf_grp_v)
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(surface_node_grp_data), intent(in) :: sf_grp_nod
!!        type(surface_group_geometry), intent(in) :: sf_grp_v
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
      subroutine set_surf_bc_data(node, ele, surf, sf_grp, sf_grp_nod,  &
     &          sf_grp_v, iphys, nod_fld)
!
      use m_machine_parameter
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_surface_group_connect
      use t_surface_group_geometry
      use t_phys_data
      use t_phys_address
!
      use set_surface_id_MHD
      use set_surface_values
      use set_normal_field
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(surface_node_grp_data), intent(in) :: sf_grp_nod
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call set_bc_surface_data(node, ele, surf,                         &
     &    sf_grp, sf_grp_nod, sf_grp_v, sf1_bcs)
!
!     set normal velocity
      if (iflag_t_evo_4_velo .gt. id_no_evolution) then
        call set_normal_velocity(sf_grp, sf_grp_nod,                    &
     &      sf1_bcs%Vsf_bcs%normal, iphys%i_velo, nod_fld)
      end if
!
      end subroutine set_surf_bc_data
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_velo
