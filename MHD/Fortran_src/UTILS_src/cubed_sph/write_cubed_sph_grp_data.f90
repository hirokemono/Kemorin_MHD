!write_cubed_sph_grp_data.f90
!      module write_cubed_sph_grp_data
!
!      Written by Kemorin on Apr., 2006
!
!      subroutine output_group_data
!
      module write_cubed_sph_grp_data
!
      use m_precision
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_mesh
      use m_cubed_sph_surf_mesh
      use m_cubed_sph_grp_param
!
      use m_node_group
      use m_element_group
      use m_surface_group
!
      use cubed_sph_file_names
      use set_node_groups_4_shell
      use set_element_groups_4_shell
      use set_surface_groups_4_shell
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data
!
!
!   set groups
!
      write(*,*) 'output group information for linear'
!
      call count_node_groups_linear(numnod_cube, numnod_sf, ione)
!
      call allocate_boundary_data
!
      call set_node_group_names
!
      call set_node_istack_linear(numnod_cube, numnod_sf, ione)
!
      call set_nodal_item_linear(numnod_cube, numnod_sf, num_hemi,      &
     &          ione)
!
      call write_node_group(id_l_group)
!
      call deallocate_boundary_data
!
!   set element group
!
      call count_ele_groups(numele_cube, numele_sf, ione)
!
      call allocate_material_data
!
      call set_element_group_names
!
      call set_ele_grp_istack(numele_cube, numele_sf, ione)
!
      call set_ele_item(numele_cube, numele_sf, ione)
!
      call write_element_group(id_l_group)
!
      call deallocate_material_data
!
! surface group
!
      call count_surf_groups(numele_sf, ione)
!
      call allocate_surface_data
!
      call set_surface_group_names
!
      call set_surf_istack(numele_sf, ione)
!
      call set_surf_item(numele_cube, numele_sf, ione)
!
      call write_surface_group(id_l_group)
!
      call deallocate_surface_data
!
      return
      end subroutine output_group_data
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data_quad
!
!   set groups
!
      write(*,*) 'output quad group information'
!
      call count_node_groups_quad(numnod_cube, numedge_cube,            &
     &    numnod_sf, numedge_sf)
!
      call allocate_boundary_data
!
      call set_node_group_names
!
      call set_node_istack_quad(numnod_cube, numedge_cube,              &
     &    numnod_sf, numedge_sf)
!
      call set_nodal_item_quad(numnod, numnod_cube, numedge_cube,       &
     &          numnod_sf, numedge_sf, num_hemi, ione)
!
      call write_node_group(id_q_group)
!
      write(*,*) 'deallocate_boundary_data'
      call deallocate_boundary_data
!
!   set element group
!
      call count_ele_groups(numele_cube, numele_sf, ione)
!
      call allocate_material_data
!
      call set_element_group_names
!
      call set_ele_grp_istack(numele_cube, numele_sf, ione)
!
      call set_ele_item(numele_cube, numele_sf, ione)
!
      call write_element_group(id_q_group)
!
      call deallocate_material_data
!
! surface group
!
      call count_surf_groups(numele_sf, ione)
!
      call allocate_surface_data
!
      call set_surface_group_names
!
      call set_surf_istack(numele_sf, ione)
!
      call set_surf_item(numele_cube, numele_sf, ione)
!
      call write_surface_group(id_q_group)
!
      call deallocate_surface_data
!
      end subroutine output_group_data_quad
!
!   --------------------------------------------------------------------
!
      subroutine output_coarse_group_data(is_level)
!
      integer(kind = kint), intent(in) :: is_level
!
!   set groups
!
      call count_node_groups_linear(nnod_cube_c, nnod_sf_c, nskip_r)
!
      call allocate_boundary_data
!
      call set_node_group_names
!
      call set_node_istack_linear(nnod_cube_c, nnod_sf_c, nskip_r)
!
      call set_nodal_item_linear(nnod_cube_c, nnod_sf_c, n_hemi_c,      &
     &          nskip_r)
!
         write(*,*) 'write_node_group'
      call write_node_group(id_l_group)
!
      call deallocate_boundary_data
!
!   set element group
!
      call count_ele_groups(nele_cube_c, nele_sf_c, nskip_r)
!
      call allocate_material_data
!
      call set_element_group_names
!
      call set_ele_grp_istack(nele_cube_c, nele_sf_c, nskip_r)
!
      call set_ele_item(nele_cube_c, nele_sf_c, nskip_r)
!
      call write_element_group(id_l_group)
!
      call deallocate_material_data
!
! surface group
!
      call count_surf_groups(nele_sf_c, nskip_r)
!
      call allocate_surface_data
!
      call set_surface_group_names
!
      call set_surf_istack(nele_sf_c, nskip_r)
!
      call set_surf_item(nele_cube_c, nele_sf_c, nskip_r)
!
      call write_surface_group(id_l_group)
!
      call deallocate_surface_data
!
      return
      end subroutine output_coarse_group_data
!
!   --------------------------------------------------------------------
!
      end module write_cubed_sph_grp_data
