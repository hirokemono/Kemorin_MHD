!write_cubed_sph_grp_data.f90
!      module write_cubed_sph_grp_data
!
!      Written by Kemorin on Apr., 2006
!
!!      subroutine output_group_data(c_sphere)
!!      subroutine output_group_data_quad(c_sphere, csph_mesh)
!!        type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!!        type(cubed_sph_mesh), intent(in) :: csph_mesh
!!      subroutine output_coarse_group_data
!
      module write_cubed_sph_grp_data
!
      use m_precision
!
      use m_constants
      use m_numref_cubed_sph
      use m_cubed_sph_grp_param
!
      use t_mesh_data
      use t_group_data
      use t_cubed_sph_surf_mesh
      use t_cubed_sph_mesh
!
      use cubed_sph_file_names
      use set_node_groups_4_shell
      use set_element_groups_4_shell
      use set_surface_groups_4_shell
!
      implicit none
!
      type(mesh_groups), save, private :: group_csph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data(c_sphere)
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
!
!   set groups
!
      write(*,*) 'output group information for linear'
!
      call count_node_groups_linear                                     &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf,                     &
     &    ione, group_csph%nod_grp)
!
      call allocate_grp_type_num(group_csph%nod_grp)
      call set_node_group_names(group_csph%nod_grp)
      call set_node_istack_linear                                       &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf,                     &
     &    ione, group_csph%nod_grp)
!
      call allocate_grp_type_item(group_csph%nod_grp)
      call set_nodal_item_linear                                        &
     &   (c_sphere%numnod_cube, c_sphere%numnod_sf, num_hemi,           &
     &    ione, group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_l_group, group_csph%nod_grp)
!
!   set element group
!
      call count_ele_groups                                             &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call allocate_grp_type_num(group_csph%ele_grp)
      call set_element_group_names(group_csph%ele_grp)
      call set_ele_grp_istack                                           &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call allocate_grp_type_item(group_csph%ele_grp)
      call set_ele_item                                                 &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call write_element_group(id_l_group, group_csph%ele_grp)
!
!
! surface group
!
      call count_surf_groups                                            &
     &   (c_sphere%numele_sf, ione, group_csph%surf_grp)
!
      call allocate_sf_grp_type_num(group_csph%surf_grp)
      call set_surface_group_names(group_csph%surf_grp)
      call set_surf_istack                                              &
     &   (c_sphere%numele_sf, ione, group_csph%surf_grp)
!
      call allocate_sf_grp_type_item(group_csph%surf_grp)
      call set_surf_item                                                &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_l_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      return
      end subroutine output_group_data
!
!   --------------------------------------------------------------------
!
      subroutine output_group_data_quad(c_sphere, csph_mesh)
!
      type(cubed_sph_surf_mesh), intent(in) :: c_sphere
      type(cubed_sph_mesh), intent(in) :: csph_mesh
!
!   set groups
!
      write(*,*) 'output quad group information'
!
      call count_node_groups_quad                                       &
     &   (c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, group_csph%nod_grp)
!
      call allocate_grp_type_num(group_csph%nod_grp)
      call set_node_group_names(group_csph%nod_grp)
      call set_node_istack_quad                                         &
     &   (c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, group_csph%nod_grp)
!
      call allocate_grp_type_item(group_csph%nod_grp)
      call set_nodal_item_quad(csph_mesh%nnod_cb_sph,                   &
     &    c_sphere%numnod_cube, c_sphere%numedge_cube,                  &
     &    c_sphere%numnod_sf, c_sphere%numedge_sf, num_hemi, ione,      &
     &    group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_q_group, group_csph%nod_grp)
!
!   set element group
!
      call count_ele_groups                                             &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call allocate_grp_type_num(group_csph%ele_grp)
      call set_element_group_names(group_csph%ele_grp)
      call set_ele_grp_istack                                           &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call allocate_grp_type_item(group_csph%ele_grp)
      call set_ele_item                                                 &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%ele_grp)
!
      call write_element_group(id_q_group, group_csph%ele_grp)
!
! surface group
!
      call count_surf_groups                                            &
     &   (c_sphere%numele_sf, ione, group_csph%surf_grp)
!
      call allocate_sf_grp_type_num(group_csph%surf_grp)
      call set_surface_group_names(group_csph%surf_grp)
      call set_surf_istack                                              &
     &   (c_sphere%numele_sf, ione, group_csph%surf_grp)
!
      call allocate_sf_grp_type_item(group_csph%surf_grp)
      call set_surf_item                                                &
     &   (c_sphere%numele_cube, c_sphere%numele_sf,                     &
     &    ione, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_q_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      end subroutine output_group_data_quad
!
!   --------------------------------------------------------------------
!
      subroutine output_coarse_group_data
!
!   set groups
!
      call count_node_groups_linear                                     &
     &   (nnod_cube_c, nnod_sf_c, nskip_r, group_csph%nod_grp)
!
      call allocate_grp_type_num(group_csph%nod_grp)
      call set_node_group_names(group_csph%nod_grp)
      call set_node_istack_linear                                       &
     &   (nnod_cube_c, nnod_sf_c, nskip_r, group_csph%nod_grp)
!
      call allocate_grp_type_item(group_csph%nod_grp)
      call set_nodal_item_linear(nnod_cube_c, nnod_sf_c, n_hemi_c,      &
     &    nskip_r, group_csph%nod_grp)
!
      call write_cubed_sph_nod_grp(id_l_group, group_csph%nod_grp)
!
!
!   set element group
!
      call count_ele_groups                                             &
     &   (nele_cube_c, nele_sf_c, nskip_r, group_csph%ele_grp)
!
      call allocate_grp_type_num(group_csph%ele_grp)
      call set_element_group_names(group_csph%ele_grp)
      call set_ele_grp_istack                                           &
     &   (nele_cube_c, nele_sf_c, nskip_r, group_csph%ele_grp)
!
      call allocate_grp_type_item(group_csph%ele_grp)
      call set_ele_item                                                 &
     &   (nele_cube_c, nele_sf_c, nskip_r, group_csph%ele_grp)
!
      call write_element_group(id_l_group, group_csph%ele_grp)
!
!
! surface group
!
      call count_surf_groups(nele_sf_c, nskip_r, group_csph%surf_grp)
!
      call allocate_sf_grp_type_num(group_csph%surf_grp)
      call set_surface_group_names(group_csph%surf_grp)
      call set_surf_istack(nele_sf_c, nskip_r, group_csph%surf_grp)
!
      call allocate_sf_grp_type_item(group_csph%surf_grp)
      call set_surf_item                                                &
     &   (nele_cube_c, nele_sf_c, nskip_r, group_csph%surf_grp)
!
      call write_surf_grp_shell(id_l_group, group_csph%surf_grp)
!
      call dealloc_groups_data(group_csph)
!
      end subroutine output_coarse_group_data
!
!   --------------------------------------------------------------------
!
      end module write_cubed_sph_grp_data
