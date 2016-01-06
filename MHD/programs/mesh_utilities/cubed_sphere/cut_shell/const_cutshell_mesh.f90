!
!      module const_cutshell_mesh
!
!!      subroutine s_const_reduced_geometry                             &
!!     &         (orgmesh, org_group, newmesh, new_group)
!
      module const_cutshell_mesh
!
      use m_precision
!
      use t_mesh_data
      use t_group_data
!
      use m_cutshell_nod_ele_flag
      use set_cutshell_node_data
      use set_cutshell_element
      use set_cutshell_node_grp
      use set_cutshell_ele_grp
      use set_cutshell_surf_grp
!
      implicit none
!
      integer(kind = kint) :: iflag_reduce_type = 1
!
      character(len=kchara) :: original_mesh_head
      character(len=kchara) :: modified_mesh_head = 'in'
!
!
      private :: select_northern_hemisphere, select_cut_shell
      private :: select_spherical_shell, select_hemispherical_shell
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_reduced_geometry                               &
     &         (orgmesh, org_group, newmesh, new_group)
!
      type(mesh_geometry), intent(in) :: orgmesh
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: new_group
!
!
      call allocate_trans_table                                         &
     &   (orgmesh%node%numnod, orgmesh%ele%numele)
!
      if      (iflag_reduce_type .eq. 1) then
         call select_northern_hemisphere                                &
     &      (orgmesh, org_group, newmesh, new_group)
      else if (iflag_reduce_type .eq. 2) then
         call select_cut_shell                                          &
     &      (orgmesh, org_group, newmesh, new_group)
      else if (iflag_reduce_type .eq. 3) then
         call select_spherical_shell                                    &
     &      (orgmesh, org_group, newmesh, new_group)
      else if (iflag_reduce_type .eq. 4) then
         call select_hemispherical_shell                                &
     &      (orgmesh, org_group, newmesh, new_group)
      end if
!
!      call check_trans_table(orgmesh%node%numnod, orgmesh%ele%numele)
!
      call deallocate_trans_table
!
      end subroutine s_const_reduced_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine select_northern_hemisphere                             &
     &         (orgmesh, org_group, newmesh, new_group)
!
      type(mesh_geometry), intent(in) :: orgmesh
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: new_group
!
!
      call set_new_node_4_hemi(orgmesh%node, newmesh%node)
!
      call s_set_new_elements(orgmesh%ele, newmesh%ele)
!
      call s_set_new_node_grp_4_hemi                                    &
     &   (org_group%nod_grp, newmesh%node, new_group%nod_grp)
      call s_set_new_element_grp(org_group%ele_grp, new_group%ele_grp)
      call s_set_new_surface_grp_4_hemi                                 &
     &   (org_group%surf_grp, newmesh, new_group%surf_grp)
!
      end subroutine select_northern_hemisphere
!
!  ---------------------------------------------------------------------
!
      subroutine select_cut_shell                                       &
     &         (orgmesh, org_group, newmesh, new_group)
!
      type(mesh_geometry), intent(in) :: orgmesh
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: new_group
!
!
      call set_new_node_4_cut_shell(orgmesh%node, newmesh%node)
!
      call s_set_new_elements(orgmesh%ele, newmesh%ele)
!
      call s_set_new_node_grp(org_group%nod_grp, new_group%nod_grp)
      call s_set_new_element_grp(org_group%ele_grp, new_group%ele_grp)
      call s_set_new_surface_grp                                        &
     &   (org_group%surf_grp, new_group%surf_grp)
!
      end subroutine select_cut_shell
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine select_spherical_shell                                 &
     &         (orgmesh, org_group, newmesh, new_group)
!
      type(mesh_geometry), intent(in) :: orgmesh
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: new_group
!
!
      call set_new_node_outer_core(orgmesh%node, org_group%nod_grp,     &
     &    newmesh%node)
!
      call s_set_new_elements(orgmesh%ele, newmesh%ele)
!
      call s_set_new_node_grp(org_group%nod_grp, new_group%nod_grp)
      call s_set_new_element_grp(org_group%ele_grp, new_group%ele_grp)
      call s_set_new_surface_grp                                        &
     &   (org_group%surf_grp, new_group%surf_grp)
!
      end subroutine select_spherical_shell
!
!  ---------------------------------------------------------------------
!
      subroutine select_hemispherical_shell                             &
     &         (orgmesh, org_group, newmesh, new_group)
!
      type(mesh_geometry), intent(in) :: orgmesh
      type(mesh_groups), intent(in) :: org_group
!
      type(mesh_geometry), intent(inout) :: newmesh
      type(mesh_groups), intent(inout) :: new_group
!
!
      call set_new_node_hemi_o_core                                     &
     &   (orgmesh%node, org_group%nod_grp, newmesh%node)
!
      call s_set_new_elements(orgmesh%ele, newmesh%ele)
!
      call s_set_new_node_grp_4_hemi                                    &
     &   (org_group%nod_grp, newmesh%node, new_group%nod_grp)
      call s_set_new_element_grp(org_group%ele_grp, new_group%ele_grp)
      call s_set_new_surface_grp_4_hemi                                 &
     &   (org_group%surf_grp, newmesh, new_group%surf_grp)
!
      end subroutine select_hemispherical_shell
!
!  ---------------------------------------------------------------------
!
      end module const_cutshell_mesh

