!
!      module const_cutshell_mesh
!
      module const_cutshell_mesh
!
      use m_precision
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
!       subroutine s_const_reduced_geometry
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_const_reduced_geometry
!
!
      call allocate_trans_table
!
      if      (iflag_reduce_type .eq. 1) then
         call select_northern_hemisphere
      else if (iflag_reduce_type .eq. 2) then
         call select_cut_shell
      else if (iflag_reduce_type .eq. 3) then
         call select_spherical_shell
      else if (iflag_reduce_type .eq. 4) then
         call select_hemispherical_shell
      end if
!
!      call check_trans_table
!
      call deallocate_trans_table
!
      end subroutine s_const_reduced_geometry
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine select_northern_hemisphere
!
!
      call set_new_node_4_hemi
!
      call s_set_new_elements
!
      call s_set_new_node_grp_4_hemi
      call s_set_new_element_grp
      call s_set_new_surface_grp_4_hemi
!
      end subroutine select_northern_hemisphere
!
!  ---------------------------------------------------------------------
!
       subroutine select_cut_shell
!
!
      call set_new_node_4_cut_shell
!
      call s_set_new_elements
!
      call s_set_new_node_grp
      call s_set_new_element_grp
      call s_set_new_surface_grp
!
      end subroutine select_cut_shell
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine select_spherical_shell
!
!
      call set_new_node_outer_core
!
      call s_set_new_elements
!
      call s_set_new_node_grp
      call s_set_new_element_grp
      call s_set_new_surface_grp
!
      end subroutine select_spherical_shell
!
!  ---------------------------------------------------------------------
!
       subroutine select_hemispherical_shell
!
!
      call set_new_node_hemi_o_core
!
      call s_set_new_elements
!
      call s_set_new_node_grp_4_hemi
      call s_set_new_element_grp
      call s_set_new_surface_grp_4_hemi
!
      end subroutine select_hemispherical_shell
!
!  ---------------------------------------------------------------------
!
      end module const_cutshell_mesh

