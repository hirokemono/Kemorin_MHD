!fieldline_type.f90
!      module fieldline_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine field_line_init_type(fem, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(phys_data), intent(in) :: fld_nod
!
!!      subroutine field_line_main_type(istep_psf, fem, surf, next_tbl, &
!!     &          fld_nod)
!        integer(kind = kint), intent(in) :: istep_psf
!        integer(kind = kint), intent(in) :: istep_iso
!        type(mesh_geometry), intent(in) :: mesh
!        type(edge_data), intent(in) :: edge
!        type(phys_data), intent(in) :: fld_nod
!
      module fieldline_type
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_init_type(fem, fld_nod)
!
      use t_mesh_data
      use t_phys_data
!
      use fieldline
!
      type(mesh_data), intent(in) :: fem
      type(phys_data), intent(in) :: fld_nod
!
!
      call field_line_init(fem%mesh%node%numnod,                        &
     &    fem%mesh%ele%numele, fem%mesh%ele%e_multi,                    &
     &    fem%group%ele_grp%num_grp, fem%group%ele_grp%num_item,        &
     &    fem%group%ele_grp%grp_name, fem%group%ele_grp%istack_grp,     &
     &    fem%group%ele_grp%item_grp, fem%group%surf_grp%num_grp,       &
     &    fem%group%surf_grp%num_item, fem%group%surf_grp%grp_name,     &
     &    fem%group%surf_grp%istack_grp,                                &
     &    fem%group%surf_grp%item_sf_grp,                               &
     &    fld_nod%num_phys, fld_nod%phys_name)
!
      end subroutine field_line_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_type(istep_psf, fem, surf, next_tbl,   &
     &          fld_nod)
!
      use t_mesh_data
      use t_surface_data
      use t_next_node_ele_4_node
      use t_phys_data
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_psf
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: fld_nod
!
!
      call field_line_main(istep_psf,                                   &
     &    fem%mesh%node%numnod, fem%mesh%ele%numele, surf%numsurf,      &
     &    surf%nnod_4_surf, fem%mesh%node%istack_nod_smp,               &
     &    fem%mesh%node%inod_global, fem%mesh%node%xx,                  &
     &    fem%mesh%node%rr, fem%mesh%node%a_r,                          &
     &    fem%mesh%node%ss, fem%mesh%node%a_s,                          &
     &    fem%mesh%ele%iele_global, fem%mesh%ele%e_multi,               &
     &    surf%ie_surf, surf%isf_4_ele, surf%iele_4_surf, surf%x_surf,  &
     &    surf%vnorm_surf, surf%area_surf, surf%interior_surf,          &
     &    fem%group%ele_grp%num_grp, fem%group%ele_grp%num_item,        &
     &    fem%group%ele_grp%istack_grp,  fem%group%ele_grp%item_grp,    &
     &    next_tbl%neib_ele%ntot, next_tbl%neib_ele%istack_4_node,      &
     &    next_tbl%neib_ele%iele_4_node,                                &
     &    fld_nod%num_phys, fld_nod%ntot_phys,                          &
     &    fld_nod%istack_component, fld_nod%d_fld, fem%mesh%nod_comm)
!
      end subroutine field_line_main_type
!
!  ---------------------------------------------------------------------
!
      end module fieldline_type
