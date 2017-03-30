!fieldline_only.f90
!      module fieldline_only
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine field_line_init_only(mesh, group, fld_nod)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(phys_data), intent(in) :: fld_nod
!!
!!      subroutine field_line_main_only                                 &
!!     &         (istep_fline, mesh, group, surf, next_tbl, fld_nod)
!!        integer(kind = kint), intent(in) :: istep_fline
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(surface_data), intent(in) :: surf
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(phys_data), intent(in) :: fld_nod
!
      module fieldline_only
!
      use m_precision
!
      use t_mesh_data
      use t_surface_data
      use t_next_node_ele_4_node
      use t_phys_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_init_only(mesh, group, fld_nod)
!
      use fieldline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(phys_data), intent(in) :: fld_nod
!
!
      call FLINE_initialize(mesh%node, mesh%ele,                        &
     &    group%ele_grp, group%surf_grp, fld_nod)
!
      end subroutine field_line_init_only
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_only                                   &
     &         (istep_fline, mesh, group, surf, ele_4_nod, fld_nod)
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_fline
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(surface_data), intent(in) :: surf
      type(element_around_node), intent(in) :: ele_4_nod
      type(phys_data), intent(in) :: fld_nod
!
!
      call FLINE_visualize(istep_fline,                                 &
     &    mesh%node, mesh%ele, surf, group%ele_grp,                     &
     &    ele_4_nod, fld_nod, mesh%nod_comm)
!
      end subroutine field_line_main_only
!
!  ---------------------------------------------------------------------
!
      end module fieldline_only
