!fieldline_type.f90
!      module fieldline_type
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine field_line_init_type(fem, fld_nod)
!        type(mesh_data), intent(in) :: fem
!        type(phys_data), intent(in) :: fld_nod
!
!!      subroutine field_line_main_type                                 &
!!     &         (istep_fline, fem, surf, next_tbl, fld_nod)
!!        integer(kind = kint), intent(in) :: istep_fline
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(edge_data), intent(in) :: edge
!!        type(phys_data), intent(in) :: fld_nod
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
      call FLINE_initialize(fem%mesh%node, fem%mesh%ele,                &
     &    fem%group%ele_grp, fem%group%surf_grp, fld_nod)
!
      end subroutine field_line_init_type
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_type                                   &
     &         (istep_fline, fem, surf, next_tbl, fld_nod)
!
      use t_mesh_data
      use t_surface_data
      use t_next_node_ele_4_node
      use t_phys_data
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_fline
      type(mesh_data), intent(in) :: fem
      type(surface_data), intent(in) :: surf
      type(next_nod_ele_table), intent(in) :: next_tbl
      type(phys_data), intent(in) :: fld_nod
!
!
      call FLINE_visualize(istep_fline,                                 &
     &    fem%mesh%node, fem%mesh%ele, surf, fem%group%ele_grp,         &
     &    next_tbl%neib_ele, fld_nod, fem%mesh%nod_comm)
!
      end subroutine field_line_main_type
!
!  ---------------------------------------------------------------------
!
      end module fieldline_type
