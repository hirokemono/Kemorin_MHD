!fieldline_only.f90
!      module fieldline_only
!
!      Written by H. Matsui on Apr., 2012
!
!      subroutine field_line_init_only(mesh, group, nod_fld)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(phys_data), intent(in) :: nod_fld
!!
!!      subroutine field_line_main_only                                 &
!!     &         (istep_fline, mesh, group, ele_mesh, next_tbl, nod_fld)
!!        integer(kind = kint), intent(in) :: istep_fline
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) ::   group
!!        type(element_geometry), intent(in) :: ele_mesh
!!        type(element_around_node), intent(in) :: ele_4_nod
!!        type(phys_data), intent(in) :: nod_fld
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
      subroutine field_line_init_only(mesh, group, nod_fld)
!
      use fieldline
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(phys_data), intent(in) :: nod_fld
!
!
      call FLINE_initialize(mesh, group, nod_fld)
!
      end subroutine field_line_init_only
!
!  ---------------------------------------------------------------------
!
      subroutine field_line_main_only                                   &
     &         (istep_fline, mesh, group, ele_mesh, ele_4_nod, nod_fld)
!
      use fieldline
!
      integer(kind = kint), intent(in) :: istep_fline
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) ::   group
      type(element_geometry), intent(in) :: ele_mesh
      type(element_around_node), intent(in) :: ele_4_nod
      type(phys_data), intent(in) :: nod_fld
!
!
      call FLINE_visualize(istep_fline, mesh, group, ele_mesh,          &
     &    ele_4_nod, nod_fld)
!
      end subroutine field_line_main_only
!
!  ---------------------------------------------------------------------
!
      end module fieldline_only
