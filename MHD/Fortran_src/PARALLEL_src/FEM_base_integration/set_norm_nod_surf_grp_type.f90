!set_norm_nod_surf_grp_type.f90
!      module set_norm_nod_surf_grp_type
!
!        programmed by H.Matsui
!     Modified by H. Matsui on Sep, 2007
!     Modified by H. Matsui on Jan., 2009
!
!      subroutine cal_surf_norm_nod_type(mesh, surf, sf_grp,            &
!     &          sf_grp_v, sf_nod)
!        type(mesh_geometry),          intent(in) :: mesh
!        type(surface_data),           intent(in) :: surf
!        type(surface_group_data),     intent(in) :: sf_grp
!        type(surface_group_geometry), intent(in) :: sf_grp_v
!        type(surface_node_grp_data), intent(inout) :: sf_nod
!
      module set_norm_nod_surf_grp_type
!
      use m_precision
!
      use set_norm_nod_4_surf_grp
!
      implicit none
!
      integer(kind = kint), allocatable, private :: imark_nod(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_surf_norm_nod_type(mesh, surf, sf_grp,             &
     &          sf_grp_v, sf_nod)
!
      use t_mesh_data
      use t_surface_data
      use t_group_data
      use t_surface_group_geometry
      use t_surface_group_connect
!
      type(mesh_geometry),          intent(in) :: mesh
      type(surface_data),           intent(in) :: surf
      type(surface_group_data),     intent(in) :: sf_grp
      type(surface_group_geometry), intent(in) :: sf_grp_v
      type(surface_node_grp_data), intent(inout) :: sf_nod
!
!
      call cal_surf_grp_norm_node(mesh%ele%numele, mesh%ele%nnod_4_ele, &
     &    surf%nnod_4_surf, surf%node_on_sf, mesh%ele%ie,               &
     &    sf_grp%num_grp, sf_grp%num_item, sf_grp%istack_grp,           &
     &    sf_grp%item_sf_grp, sf_grp_v%vnorm_sf_grp,                    &
     &    sf_grp_v%a_area_sf_grp, sf_nod%ntot_node_sf_grp,              &
     &    sf_nod%inod_stack_sf_grp, sf_nod%inod_surf_grp,               &
     &    sf_nod%surf_norm_nod, sf_nod%coef_sf_nod)
!
      end subroutine cal_surf_norm_nod_type
!
! -----------------------------------------------------------------------
!
      end module set_norm_nod_surf_grp_type
