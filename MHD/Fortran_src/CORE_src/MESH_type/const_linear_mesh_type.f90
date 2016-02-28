!const_linear_mesh_type.f90
!      module const_linear_mesh_type
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine const_linear_mesh_type_by_t                          &
!!     &         (femmesh_q, ele_mesh_q, nod_fld_q,                     &
!!     &          femmesh_l, ele_mesh_l, nod_fld_l)
!!      subroutine set_linear_type_phys_from_t(femmesh_q, ele_mesh_q,   &
!!     &          nod_fld_q, femmesh_l, nod_fld_l)
!!        type(mesh_data), intent(in) :: femmesh_q
!!        type(element_geometry), intent(inout) :: ele_mesh_q
!!        type(phys_data), intent(in) :: nod_fld_q
!!        type(mesh_data), intent(inout) :: femmesh_l
!!        type(element_geometry), intent(inout) :: ele_mesh_l
!!        type(phys_data), intent(inout) :: nod_fld_l
!!
!!      subroutine const_linear_mesh_by_q(node_q, ele_q, surf_q, edge_q,&
!!     &    nod_grp_q, ele_grp_q, sf_grp_q, ele_grp_tbl_q, sf_grp_tbl_q,&
!!     &    nod_fld_q, femmesh_l, ele_mesh_l, nod_fld_l)
!!      subroutine set_linear_phys_data_type(node_q, ele_q, surf_q,     &
!!     &         nod_fld_q, femmesh_l, nod_fld_l)
!!        type(node_data), intent(in) ::    node_q
!!        type(element_data), intent(in) :: ele_q
!!        type(surface_data), intent(in) :: surf_q
!!        type(edge_data), intent(in) ::    edge_q
!!
!!        type(group_data), intent(in) :: nod_grp_q
!!        type(group_data), intent(in) :: ele_grp_q
!!        type(surface_group_data), intent(in) :: sf_grp_q
!!        type(element_group_table), intent(in) :: ele_grp_tbl_q
!!        type(surface_group_table), intent(in) :: sf_grp_tbl_q
!!
!!        type(phys_data), intent(in) ::     nod_fld_q
!!
!!        type(mesh_data), intent(inout) :: femmesh_l
!!        type(element_geometry), intent(inout) :: ele_mesh_l
!!        type(phys_data), intent(inout) :: nod_fld_l
!
      module const_linear_mesh_type
!
      use m_precision
      use m_geometry_constants
!
      use t_mesh_data
      use t_phys_data
!
      implicit none
!
      private :: const_linear_mesh_by_q
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_linear_mesh_type_by_t                            &
     &         (femmesh_q, ele_mesh_q, nod_fld_q,                       &
     &          femmesh_l, ele_mesh_l, nod_fld_l)
!
      type(mesh_data), intent(in) :: femmesh_q
      type(element_geometry), intent(inout) :: ele_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(element_geometry), intent(inout) :: ele_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call const_linear_mesh_by_q(femmesh_q%mesh%node,                  &
     &    femmesh_q%mesh%ele, ele_mesh_q%surf, ele_mesh_q%edge,         &
     &    femmesh_q%group%nod_grp, femmesh_q%group%ele_grp,             &
     &    femmesh_q%group%surf_grp, femmesh_q%group%tbls_ele_grp,       &
     &    femmesh_q%group%tbls_surf_grp, nod_fld_q,                     &
     &    femmesh_l, ele_mesh_l, nod_fld_l)
!
      end subroutine const_linear_mesh_type_by_t
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_type_phys_from_t(femmesh_q, ele_mesh_q,     &
     &          nod_fld_q, femmesh_l, nod_fld_l)
!
      use cvt_quad_2_linear_mesh
!
      type(mesh_data), intent(in) :: femmesh_q
      type(element_geometry), intent(in) :: ele_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
      type(mesh_data), intent(in) :: femmesh_l
!
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_linear_phys_data_type                                    &
     &   (femmesh_q%mesh%node, femmesh_q%mesh%ele, ele_mesh_q%surf,     &
     &     nod_fld_q, femmesh_l, nod_fld_l)
!
      end subroutine set_linear_type_phys_from_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_linear_mesh_by_q(node_q, ele_q, surf_q, edge_q,  &
     &     nod_grp_q, ele_grp_q, sf_grp_q, ele_grp_tbl_q, sf_grp_tbl_q, &
     &     nod_fld_q, femmesh_l, ele_mesh_l, nod_fld_l)
!
      use t_geometry_data
      use t_surface_data
      use t_edge_data
      use t_group_data
      use const_linear_mesh_by_quad
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
      type(edge_data), intent(in) ::    edge_q
!
      type(group_data), intent(in) :: nod_grp_q
      type(group_data), intent(in) :: ele_grp_q
      type(surface_group_data), intent(in) :: sf_grp_q
      type(element_group_table), intent(in) :: ele_grp_tbl_q
      type(surface_group_table), intent(in) :: sf_grp_tbl_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(element_geometry), intent(inout) :: ele_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      femmesh_l%mesh%ele%nnod_4_ele = num_t_linear
      ele_mesh_l%surf%nnod_4_surf =  num_linear_sf
      ele_mesh_l%edge%nnod_4_edge =  num_linear_edge
!
      if      (ele_q%nnod_4_ele .eq. num_t_linear) then
        call link_data_4_linear_grid(node_q, ele_q, surf_q, edge_q,     &
     &     nod_grp_q, ele_grp_q, sf_grp_q, ele_grp_tbl_q, sf_grp_tbl_q, &
     &     nod_fld_q, femmesh_l, ele_mesh_l, nod_fld_l)
      else if (ele_q%nnod_4_ele .eq. num_t_quad) then
        call set_linear_data_by_quad_data(node_q, ele_q, surf_q,        &
     &      nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,                  &
     &      femmesh_l, ele_mesh_l, nod_fld_l)
      else if (ele_q%nnod_4_ele .eq. num_t_lag) then
        call set_linear_data_by_lag_data(node_q, ele_q,                 &
     &      nod_grp_q, ele_grp_q, sf_grp_q, nod_fld_q,                  &
     &      femmesh_l, ele_mesh_l, nod_fld_l)
      end if 
!
      end subroutine const_linear_mesh_by_q
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_phys_data_type(node_q, ele_q, surf_q,       &
     &          nod_fld_q, femmesh_l, nod_fld_l)
!
      use t_geometry_data
      use t_surface_data
!
      use cvt_quad_2_linear_mesh
!
      type(node_data), intent(in) ::    node_q
      type(element_data), intent(in) :: ele_q
      type(surface_data), intent(in) :: surf_q
!
      type(phys_data), intent(in) ::     nod_fld_q
!
      type(mesh_data), intent(in) :: femmesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      if (ele_q%nnod_4_ele .eq. num_t_quad) then
        call copy_nod_phys_2_linear                                     &
     &     (node_q, nod_fld_q, femmesh_l%mesh, nod_fld_l)
        call generate_phys_on_surf(node_q, ele_q, surf_q,               &
     &      femmesh_l%mesh, nod_fld_l)
      end if
!
      end subroutine set_linear_phys_data_type
!
!  ---------------------------------------------------------------------
!
      end module const_linear_mesh_type
