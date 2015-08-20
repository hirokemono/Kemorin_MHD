!const_linear_mesh_t_by_type.f90
!      module const_linear_mesh_t_by_type
!
!      Written by H. Matsui on Apr., 2006
!
!      subroutine const_linear_mesh_type_by_t                           &
!     &     (femmesh_q, surf_mesh_q, edge_mesh_q, nod_fld_q,            &
!     &      femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!      subroutine set_linear_type_phys_from_t(femmesh_q, surf_mesh_q,   &
!     &          nod_fld_q, femmesh_l, nod_fld_l)
!
      module const_linear_mesh_t_by_type
!
      use m_precision
!
      implicit none
!
      private :: link_data_4_linear_grid
      private :: set_linear_data_by_quad_data
      private :: set_linear_data_by_lag_data
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_linear_mesh_type_by_t                            &
     &     (femmesh_q, surf_mesh_q, edge_mesh_q, nod_fld_q,             &
     &      femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use t_mesh_data
      use t_phys_data
      use m_geometry_constants
!
      type(mesh_data), intent(in) :: femmesh_q
      type(surface_geometry), intent(in) :: surf_mesh_q
      type(edge_geometry),    intent(in) :: edge_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      femmesh_l%mesh%ele%nnod_4_ele = num_t_linear
      surf_mesh_l%surf%nnod_4_surf =  num_linear_sf
      edge_mesh_l%edge%nnod_4_edge =  num_linear_edge
!
      if      (femmesh_q%mesh%ele%nnod_4_ele .eq. num_t_linear) then
        call link_data_4_linear_grid                                    &
     &     (femmesh_q, surf_mesh_q, edge_mesh_q, nod_fld_q,             &
     &      femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
      else if (femmesh_q%mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call set_linear_data_by_quad_data(femmesh_q, surf_mesh_q,       &
     &      nod_fld_q, femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
      else if (femmesh_q%mesh%ele%nnod_4_ele .eq. num_t_lag) then
        call set_linear_data_by_lag_data(femmesh_q, surf_mesh_q,        &
     &      nod_fld_q, femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
      end if 
!
      end subroutine const_linear_mesh_type_by_t
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_type_phys_from_t(femmesh_q, surf_mesh_q,    &
     &          nod_fld_q, femmesh_l, nod_fld_l)
!
      use m_geometry_constants
      use t_mesh_data
      use t_phys_data
      use cvt_quad_t_2_linear_t
!
      type(mesh_data), intent(in) :: femmesh_q
      type(surface_geometry), intent(in) :: surf_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
      type(mesh_data), intent(in) :: femmesh_l
!
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      if (femmesh_q%mesh%ele%nnod_4_ele .eq. num_t_quad) then
        call copy_nod_phys_t_2_linear_t(femmesh_q%mesh, nod_fld_q,      &
     &      femmesh_l%mesh, nod_fld_l)
        call generate_phys_t_on_sf_t(femmesh_q%mesh, surf_mesh_q%surf,  &
     &      femmesh_l%mesh, nod_fld_l)
      end if
!
      end subroutine set_linear_type_phys_from_t
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine link_data_4_linear_grid                                &
     &         (femmesh_q, surf_mesh_q, edge_mesh_q, nod_fld_q,         &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use t_mesh_data
      use t_phys_data
!
      type(mesh_data), intent(in) :: femmesh_q
      type(surface_geometry), intent(in) :: surf_mesh_q
      type(edge_geometry),    intent(in) :: edge_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call link_new_nod_geometry_type(femmesh_q%mesh%node,              &
     &    femmesh_l%mesh%node)
      call link_new_ele_connect_type(femmesh_q%mesh%ele,                &
     &    femmesh_l%mesh%ele)
!
      call link_groups_type(femmesh_q%group, femmesh_l%group)
!
      call link_new_surf_connect_type(surf_mesh_q%surf,                 &
     &    surf_mesh_l%surf)
      call link_new_edge_connect_type(edge_mesh_q%edge,                 &
     &    edge_mesh_l%edge)
!
      call link_ele_grp_connect_type(femmesh_q%group%tbls_ele_grp,      &
     &    femmesh_l%group%tbls_ele_grp)
      call link_surf_grp_connect_type(femmesh_q%group%tbls_surf_grp,    &
     &    femmesh_l%group%tbls_surf_grp)
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine link_data_4_linear_grid
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_quad_data                           &
     &         (femmesh_q, surf_mesh_q, nod_fld_q,                      &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use m_machine_parameter
      use t_mesh_data
      use t_phys_data
!
      use const_group_type_info
      use cvt_quad_t_2_linear_t
      use set_size_4_smp_types
!
      type(mesh_data), intent(in) :: femmesh_q
      type(surface_geometry), intent(in) :: surf_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_type_info(surf_mesh_l%surf,                &
     &    edge_mesh_l%edge)
!
      call generate_linear_nod_by_quad_t(femmesh_q%mesh,                &
     &    surf_mesh_q%surf, femmesh_l%mesh)
!
      if (iflag_debug.eq.1)                                             &
     &      write(*,*) 'connect_quad_type_2_linear_t'
      call connect_quad_type_2_linear_t(femmesh_q%mesh,                 &
     &    surf_mesh_q%surf, femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_grp_type_by_t'
      call gen_linear_grp_type_by_t(femmesh_q%group, femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call construct_edge_data(femmesh_l%mesh%node,                     &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_group_type_info'
      call s_const_group_type_info(femmesh_l%mesh, surf_mesh_l,         &
     &    edge_mesh_l, femmesh_l%group)
!
      call count_size_4_smp_mesh_type(femmesh_l%mesh%node,              &
     &    femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_4_ltype_20t(femmesh_q%mesh,                &
     &     surf_mesh_q%surf, femmesh_l%mesh, surf_mesh_l%surf,          &
     &     edge_mesh_l%edge)
!
      call init_linear_phys_type(femmesh_l%mesh, nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_quad_data
!
!  ---------------------------------------------------------------------
!
      subroutine set_linear_data_by_lag_data                            &
     &         (femmesh_q, surf_mesh_q, nod_fld_q,                      &
     &          femmesh_l, surf_mesh_l, edge_mesh_l, nod_fld_l)
!
      use m_machine_parameter
      use t_mesh_data
      use t_phys_data
!
      use cvt_quad_t_2_linear_t
      use const_surface_data
      use const_edge_data
      use const_group_type_info
      use set_size_4_smp_types
!
      type(mesh_data), intent(in) :: femmesh_q
      type(surface_geometry), intent(in) :: surf_mesh_q
      type(phys_data), intent(in) :: nod_fld_q
!
      type(mesh_data), intent(inout) :: femmesh_l
      type(surface_geometry), intent(inout) :: surf_mesh_l
      type(edge_geometry),    intent(inout) :: edge_mesh_l
      type(phys_data), intent(inout) :: nod_fld_l
!
!
      call set_local_element_type_info(surf_mesh_l%surf,                &
     &      edge_mesh_l%edge)
!
      call link_new_nod_geometry_type(femmesh_q%mesh%node,              &
     &    femmesh_l%mesh%node)
!
      if (iflag_debug.eq.1) write(*,*) 'connect_lag_type_2_linear_t'
      call connect_lag_type_2_linear_t(femmesh_q%mesh, femmesh_l%mesh)
!
      if (iflag_debug.eq.1) write(*,*) 'gen_linear_grp_type_by_t'
      call gen_linear_grp_type_by_t(femmesh_q%group, femmesh_l%group)
!
      if (iflag_debug.eq.1) write(*,*) 'construct_surface_data'
      call construct_surface_data(femmesh_l%mesh%node,                  &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf)
      call construct_edge_data(femmesh_l%mesh%node,                     &
     &    femmesh_l%mesh%ele, surf_mesh_l%surf, edge_mesh_l%edge)
!
      if (iflag_debug.eq.1) write(*,*) 's_const_group_type_info'
      call s_const_group_type_info(femmesh_l%mesh, surf_mesh_l,         &
     &    edge_mesh_l, femmesh_l%group)
!
      call count_size_4_smp_mesh_type(femmesh_l%mesh%node,              &
     &    femmesh_l%mesh%ele)
      call count_surf_size_smp_type(surf_mesh_l%surf)
      call count_edge_size_smp_type(edge_mesh_l%edge)
!
      call set_internal_list_4_ltype_27t(femmesh_q%mesh,                &
     &     surf_mesh_q%surf, femmesh_l%mesh, surf_mesh_l%surf,          &
     &     edge_mesh_l%edge)
!
      call link_field_data_type(nod_fld_q, nod_fld_l)
!
      end subroutine set_linear_data_by_lag_data
!
! ----------------------------------------------------------------------
!
      end module const_linear_mesh_t_by_type
