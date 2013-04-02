!set_surf_edge_mesh_types.f90
!      module set_surf_edge_mesh_types
!
!     Written by H. Matsui on Dec., 2008
!
!      subroutine set_surf_connect_type(mesh, surf_mesh)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!      subroutine set_edge_connect_type(mesh, surf_mesh, edge_mesh)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(in) :: surf_mesh
!        type(edge_geometry), intent(inout) :: edge_mesh
!
!      subroutine set_surf_geometry_type(mesh, surf_mesh)
!        type(mesh_geometry), intent(in) :: mesh
!        type(surface_geometry), intent(inout) :: surf_mesh
!      subroutine set_edge_geometry_type(mesh, edge_mesh)
!        type(mesh_geometry), intent(in) :: mesh
!        type(edge_geometry), intent(inout) :: edge_mesh
!
      module set_surf_edge_mesh_types
!
      use m_precision
!
      use t_mesh_data
!
      implicit  none
!
! ----------------------------------------------------------------------
!
     contains
!
! ----------------------------------------------------------------------
!
      subroutine set_surf_connect_type(mesh, surf_mesh)
!
      use m_machine_parameter
      use const_surface_type_data
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(inout) :: surf_mesh
      logical :: read_surface
!
!
      read_surface = associated(surf_mesh%surf%isurf_global)
!
      if(read_surface .eqv. .false.) then
        if (iflag_debug.eq.1) write(*,*) 's_const_surface_type_data'
        call s_const_surface_type_data(mesh%node,                       &
     &      mesh%ele, surf_mesh%surf)
!
        if (iflag_debug.eq.1) write(*,*) 'count_overlap_surface'
        call count_surf_size_smp_type(surf_mesh%surf)
        call count_overlap_surf_type(mesh%node, surf_mesh%surf)
!
        if (iflag_debug.eq.1) write(*,*) 'set_gl_surf_type_by_local'
        call set_gl_surf_type_by_local(surf_mesh%surf)
      else
!
        if (iflag_debug.eq.1) write(*,*) 'count_overlap_surface'
        call count_surf_size_smp_type(surf_mesh%surf)
        call count_overlap_surf_type(mesh%node, surf_mesh%surf)
!
      end if
!
      end subroutine set_surf_connect_type
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_connect_type(mesh, surf_mesh, edge_mesh)
!
      use m_machine_parameter
      use const_edge_type_data
      use set_size_4_smp_types
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(in) :: surf_mesh
      type(edge_geometry), intent(inout) :: edge_mesh
      logical :: read_edge
!
!
      read_edge =    associated(edge_mesh%edge%iedge_global)
!
      if(read_edge .eqv. .false.) then
        if (iflag_debug.eq.1) write(*,*) 's_const_edge_type_data'
        call s_const_edge_type_data(mesh%node, mesh%ele,                &
     &      surf_mesh%surf, edge_mesh%edge)
!
        if (iflag_debug.eq.1) write(*,*) 'count_overlap_edge'
        call count_edge_size_smp_type(edge_mesh%edge)
        call count_overlap_edge_type(mesh%node, edge_mesh%edge)
!
!
        if (iflag_debug.eq.1) write(*,*) 'set_gl_edge_type_by_local'
        call set_gl_edge_type_by_local(edge_mesh%edge)
      else
!
        if (iflag_debug.eq.1) write(*,*) 'count_overlap_edge'
        call count_edge_size_smp_type(edge_mesh%edge)
        call count_overlap_edge_type(mesh%node, edge_mesh%edge)
!
      end if
!
      end subroutine set_edge_connect_type
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_surf_geometry_type(mesh, surf_mesh)
!
      use cal_mesh_position_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(surface_geometry), intent(inout) :: surf_mesh
!
!
      call allocate_surface_geom_type(surf_mesh%surf)
      call set_center_of_surf_type(mesh%node, surf_mesh%surf)
!
      end subroutine set_surf_geometry_type
!
! ----------------------------------------------------------------------
!
      subroutine set_edge_geometry_type(mesh, edge_mesh)
!
      use cal_mesh_position_type
!
      type(mesh_geometry), intent(in) :: mesh
      type(edge_geometry), intent(inout) :: edge_mesh
!
!
      call allocate_edge_geom_type(edge_mesh%edge)
      call set_center_of_edge_type(mesh%node, edge_mesh%edge)
!
      end subroutine set_edge_geometry_type
!
! ----------------------------------------------------------------------
!
      end module set_surf_edge_mesh_types
