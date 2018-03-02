!viewer_mesh_data_IO.f90
!      module viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!      subroutine write_domain_data_viewer
!      subroutine read_domain_data_viewer
!
!      subroutine write_node_data_viewer
!      subroutine read_node_data_viewer
!
!      subroutine write_surf_connect_viewer(nnod_4_surf)
!!      subroutine read_surf_connect_viewer                             &
!!     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!!
!!      subroutine write_edge_connect_viewer(nnod_4_edge)
!!      subroutine read_edge_connect_viewer(nnod_4_edge)
!
      module viewer_mesh_data_IO
!
      use m_precision
!
      use m_surface_mesh_4_merge
!
      implicit none
!
      character (len = 255) :: tmp_character
      private :: tmp_character
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine write_domain_data_viewer
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! number of domain '
      write(surface_id,'(a)') '!   stack of node for domain '
      write(surface_id,'(a)') '!   stack of surface for domain '
      write(surface_id,'(a)') '!   stack of edge for domain '
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') num_pe_sf
      write(surface_id,'(8i16)') inod_sf_stack(1:num_pe_sf)
      write(surface_id,'(8i16)') isurf_sf_stack(1:num_pe_sf)
      write(surface_id,'(8i16)') iedge_sf_stack(1:num_pe_sf)
!
      end subroutine write_domain_data_viewer
!
!------------------------------------------------------------------
!
      subroutine read_domain_data_viewer
!
      use skip_comment_f
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) num_pe_sf
!
      call allocate_num_mesh_sf
!
      read(surface_id,*) inod_sf_stack(1:num_pe_sf)
      read(surface_id,*) isurf_sf_stack(1:num_pe_sf)
      read(surface_id,*) iedge_sf_stack(1:num_pe_sf)
!
      view_mesh%nodpetot_viewer =  inod_sf_stack(num_pe_sf)
      surfpetot_viewer = isurf_sf_stack(num_pe_sf)
      edgepetot_viewer = iedge_sf_stack(num_pe_sf)
!
      end subroutine read_domain_data_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer
!
      integer(kind = kint) :: i
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 1. node information'
      write(surface_id,'(a)') '! number_of node, intenal_node'
      write(surface_id,'(a)') '! Global ID, x, y, z'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') view_mesh%nodpetot_viewer
!
      do i = 1, view_mesh%nodpetot_viewer
        write(surface_id,1002) i, xx_view(i,1:3)
      end do
 1002 format(i16, 1p3e23.12)
!
      end subroutine write_node_data_viewer
!
!------------------------------------------------------------------
!
      subroutine read_node_data_viewer
!
      use skip_comment_f
!
      integer(kind = kint) :: i, itmp
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
!
      call allocate_nod_position_viewer
!
      do i = 1, view_mesh%nodpetot_viewer
       read(surface_id,*) itmp, xx_view(i,1:3)
      end do
!
      end subroutine read_node_data_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer(nnod_4_surf)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint) :: i
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '! 2. element information'
      write(surface_id,'(a)') '! element type'
      write(surface_id,'(a)') '! Global ID, connectivity'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') surfpetot_viewer
      write(surface_id,1003) surftyp_viewer(1:surfpetot_viewer)
!
      do i = 1, surfpetot_viewer
       write(surface_id,'(10i16)')  i, ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
 1003 format(10i16)
!
      end subroutine write_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_connect_viewer                               &
     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
      use skip_comment_f
      use set_nnod_4_ele_by_type
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
      integer(kind = kint) :: i, itmp
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
!
      call allocate_surf_type_viewer
!
      read(surface_id,*) surftyp_viewer(1:surfpetot_viewer)
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
      call allocate_quad4_2_linear(nnod_4_ele)
!
      call allocate_surf_connect_viewer(nnod_4_surf)
!
      do i = 1, surfpetot_viewer
       read(surface_id,*) itmp, ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
      end subroutine read_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer(nnod_4_edge)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint) :: i
!
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '!  edge information'
      write(surface_id,'(a)') '!  edge type'
      write(surface_id,'(a)') '!  global ID, connectivity'
      write(surface_id,'(a)') '!'
!
      write(surface_id,'(i16)') edgepetot_viewer
!
      do i = 1, edgepetot_viewer
       write(surface_id,'(10i16)')                                      &
     &               i, ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      write(surface_id,'(a)') '!'
      write(surface_id,'(a)') '!  edge ID for surfaces'
      write(surface_id,'(a)') '!'
      write(surface_id,'(i16)') surfpetot_viewer
      do i = 1, surfpetot_viewer
        write(surface_id,'(10i16)') i, iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine write_edge_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine read_edge_connect_viewer(nnod_4_edge)
!
      use m_node_quad_2_linear_sf
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      integer(kind = kint) :: i, itmp
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
!
      call allocate_edge_data_4_sf(nnod_4_edge)
!
      do i = 1, edgepetot_viewer
       read(surface_id,*) itmp, ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
!
      do i = 1, surfpetot_viewer
       read(surface_id,*) itmp, iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine read_edge_connect_viewer
!
!------------------------------------------------------------------
!
      end module viewer_mesh_data_IO
