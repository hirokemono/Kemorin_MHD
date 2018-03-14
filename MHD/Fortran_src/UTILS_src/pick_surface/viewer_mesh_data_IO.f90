!viewer_mesh_data_IO.f90
!      module viewer_mesh_data_IO
!
!      Written by Kemorin on Jan., 2007
!
!!      subroutine write_domain_data_viewer(mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_domain_data_viewer(mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!
!!      subroutine write_node_data_viewer(mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_node_data_viewer(mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!
!!      subroutine write_surf_connect_viewer(nnod_4_surf, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_surf_connect_viewer                             &
!!     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!
!!      subroutine write_edge_connect_viewer(nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine read_edge_connect_viewer(nnod_4_edge, mgd_view_mesh)
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      module viewer_mesh_data_IO
!
      use m_precision
      use t_viewer_mesh
      use t_merged_viewer_mesh
      use m_viewer_mesh_labels
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
      subroutine write_domain_data_viewer(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
!
      write(surface_id,'(a)', advance='NO') hd_ndomain_viewer()
!
      write(surface_id,'(i16)') mgd_view_mesh%num_pe_sf
!
      end subroutine write_domain_data_viewer
!
!------------------------------------------------------------------
!
      subroutine read_domain_data_viewer(mgd_view_mesh)
!
      use skip_comment_f
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: num_pe
!
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) num_pe
!
      call alloc_num_mesh_sf(num_pe, mgd_view_mesh)
!
      end subroutine read_domain_data_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_node_data_viewer(mgd_view_mesh)
!
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      integer(kind = kint) :: i
!
!
      write(surface_id,'(a)', advance='NO') hd_node_viewer()
!
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nnod_sf(1:mgd_view_mesh%num_pe_sf)
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nnod_sf(1:mgd_view_mesh%num_pe_sf)
!
      do i = 1, mgd_view_mesh%view_mesh%nodpetot_viewer
        write(surface_id,1002)                                          &
     &            i, mgd_view_mesh%view_mesh%xx_view(i,1:3)
      end do
 1002 format(i16, 1p3e26.15e3)
!
      end subroutine write_node_data_viewer
!
!------------------------------------------------------------------
!
      subroutine read_node_data_viewer(mgd_view_mesh)
!
      use skip_comment_f
      use cal_minmax_and_stacks
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
!
      integer(kind = kint) :: i, itmp
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*)                                             &
     &          mgd_view_mesh%nnod_sf(1:mgd_view_mesh%num_pe_sf)
      read(tmp_character,*) (itmp,i=1,mgd_view_mesh%num_pe_sf)
!
      call s_cal_total_and_stacks(mgd_view_mesh%num_pe_sf,              &
     &    mgd_view_mesh%nnod_sf, izero, mgd_view_mesh%inod_sf_stack,    &
     &    mgd_view_mesh%view_mesh%nodpetot_viewer)
!
      call alloc_nod_position_viewer(mgd_view_mesh%view_mesh)
!
      do i = 1, mgd_view_mesh%view_mesh%nodpetot_viewer
       read(surface_id,*) itmp, mgd_view_mesh%view_mesh%xx_view(i,1:3)
      end do
!
      end subroutine read_node_data_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_surf_connect_viewer(nnod_4_surf, mgd_view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      integer(kind = kint) :: i, ist, ied
!
!
      write(surface_id,'(a)', advance='NO') hd_surf_viewer()
!
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nsurf_sf(1:mgd_view_mesh%num_pe_sf)
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nsurf_sf(1:mgd_view_mesh%num_pe_sf)
!
      do i = 1, mgd_view_mesh%num_pe_sf
        ist = mgd_view_mesh%isurf_sf_stack(i-1) + 1
        ied = mgd_view_mesh%isurf_sf_stack(i)
        write(surface_id,1003)                                          &
     &             mgd_view_mesh%view_mesh%surftyp_viewer(ist:ied)
      end do
!
      do i = 1, mgd_view_mesh%view_mesh%surfpetot_viewer
       write(surface_id,'(10i16)')                                      &
     &        i, mgd_view_mesh%view_mesh%ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
 1003 format(10i16)
!
      end subroutine write_surf_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine read_surf_connect_viewer                               &
     &         (nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      use m_geometry_constants
      use m_node_quad_2_linear_sf
      use skip_comment_f
      use set_nnod_4_ele_by_type
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: i, itmp, ist, ied
!
!
      call skip_comment(tmp_character, surface_id)
      read(surface_id,*)                                                &
     &   mgd_view_mesh%nsurf_sf(1:mgd_view_mesh%num_pe_sf)
      read(surface_id,*)                                                &
     &   mgd_view_mesh%isurf_sf_stack(1:mgd_view_mesh%num_pe_sf)
!
      call s_cal_total_and_stacks(mgd_view_mesh%num_pe_sf,              &
     &    mgd_view_mesh%nsurf_sf, izero, mgd_view_mesh%isurf_sf_stack,  &
     &    mgd_view_mesh%view_mesh%surfpetot_viewer)
!
      call alloc_surf_type_viewer(mgd_view_mesh%view_mesh)
!
      do i = 1, mgd_view_mesh%num_pe_sf
        ist = mgd_view_mesh%isurf_sf_stack(i-1) + 1
        ied = mgd_view_mesh%isurf_sf_stack(i)
        read(surface_id,*)                                              &
     &      mgd_view_mesh%view_mesh%surftyp_viewer(ist:ied)
      end do
!
      call set_3D_nnod_4_sfed_by_ele                                    &
     &   (nnod_4_ele, nnod_4_surf, nnod_4_edge)
      call allocate_quad4_2_linear(nnod_4_ele)
!
      call alloc_surf_connect_viewer                                    &
     &   (nnod_4_surf, mgd_view_mesh%view_mesh)
!
      do i = 1, mgd_view_mesh%view_mesh%surfpetot_viewer
       read(surface_id,*) itmp,                                         &
     &        mgd_view_mesh%view_mesh%ie_sf_viewer(i,1:nnod_4_surf)
      end do
!
      end subroutine read_surf_connect_viewer
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine write_edge_connect_viewer(nnod_4_edge, mgd_view_mesh)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      integer(kind = kint) :: i
!
!
      write(surface_id,'(a)', advance='NO') hd_edge_viewer()
!
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nedge_sf(1:mgd_view_mesh%num_pe_sf)
      write(surface_id,'(8i16)')                                        &
     &             mgd_view_mesh%nedge_sf(1:mgd_view_mesh%num_pe_sf)
!
!
      do i = 1, mgd_view_mesh%view_mesh%edgepetot_viewer
       write(surface_id,'(10i16)')                                      &
     &     i, mgd_view_mesh%view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      write(surface_id,'(a)', advance='NO') hd_edge_on_sf_viewer()
      write(surface_id,'(i16)')                                         &
     &         mgd_view_mesh%view_mesh%surfpetot_viewer
      do i = 1, mgd_view_mesh%view_mesh%surfpetot_viewer
        write(surface_id,'(10i16)')                                     &
     &    i, mgd_view_mesh%view_mesh%iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine write_edge_connect_viewer
!
!------------------------------------------------------------------
!
      subroutine read_edge_connect_viewer(nnod_4_edge, mgd_view_mesh)
!
      use m_node_quad_2_linear_sf
      use skip_comment_f
      use cal_minmax_and_stacks
!
      integer(kind = kint), intent(in) :: nnod_4_edge
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      integer(kind = kint) :: i, itmp
!
      call skip_comment(tmp_character, surface_id)
      read(surface_id,*)                                                &
     &   mgd_view_mesh%nedge_sf(1:mgd_view_mesh%num_pe_sf)
      read(surface_id,*)                                                &
     &   mgd_view_mesh%iedge_sf_stack(1:mgd_view_mesh%num_pe_sf)
!
      call s_cal_total_and_stacks(mgd_view_mesh%num_pe_sf,              &
     &    mgd_view_mesh%nedge_sf, izero, mgd_view_mesh%iedge_sf_stack,  &
     &    mgd_view_mesh%view_mesh%edgepetot_viewer)
!
      call alloc_edge_data_4_sf(nnod_4_edge, mgd_view_mesh%view_mesh)
!
      do i = 1, mgd_view_mesh%view_mesh%edgepetot_viewer
       read(surface_id,*)                                               &
     &   itmp, mgd_view_mesh%view_mesh%ie_edge_viewer(i,1:nnod_4_edge)
      end do
!
      call skip_comment(tmp_character, surface_id)
      read(tmp_character,*) itmp
!
      do i = 1, mgd_view_mesh%view_mesh%surfpetot_viewer
       read(surface_id,*) itmp,                                         &
     &  mgd_view_mesh%view_mesh%iedge_sf_viewer(i,1:nedge_4_surf)
      end do
!
      end subroutine read_edge_connect_viewer
!
!------------------------------------------------------------------
!
      end module viewer_mesh_data_IO
