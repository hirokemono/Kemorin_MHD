!>@file   viewer_mesh_IO_select.F90
!!@brief  module viewer_mesh_IO_select
!!
!!@author  H. Matsui
!!@date Programmed in Dec., 2006
!
!>@brief  Viewer mesh file IO selector
!!
!!@verbatim
!!      subroutine sel_output_surface_grid                              &
!!     &         (mesh_file, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!!      subroutine sel_read_surface_grid(mesh_file,                     &
!!     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!!
!!      subroutine sel_output_single_surface_grid(id_rank, mesh_file,  &
!!     &          nnod_4_surf, nnod_4_edge, view_mesh, domain_grps,    &
!!     &          view_nod_grps, view_ele_grps, view_sf_grps)
!!        type(field_IO_params), intent(in) :: mesh_file
!!        type(viewer_mesh_data), intent(in) :: view_mesh
!!        type(viewer_surface_groups), intent(in) :: domain_grps
!!        type(viewer_node_groups), intent(in) :: view_nod_grps
!!        type(viewer_surface_groups), intent(in) :: view_ele_grps
!!        type(viewer_surface_groups), intent(in) :: view_sf_grps
!!
!!      subroutine sel_mpi_output_surface_grid                          &
!!     &         (mesh_file, nnod_4_surf, nnod_4_edge,                  &
!!     &          mgd_v_mesh_p, mgd_view_mesh)
!!@endverbatim
!
      module viewer_mesh_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_merged_viewer_mesh
      use t_file_IO_parameter
!
      use viewer_file_IO
      use MPI_viewer_mesh_file_IO
!
#ifdef ZLIB_IO
      use gz_viewer_file_IO
      use gz_MPI_viewer_mesh_file_IO
#endif
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine sel_output_surface_grid                                &
     &         (mesh_file, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nnod_4_edge
!
      type(field_IO_params), intent(in) :: mesh_file
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      character(len = kchara) :: file_name
!
!
      call add_ksm_extension                                            &
     &   (mesh_file%file_prefix, file_name)
!
#ifdef ZLIB_IO
      if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call output_surface_grid_gz                                     &
     &     (file_name, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
        return
      end if
#endif
!
      call output_surface_grid(file_name,                               &
     &    nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      end subroutine sel_output_surface_grid
!
!------------------------------------------------------------------
!
      subroutine sel_read_surface_grid(mesh_file,                       &
     &          nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      type(field_IO_params), intent(in) :: mesh_file
      integer(kind = kint), intent(in) :: nnod_4_ele
      integer(kind = kint), intent(inout) :: nnod_4_surf
      integer(kind = kint), intent(inout) :: nnod_4_edge
!
      type(merged_viewer_mesh), intent(inout) :: mgd_view_mesh
!
      character(len = kchara) :: file_name
!
!
      call add_ksm_extension                                            &
     &   (mesh_file%file_prefix, file_name)
!
#ifdef ZLIB_IO
      if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call read_surface_grid_gz(file_name,                            &
     &      nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
        return
      end if
#endif
!
      call read_surface_grid(file_name,                                 &
     &    nnod_4_ele, nnod_4_surf, nnod_4_edge, mgd_view_mesh)
!
      end subroutine sel_read_surface_grid
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_output_single_surface_grid(id_rank, mesh_file,    &
     &          nnod_4_surf, nnod_4_edge, view_mesh, domain_grps,      &
     &          view_nod_grps, view_ele_grps, view_sf_grps)
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nnod_4_edge
!
      type(field_IO_params), intent(in) :: mesh_file
      type(viewer_mesh_data), intent(in) :: view_mesh
      type(viewer_surface_groups), intent(in) :: domain_grps
      type(viewer_node_groups), intent(in) :: view_nod_grps
      type(viewer_surface_groups), intent(in) :: view_ele_grps
      type(viewer_surface_groups), intent(in) :: view_sf_grps
!
!
      integer(kind = kint) :: istack_nsurf(0:1)
      character(len = kchara) :: fname_tmp, file_name
!
!
      istack_nsurf(0) = 0
      istack_nsurf(1) = view_mesh%nsurf_viewer
!
      call add_int_suffix                                               &
     &  (id_rank, mesh_file%file_prefix, fname_tmp)
      call add_ksm_extension(fname_tmp, file_name)
!
      call output_single_surface_grid(file_name, istack_nsurf,          &
     &    nnod_4_surf, nnod_4_edge, view_mesh, domain_grps,             &
     &    view_nod_grps, view_ele_grps, view_sf_grps)
!
      end subroutine sel_output_single_surface_grid
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine sel_mpi_output_surface_grid                            &
     &         (mesh_file, nnod_4_surf, nnod_4_edge,                    &
     &          mgd_v_mesh_p, mgd_view_mesh)
!
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: nnod_4_edge
!
      type(field_IO_params), intent(in) :: mesh_file
      type(merged_viewer_mesh), intent(in) :: mgd_v_mesh_p
      type(merged_viewer_mesh), intent(in) :: mgd_view_mesh
!
      character(len = kchara) :: file_name
!
!
      call add_ksm_extension                                            &
     &   (mesh_file%file_prefix, file_name)
!
#ifdef ZLIB_IO
      if(mesh_file%iflag_format .eq. id_gzip_txt_file_fmt) then
        call gz_mpi_write_viewer_mesh_file(file_name,                   &
     &      nnod_4_surf, nnod_4_edge, mgd_v_mesh_p, mgd_view_mesh)
        return
      end if
#endif
!
      call mpi_write_viewer_mesh_file(file_name,                        &
     &    nnod_4_surf, nnod_4_edge, mgd_v_mesh_p, mgd_view_mesh)
!
      end subroutine sel_mpi_output_surface_grid
!
!------------------------------------------------------------------
!
      end module viewer_mesh_IO_select
