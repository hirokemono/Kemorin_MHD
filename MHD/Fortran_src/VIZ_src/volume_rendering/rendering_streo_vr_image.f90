!>@file  rendering_streo_vr_image.f90
!!       module rendering_streo_vr_image
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief Structures for position in the projection coordinate 
!!
!!@verbatim
!!      subroutine rendering_with_rotation(istep_pvr, time, mesh, group,&
!!     &          field_pvr, pvr_rgb, pvr_param, pvr_proj)
!!      subroutine anaglyph_rendering_w_rotation                        &
!!     &         (istep_pvr, time, mesh, group, field_pvr, pvr_rgb,     &
!!     &          pvr_param, pvr_proj)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(mesh_groups), intent(in) :: group
!!        type(pvr_field_data), intent(in) :: field_pvr
!!        type(pvr_image_type), intent(in) :: pvr_rgb
!!        type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!!        type(PVR_control_params), intent(inout) :: pvr_param
!!@endverbatim
!
      module rendering_streo_vr_image
!
      use m_precision
      use m_machine_parameter
      use m_constants
      use m_work_time
!
      use calypso_mpi
!
      use t_mesh_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_geometries_in_pvr_screen
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_field_data
      use generate_vr_image
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine rendering_with_rotation(istep_pvr, time, mesh, group,  &
     &          field_pvr, pvr_rgb, pvr_param, pvr_proj)
!
      use t_rotation_pvr_images
      use t_MPI_quilt_bitmap_IO
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use write_PVR_image
      use set_parallel_file_name
      use output_image_sel_4_png
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(pvr_field_data), intent(in) :: field_pvr
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj
!
      character(len=kchara) :: file_w_step
      integer(kind = kint) :: i_rot, iflag_img_fmt
      type(rotation_pvr_images) :: rot_imgs1
      type(MPI_quilt_bitmap_IO) :: quilt_d1
!
!
      if(my_rank .eq. 0) write(*,*) 'init_rot_pvr_image_arrays'
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%view, pvr_rgb, rot_imgs1)
!
      do i_rot = 1, pvr_param%view%num_frame
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_param%view, pvr_param%color)
!
        call rendering_at_once(istep_pvr, time, mesh, group, field_pvr, &
     &      pvr_param, pvr_proj, rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      call calypso_mpi_barrier
!
      call set_sequence_rgb_data                                        &
     &   (pvr_param%view, rot_imgs1%rot_pvr_rgb, quilt_d1)
!
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      if(pvr_param%view%iflag_movie_fmt .eq. iflag_UNDEFINED) then
        iflag_img_fmt = pvr_param%view%iflag_movie_fmt
      else
        iflag_img_fmt = pvr_rgb%id_pvr_file_type
      end if
      if(istep_pvr .ge. 0) then
        file_w_step = add_int_suffix(istep_pvr, pvr_rgb%pvr_prefix)
      else
        file_w_step = pvr_rgb%pvr_prefix
      end if
      call sel_write_pvr_image_files                                    &
     &   (iflag_img_fmt, file_w_step, quilt_d1)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
!
      call dealloc_quilt_rgb_images(quilt_d1)
      call dealloc_rot_pvr_image_arrays(pvr_param%view, rot_imgs1)
!
      end subroutine rendering_with_rotation
!
!  ---------------------------------------------------------------------
!
      subroutine anaglyph_rendering_w_rotation                          &
     &         (istep_pvr, time, mesh, group, field_pvr, pvr_rgb,       &
     &          pvr_param, pvr_proj)
!
      use t_rotation_pvr_images
      use m_elapsed_labels_4_VIZ
      use cal_pvr_modelview_mat
      use write_PVR_image
!
      integer(kind = kint), intent(in) :: istep_pvr
      real(kind = kreal), intent(in) :: time
!
      type(mesh_geometry), intent(in) :: mesh
      type(mesh_groups), intent(in) :: group
      type(pvr_field_data), intent(in) :: field_pvr
      type(pvr_image_type), intent(in) :: pvr_rgb
!
      type(PVR_control_params), intent(inout) :: pvr_param
      type(PVR_projection_data), intent(inout) :: pvr_proj(2)
!
      integer(kind = kint) :: i_rot
      type(rotation_pvr_images) :: rot_imgs1
!
!
      if(my_rank .eq. 0) write(*,*) 'init_rot_pvr_image_arrays'
      call init_rot_pvr_image_arrays                                    &
     &   (pvr_param%view, pvr_rgb, rot_imgs1)
!
      do i_rot = 1, pvr_param%view%num_frame
        call cal_pvr_modelview_matrix                                   &
     &     (i_rot, pvr_param%outline, pvr_param%view, pvr_param%color)
!
!    Left eye
        call rendering_at_once(istep_pvr, time, mesh, group, field_pvr, &
     &      pvr_param, pvr_proj(1), rot_imgs1%rot_pvr_rgb(i_rot))
        call store_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
!
!    Right eye
        call rendering_at_once(istep_pvr, time, mesh, group, field_pvr, &
     &      pvr_param, pvr_proj(2), rot_imgs1%rot_pvr_rgb(i_rot))
        call add_left_eye_image(rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      call calypso_mpi_barrier
!
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+1)
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+2)
      do i_rot = 1, pvr_param%view%num_frame
        call sel_write_pvr_image_file                                   &
     &     (i_rot, istep_pvr, rot_imgs1%rot_pvr_rgb(i_rot))
      end do
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+2)
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+1)
      call dealloc_rot_pvr_image_arrays(pvr_param%view, rot_imgs1)
!
      end subroutine anaglyph_rendering_w_rotation
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_sequence_rgb_data(view_param, rot_rgb, quilt_d)
!
      use t_pvr_image_array
      use t_MPI_quilt_bitmap_IO
      use convert_real_rgb_2_bite
!
      type(pvr_view_parameter), intent(inout) :: view_param
      type(pvr_image_type), intent(in) :: rot_rgb(view_param%num_frame)
      type(MPI_quilt_bitmap_IO), intent(inout) :: quilt_d
!
      integer(kind = kint) :: i_rot, icou
!
      quilt_d%n_column(1) = view_param%n_row
      quilt_d%n_column(2) = view_param%n_column
      quilt_d%n_image = view_param%num_frame
!
      icou = 0
      do i_rot = 1, view_param%num_frame
        if(my_rank .eq. rot_rgb(i_rot)%irank_image_file) icou = icou+1
      end do
      quilt_d%num_image_lc = icou
!
      call alloc_quilt_rgb_images                                       &
     &   (rot_rgb(1)%num_pixels(1), rot_rgb(1)%num_pixels(2), quilt_d)
!
      icou = 0
      do i_rot = 1, view_param%num_frame
        if(my_rank .eq. rot_rgb(i_rot)%irank_image_file) then
          icou = icou + 1
          quilt_d%icou_each_pe(icou) = my_rank
          call cvt_double_rgba_to_char_rgb                              &
     &       (rot_rgb(i_rot)%num_pixel_xy, rot_rgb(i_rot)%rgba_real_gl, &
     &        quilt_d%images(i_rot)%rgb(1,1,1))
        end if
      end do
!
      end subroutine set_sequence_rgb_data
!
!  ---------------------------------------------------------------------
!
      end module rendering_streo_vr_image
