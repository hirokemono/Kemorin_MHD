!>@file   ctl_data_lic_pvr_IO.f90
!!@brief  module ctl_data_lic_pvr_IO
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!      subroutine read_lic_pvr_ctl                                     &
!!     &         (id_control, hd_block, pvr, lic_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_lic_pvr_ctl(id_control, hd_block,              &
!!     &                             pvr, lic_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_parameter_ctl), intent(in) :: pvr
!!        type(lic_parameter_ctl), intent(in) :: lic_ctl
!!        integer(kind = kint), intent(inout) :: level
!!
!!      subroutine dealloc_lic_count_data(pvr, lic_ctl)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr
!!        type(lic_parameter_ctl), intent(inout) :: lic_ctl
!!
!!      integer(kind = kint) function num_ctl_label_LIC_pvr()
!!      subroutine set_ctl_label_LIC_pvr(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  lic_file_prefix      pvr_temp
!!  lic_output_format    PNG
!!  monitoring_mode      YES
!!
!!  streo_imaging        YES
!!  anaglyph_switch      NO
!!  quilt_3d_imaging     YES
!!!
!!  begin LIC_ctl
!!   ...
!!  end  LIC_ctl
!!!
!!  begin plot_area_ctl
!!   ...
!!  end  plot_area_ctl
!!!
!!  begin view_transform_ctl
!!   ...
!!  end view_transform_ctl
!!
!!  begin lighting_ctl
!!   ...
!!  end lighting_ctl
!!
!!  begin colormap_ctl
!!   ...
!!  end   colormap_ctl
!!!
!!  begin colorbar_ctl
!!   ...
!!  end colorbar_ctl
!!!
!!  begin quilt_image_ctl
!!   ...
!!  end quilt_image_ctl
!!
!!  begin snapshot_movie_ctl
!!   ...
!!  end snapshot_movie_ctl
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module ctl_data_lic_pvr_IO
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_pvr
      use t_control_data_LIC
      use t_ctl_data_4_view_transfer
      use t_control_data_pvr_isosurfs
      use t_ctl_data_pvr_movie
      use t_ctl_data_quilt_image
      use t_ctl_data_pvr_area
      use skip_comment_f
!
      implicit  none
!
!     2nd level for volume_rendering
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_updated =     'updated_sign'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_file_head =   'lic_file_prefix'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_out_format =  'lic_output_format'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_monitor =   'monitoring_mode'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_streo =    'streo_imaging'
      character(len=kchara), parameter, private                         &
     &             :: hd_anaglyph_switch = 'anaglyph_switch'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_quilt_3d = 'quilt_3d_imaging'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_plot_area =   'plot_area_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_control = 'LIC_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_colordef =  'LIC_color_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_colormap =      'colormap_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_colorbar =  'colorbar_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_sections = 'section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_isosurf =  'isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_quilt_image =  'quilt_image_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_snapshot_movie = 'snapshot_movie_ctl'
!
!   Deprecated labels
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_out_type =    'lic_image_format'
!
      integer(kind = kint), parameter :: n_label_LIC_pvr = 18
!
      private :: n_label_LIC_pvr
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_pvr_ctl                                       &
     &         (id_control, hd_block, pvr, lic_ctl, c_buf)
!
      use ctl_data_LIC_IO
      use ctl_file_pvr_modelview_IO
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
      use ctl_file_pvr_light_IO
      use ctl_data_pvr_movie_IO
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr%i_pvr_ctl .gt. 0) return
!
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_modelview_file(id_control, hd_view_transform, &
     &      izero, pvr%fname_mat_ctl, pvr%mat, c_buf)
!
        call sel_read_ctl_pvr_colormap_file                             &
     &     (id_control, hd_lic_colordef, pvr%fname_cmap_cbar_c,         &
     &      pvr%cmap_cbar_c, c_buf)
        call sel_read_ctl_pvr_light_file(id_control, hd_pvr_lighting,   &
     &      pvr%fname_pvr_light_c, pvr%light, c_buf)
!
        if(pvr%cmap_cbar_c%i_cmap_cbar .eq. 0) then
          call read_pvr_colordef_ctl(id_control, hd_lic_colordef,       &
     &        pvr%cmap_cbar_c%color, c_buf)
          call read_pvr_colordef_ctl(id_control, hd_colormap,           &
     &        pvr%cmap_cbar_c%color, c_buf)
!
          call read_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,       &
     &        pvr%cmap_cbar_c%cbar_ctl, c_buf)
        end if
!
        call read_pvr_sections_ctl(id_control, hd_pvr_sections,         &
     &      pvr%pvr_scts_c, c_buf)
        call read_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,          &
     &      pvr%pvr_isos_c, c_buf)
!
        call read_pvr_render_area_ctl(id_control, hd_plot_area,         &
     &      pvr%render_area_c, c_buf)
        call read_quilt_image_ctl(id_control, hd_quilt_image,           &
     &      pvr%quilt_c, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_snapshot_movie,       &
     &      pvr%movie, c_buf)
!
        call s_read_lic_control_data                                    &
     &     (id_control, hd_lic_control, lic_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr%updated_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_file_head, pvr%file_head_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_out_format, pvr%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_out_type, pvr%file_fmt_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_monitor, pvr%monitoring_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_streo, pvr%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_anaglyph_switch, pvr%anaglyph_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_quilt_3d, pvr%quilt_ctl)
      end do
      pvr%i_pvr_ctl = 1
!
      end subroutine read_lic_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine write_lic_pvr_ctl(id_control, hd_block,                &
     &                             pvr, lic_ctl, level)
!
      use ctl_data_LIC_IO
      use ctl_file_pvr_modelview_IO
      use ctl_data_pvr_colorbar_IO
      use ctl_data_pvr_colormap_IO
      use ctl_file_pvr_light_IO
      use ctl_data_pvr_movie_IO
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_parameter_ctl), intent(in) :: pvr
      type(lic_parameter_ctl), intent(in) :: lic_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(pvr%i_pvr_ctl .le. 0) return
!
      maxlen = len_trim(hd_pvr_updated)
      maxlen = max(maxlen, len_trim(hd_lic_file_head))
      maxlen = max(maxlen, len_trim(hd_lic_out_format))
      maxlen = max(maxlen, len_trim(hd_pvr_monitor))
      maxlen = max(maxlen, len_trim(hd_anaglyph_switch))
      maxlen = max(maxlen, len_trim(hd_pvr_streo))
      maxlen = max(maxlen, len_trim(hd_pvr_quilt_3d))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_updated, pvr%updated_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_file_head, pvr%file_head_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_lic_out_format, pvr%file_fmt_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_monitor, pvr%monitoring_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_streo, pvr%streo_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_anaglyph_switch, pvr%anaglyph_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_pvr_quilt_3d, pvr%quilt_ctl)
!
      call write_pvr_render_area_ctl(id_control, hd_plot_area,          &
     &                               pvr%render_area_c, level)
      call write_lic_control_data(id_control, hd_lic_control,           &
     &                            lic_ctl, level)
!
      write(*,'(2a)', ADVANCE='NO') '!  ', trim(hd_view_transform)
      call sel_write_ctl_modelview_file(id_control, hd_view_transform,  &
     &    pvr%fname_mat_ctl, pvr%mat, level)
!
      call sel_write_ctl_pvr_colormap_file                              &
     &   (id_control, hd_lic_colordef, pvr%fname_cmap_cbar_c,           &
     &    pvr%cmap_cbar_c, level)
!
      if(pvr%cmap_cbar_c%i_cmap_cbar .eq. 0) then
        call write_pvr_colordef_ctl(id_control, hd_colormap,            &
     &                              pvr%cmap_cbar_c%color, level)
        call write_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,        &
     &                              pvr%cmap_cbar_c%cbar_ctl, level)
      end if
!
      call sel_write_ctl_pvr_light_file                                 &
     &   (id_control, hd_pvr_lighting, pvr%fname_pvr_light_c,           &
     &    pvr%light, level)
!
      call write_pvr_sections_ctl(id_control, hd_pvr_sections,          &
     &                            pvr%pvr_scts_c, level)
      call write_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,           &
     &                            pvr%pvr_isos_c, level)
!
      call write_quilt_image_ctl(id_control, hd_quilt_image,            &
     &                           pvr%quilt_c, level)
      call write_pvr_rotation_ctl(id_control, hd_snapshot_movie,        &
     &                            pvr%movie, level)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_lic_pvr_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_lic_count_data(pvr, lic_ctl)
!
      type(pvr_parameter_ctl), intent(inout) :: pvr
      type(lic_parameter_ctl), intent(inout) :: lic_ctl
!
!
      call dealloc_lic_control_flags(lic_ctl)
      call deallocate_cont_dat_pvr(pvr)
!
      end subroutine dealloc_lic_count_data
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_ctl_label_LIC_pvr()
      num_ctl_label_LIC_pvr = n_label_LIC_pvr
      return
      end function num_ctl_label_LIC_pvr
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_label_LIC_pvr(names)
!
      character(len = kchara), intent(inout) :: names(n_label_LIC_pvr)
!
!
      call set_control_labels(hd_pvr_updated,    names( 1))
!
      call set_control_labels(hd_lic_file_head,  names( 2))
      call set_control_labels(hd_lic_out_format, names( 3))
      call set_control_labels(hd_pvr_monitor,    names( 4))
!
      call set_control_labels(hd_pvr_streo,       names( 5))
      call set_control_labels(hd_anaglyph_switch, names( 6))
      call set_control_labels(hd_pvr_quilt_3d,    names( 7))
!
      call set_control_labels(hd_lic_control,    names( 8))
!
      call set_control_labels(hd_plot_area,      names( 9))
      call set_control_labels(hd_view_transform, names(10))
      call set_control_labels(hd_lic_colordef,   names(11))
      call set_control_labels(hd_colormap,       names(12))
      call set_control_labels(hd_pvr_lighting,   names(13))
      call set_control_labels(hd_pvr_colorbar,   names(14))
!
      call set_control_labels(hd_pvr_sections,   names(15))
      call set_control_labels(hd_pvr_isosurf,    names(16))
      call set_control_labels(hd_quilt_image,    names(17))
      call set_control_labels(hd_snapshot_movie, names(18))
!
      end subroutine set_ctl_label_LIC_pvr
!
! ----------------------------------------------------------------------
!
      end module ctl_data_lic_pvr_IO
