!>@file   read_pvr_control.f90
!!@brief  module read_pvr_control
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief REad control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_ctl                                         &
!!     &         (id_control, hd_block, hd_pvr_colordef, pvr_ctl, c_buf)
!!      subroutine read_pvr_update_flag                                 &
!!     &         (id_control, hd_block, pvr_ctl, c_buf)
!!        type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      integer(kind = kint) function num_label_pvr_ctl()
!!      integer(kind = kint) function num_label_pvr_ctl_w_dup()
!!      subroutine set_label_pvr_ctl_w_dup(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!     example of control for Kemo's volume rendering
!!
!!begin volume_rendering   (BMP or PNG)
!!  updated_sign         go
!!  pvr_file_head        pvr_temp
!!  pvr_output_type      PNG
!!  monitoring_mode      YES
!!  image_tranceparency  tranceparent
!!
!!  streo_imaging        YES
!!  anaglyph_image       YES
!!!
!!  max_pe_4_composit     32
!!
!!  output_field    temperature
!!  output_component     scalar
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
!!  begin pvr_color_ctl
!!   ...
!!  end   pvr_color_ctl
!!!
!!  begin colorbar_ctl
!!   ...
!!  end colorbar_ctl
!!!
!!  array section_ctl  2
!!    file section_ctl     ctl_psf_eq
!!    begin section_ctl
!!      ...
!!    end section_ctl
!!  end array section_ctl
!!!
!!  array isosurface_ctl  2
!!    begin isosurface_ctl
!!      isosurf_value       0.3
!!      opacity_ctl         0.9
!!      surface_direction   normal
!!    end isosurface_ctl
!!     ...
!!  end array isosurface_ctl
!!!
!!  begin image_rotation_ctl
!!   ...
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module read_pvr_control
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_view_transfer
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_chara2real
      use t_ctl_data_pvr_colormap_bar
      use t_ctl_data_pvr_light
      use t_control_data_pvr_sections
      use t_control_data_pvr_movie
      use t_control_data_pvr_isosurfs
      use t_control_data_pvr_area
      use t_control_data_4_pvr
      use skip_comment_f
!
      implicit  none
!
!
!     2nd level for volume_rendering
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_updated =     'updated_sign'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_file_head =   'pvr_file_head'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_out_type =    'pvr_output_type'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_monitor =   'monitoring_mode'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_rgba_type = 'image_tranceparency'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_maxpe_composit = 'max_pe_4_composit'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_streo =    'streo_imaging'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_anaglyph = 'anaglyph_image'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_output_field_def = 'output_field'
      character(len=kchara), parameter, private                         &
     &             :: hd_output_comp_def =  'output_component'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_sections = 'section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_isosurf =  'isosurface_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_movie =     'movie_mode_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara), parameter, private                         &
     &             :: hd_plot_area =   'plot_area_ctl'
!
      character(len=kchara), parameter, private                         &
     &              :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_pvr_colordef =  'pvr_color_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_colormap =      'colormap_ctl'
      character(len=kchara), parameter, private                         &
     &              :: hd_pvr_lighting =  'lighting_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_colorbar =  'colorbar_ctl'
!
!       Deprecated label
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_rotation =  'image_rotation_ctl'
!
      integer(kind = kint), parameter :: n_label_pvr_ctl =       19
      integer(kind = kint), parameter :: n_label_pvr_ctl_w_dup = 20
!
      private :: n_label_pvr_ctl, n_label_pvr_ctl_w_dup
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_ctl                                           &
     &         (id_control, hd_block, pvr_ctl, c_buf)
!
      use read_control_pvr_modelview
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_ctl%i_pvr_ctl .gt. 0) return
!
      pvr_ctl%view_file_ctl = 'NO_FILE'
      pvr_ctl%color_file_ctl = 'NO_FILE'
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_view_transform)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &            'Read file for ', trim(hd_view_transform), '... '
          pvr_ctl%view_file_ctl = third_word(c_buf)
          call read_control_modelview_file                              &
     &       (id_control+2, pvr_ctl%view_file_ctl, pvr_ctl%mat)
        else if(check_begin_flag(c_buf, hd_view_transform)) then
          write(*,*)  'Modelview control is included'
          call read_view_transfer_ctl(id_control, hd_view_transform,    &
     &        pvr_ctl%mat, c_buf)
        end if
!
        if(check_file_flag(c_buf, hd_pvr_colordef)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &              'Read file for ', trim(hd_pvr_colordef), '... '
          pvr_ctl%color_file_ctl = third_word(c_buf)
          call read_control_pvr_colormap_file                           &
     &       (id_control+2, pvr_ctl%color_file_ctl, hd_pvr_colordef,    &
     &        pvr_ctl%cmap_cbar_c)
        end if
!
        if(pvr_ctl%cmap_cbar_c%i_cmap_cbar .eq. 0) then
          call read_pvr_colordef_ctl(id_control, hd_pvr_colordef,       &
     &        pvr_ctl%cmap_cbar_c%color, c_buf)
          call read_pvr_colordef_ctl(id_control, hd_colormap,           &
     &        pvr_ctl%cmap_cbar_c%color, c_buf)
!
          call read_pvr_colorbar_ctl(id_control, hd_pvr_colorbar,       &
     &        pvr_ctl%cmap_cbar_c%cbar_ctl, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_sections)) then
          call read_pvr_sections_ctl(id_control, hd_pvr_sections,       &
     &        pvr_ctl%pvr_scts_c, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_isosurf)) then
          call read_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,        &
     &        pvr_ctl%pvr_isos_c, c_buf)
        end if
!
        call read_pvr_render_area_ctl(id_control, hd_plot_area,         &
     &      pvr_ctl%render_area_c, c_buf)
        call read_lighting_ctl(id_control, hd_pvr_lighting,             &
     &      pvr_ctl%light, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_pvr_movie,            &
     &      pvr_ctl%movie, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_pvr_rotation,         &
     &      pvr_ctl%movie, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_file_head, pvr_ctl%file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_out_type, pvr_ctl%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_monitor, pvr_ctl%monitoring_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_rgba_type, pvr_ctl%transparent_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_streo, pvr_ctl%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_anaglyph, pvr_ctl%anaglyph_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_field_def, pvr_ctl%pvr_field_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_output_comp_def, pvr_ctl%pvr_comp_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_pvr_maxpe_composit, pvr_ctl%maxpe_composit_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_update_flag                                   &
     &         (id_control, hd_block, pvr_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pvr_ctl%i_pvr_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr_ctl%updated_ctl)
      end do
      pvr_ctl%i_pvr_ctl = 1
!
      end subroutine read_pvr_update_flag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_ctl()
      num_label_pvr_ctl = n_label_pvr_ctl
      return
      end function num_label_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_ctl_w_dup()
      num_label_pvr_ctl_w_dup = n_label_pvr_ctl_w_dup
      return
      end function num_label_pvr_ctl_w_dup
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_ctl_w_dup(names)
!
      use t_read_control_elements
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_ctl_w_dup)
!
!
      call set_control_labels(hd_pvr_updated,        names( 1))
!
      call set_control_labels(hd_pvr_file_head,      names( 2))
      call set_control_labels(hd_pvr_out_type,       names( 3))
      call set_control_labels(hd_pvr_monitor,        names( 4))
      call set_control_labels(hd_pvr_rgba_type,      names( 5))
!
      call set_control_labels(hd_pvr_maxpe_composit, names( 6))
      call set_control_labels(hd_pvr_streo,          names( 7))
      call set_control_labels(hd_pvr_anaglyph,       names( 8))
!
      call set_control_labels(hd_output_field_def, names( 9))
      call set_control_labels(hd_output_comp_def,  names(10))
!
      call set_control_labels(hd_plot_area,      names(11))
      call set_control_labels(hd_view_transform, names(12))
      call set_control_labels(hd_pvr_colordef,   names(13))
      call set_control_labels(hd_colormap,       names(14))
      call set_control_labels(hd_pvr_lighting,   names(15))
      call set_control_labels(hd_pvr_colorbar,   names(16))
!
      call set_control_labels(hd_pvr_sections, names(17))
      call set_control_labels(hd_pvr_isosurf,  names(18))
      call set_control_labels(hd_pvr_movie,    names(19))
      call set_control_labels(hd_pvr_rotation, names(20))
!
      end subroutine set_label_pvr_ctl_w_dup
!
! ----------------------------------------------------------------------
!
      end module read_pvr_control
