!>@file   t_control_data_lic_pvr.f90
!!@brief  module t_control_data_lic_pvr
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_control_lic_pvr_file(id_control, fname_lic_ctl, &
!!     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!!      subroutine read_lic_pvr_ctl                                     &
!!     &         (id_control, hd_block, pvr, lic_ctl, c_buf)
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
!!  lic_image_format     PNG
!!  monitoring_mode      YES
!!  image_tranceparency  tranceparent
!!
!!  streo_imaging        YES
!!  anaglyph_image       YES
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
!!  begin image_rotation_ctl
!!   ...
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_lic_pvr
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_pvr
      use t_control_data_LIC
      use skip_comment_f
!
      implicit  none
!
!     2nd level for volume_rendering
!
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_updated =     'updated_sign'
      character(len=kchara), parameter                                  &
     &             :: hd_lic_file_head =   'lic_file_prefix'
      character(len=kchara), parameter                                  &
     &             :: hd_lic_out_type =    'lic_image_format'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_monitor =   'monitoring_mode'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_rgba_type = 'image_tranceparency'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_maxpe_composit = 'max_pe_4_composit'
!
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_streo =    'streo_imaging'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_anaglyph = 'anaglyph_image'
!
      character(len=kchara), parameter                                  &
     &             :: hd_lic_control = 'LIC_ctl'
!
!     3rd level for surface_define
!
      character(len=kchara), parameter                                  &
     &             :: hd_plot_area =   'plot_area_ctl'
!
!     3rd level for rotation
!
      character(len=kchara), parameter                                  &
     &             :: hd_view_transform = 'view_transform_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_lic_colordef =  'LIC_color_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_colormap =      'colormap_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_lighting =  'lighting_ctl'
!
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_sections = 'section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_isosurf =  'isosurface_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_movie =    'movie_mode_ctl'
!
      integer(kind = kint), parameter :: n_label_LIC_pvr = 18
!
!
      private :: hd_view_transform, hd_pvr_lighting, hd_lic_colordef
      private :: hd_colormap, n_label_LIC_pvr
!
      private :: hd_lic_file_head, hd_lic_out_type, hd_pvr_rgba_type
      private :: hd_pvr_streo, hd_pvr_anaglyph, hd_pvr_updated
      private :: hd_lic_control, hd_pvr_monitor, hd_pvr_movie
      private :: hd_pvr_sections, hd_pvr_isosurf, hd_pvr_colorbar
      private :: hd_plot_area, hd_pvr_maxpe_composit
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_lic_pvr_file(id_control, fname_lic_ctl,   &
     &          hd_lic_ctl, pvr_ctl_type, lic_ctl_type)
!
      integer(kind = kint), intent(in) :: id_control
      character(len = kchara), intent(in) :: hd_lic_ctl
      character(len = kchara), intent(in) :: fname_lic_ctl
      type(pvr_parameter_ctl), intent(inout) :: pvr_ctl_type
      type(lic_parameter_ctl), intent(inout) :: lic_ctl_type
!
      type(buffer_for_control) :: c_buf1
!
!
      if(fname_lic_ctl .eq. 'NO_FILE') return
!
      write(*,*) 'LIC control file: ', trim(fname_lic_ctl)
!
      open(id_control, file=fname_lic_ctl, status='old')
      do
        call load_one_line_from_control(id_control, c_buf1)
        call read_lic_pvr_ctl                                           &
     &     (id_control, hd_lic_ctl, pvr_ctl_type, lic_ctl_type, c_buf1)
        if(pvr_ctl_type%i_pvr_ctl .gt. 0) exit
      end do
      close(id_control)
!
      end subroutine read_control_lic_pvr_file
!
!  ---------------------------------------------------------------------
!
      subroutine read_lic_pvr_ctl                                       &
     &         (id_control, hd_block, pvr, lic_ctl, c_buf)
!
      use t_ctl_data_4_view_transfer
      use t_control_data_pvr_isosurfs
      use t_control_data_pvr_movie
      use t_control_data_pvr_area
      use read_control_pvr_modelview
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
      pvr%view_file_ctl = 'NO_FILE'
      pvr%color_file_ctl = 'NO_FILE'
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_file_flag(c_buf, hd_view_transform)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &              'Read file for ', trim(hd_view_transform), '... '
          pvr%view_file_ctl = third_word(c_buf)
          call read_control_modelview_file                              &
     &       (id_control+2, pvr%view_file_ctl, pvr%mat)
        else if(check_begin_flag(c_buf, hd_view_transform)) then
          write(*,*) 'Modelview control is included'
          call read_view_transfer_ctl(id_control, hd_view_transform,    &
     &        pvr%mat, c_buf)
        end if
!
        if(check_file_flag(c_buf, hd_lic_colordef)) then
          write(*,'(3a)', ADVANCE='NO')                                 &
     &               'Read file for ', trim(hd_lic_colordef), '... '
          pvr%color_file_ctl = third_word(c_buf)
          call read_control_pvr_colormap_file                           &
     &       (id_control+2, pvr%color_file_ctl, hd_lic_colordef,        &
     &        pvr%cmap_cbar_c)
        end if
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
        if(check_array_flag(c_buf, hd_pvr_sections)) then
          call read_pvr_sections_ctl(id_control, hd_pvr_sections,       &
     &        pvr%pvr_scts_c, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_isosurf)) then
          call read_pvr_isosurfs_ctl(id_control, hd_pvr_isosurf,        &
     &        pvr%pvr_isos_c, c_buf)
        end if
!
        call read_pvr_render_area_ctl(id_control, hd_plot_area,         &
     &      pvr%render_area_c, c_buf)
        call read_lighting_ctl(id_control, hd_pvr_lighting,             &
     &      pvr%light, c_buf)
        call read_pvr_rotation_ctl(id_control, hd_pvr_movie,            &
     &      pvr%movie, c_buf)
!
        call read_lic_control_data                                      &
     &     (id_control, hd_lic_control, lic_ctl, c_buf)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_updated, pvr%updated_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_file_head, pvr%file_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_lic_out_type, pvr%file_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_monitor, pvr%monitoring_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_rgba_type, pvr%transparent_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_streo, pvr%streo_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_pvr_anaglyph, pvr%anaglyph_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_pvr_maxpe_composit, pvr%maxpe_composit_ctl)
      end do
      pvr%i_pvr_ctl = 1
!
      end subroutine read_lic_pvr_ctl
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
      call set_control_labels(hd_lic_out_type,   names( 3))
      call set_control_labels(hd_pvr_monitor,    names( 4))
      call set_control_labels(hd_pvr_rgba_type,  names( 5))
!
      call set_control_labels(hd_pvr_maxpe_composit, names( 6))
      call set_control_labels(hd_pvr_streo,          names( 7))
      call set_control_labels(hd_pvr_anaglyph,       names( 8))
!
      call set_control_labels(hd_lic_control,    names( 9))
!
      call set_control_labels(hd_plot_area,      names(10))
      call set_control_labels(hd_view_transform, names(11))
      call set_control_labels(hd_lic_colordef,   names(12))
      call set_control_labels(hd_colormap,       names(13))
      call set_control_labels(hd_pvr_lighting,   names(14))
      call set_control_labels(hd_pvr_colorbar,   names(15))
!
      call set_control_labels(hd_pvr_sections,   names(16))
      call set_control_labels(hd_pvr_isosurf,    names(17))
      call set_control_labels(hd_pvr_movie,      names(18))
!
      end subroutine set_ctl_label_LIC_pvr
!
! ----------------------------------------------------------------------
!
      end module t_control_data_lic_pvr
