!>@file   ctl_data_visualiser_IO.f90
!!@brief  module ctl_data_visualiser_IO
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine s_read_viz_controls(id_control, viz_ctls, c_buf)
!!        integer(kind = kint), intent(in) :: id_control 
!!        character(len=kchara), intent(in) :: hd_block
!!        type(visualization_controls), intent(inout) :: viz_ctls
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_viz_controls(id_control, viz_ctls, level)
!!        integer(kind = kint), intent(in) :: id_control 
!!        character(len=kchara), intent(in) :: hd_block
!!        type(visualization_controls), intent(in) :: viz_ctls
!!        integer(kind = kint), intent(inout) :: level
!!
!!      integer(kind = kint) function num_label_vizs()
!!      integer(kind = kint) function num_label_vizs_w_dep()
!!      subroutine set_label_vizs(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array  cross_section_ctl
!!      ....
!!    end array cross_section_ctl
!!
!!    array  isosurface_ctl
!!      ....
!!    end array isosurface_ctl
!!
!!    array  map_rendering_ctl
!!      ....
!!    end array map_rendering_ctl
!!
!!    array  volume_rendering
!!      ....
!!    end array volume_rendering
!!
!!    array  LIC_rendering
!!      ....
!!    end array LIC_rendering
!!
!!    array  fieldline
!!      ....
!!    end array fieldline
!!  end  visual_control
!!
!!    delta_t_sectioning_ctl       1.0e-3
!!    i_step_sectioning_ctl        400
!!    delta_t_isosurface_ctl       1.0e-3
!!    i_step_isosurface_ctl        400
!!    delta_t_map_projection_ctl   1.0e-3
!!    i_step_map_projection_ctl    400
!!    delta_t_pvr_ctl              1.0e-2
!!    i_step_pvr_ctl               400
!!    delta_t_fline_ctl            1.0e-1
!!    i_step_fline_ctl             400
!!    delta_t_LIC_ctl              1.0e-1
!!    i_step_LIC_ctl               400
!!    delta_t_field_ctl            1.0e-3
!!    i_step_field_ctl             800
!!    output_field_file_fmt_ctl   'VTK'
!!
!!    begin LIC_repartition_ctl
!!      ....
!!    end LIC_repartition_ctl
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module ctl_data_visualiser_IO
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_vizs
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_data_pvrs
      use t_control_data_flines
      use t_control_data_LIC_pvrs
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_ctl_data_FEM_sleeve_size
!
      implicit  none
!
!     Top level
      character(len=kchara), parameter, private                         &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_map_rendering = 'map_rendering_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_ctl = 'LIC_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_fline_ctl =  'fieldline'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_section =        'i_step_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_isosurf =        'i_step_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_map_projection = 'i_step_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_pvr =            'i_step_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_lic =            'i_step_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_fline =          'i_step_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
!
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_section =        'delta_t_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_isosurf =        'delta_t_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_map_projection = 'delta_t_map_projection_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_pvr =            'delta_t_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_lic =            'delta_t_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &      :: hd_delta_t_fline =          'delta_t_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_ucd =       'delta_t_field_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_output_fld_file_fmt = 'output_field_file_fmt_ctl'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_viz_partition = 'viz_repartition_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter, private                         &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_ctl = 'isosurf_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_partition = 'LIC_repartition_ctl'
!
      integer(kind = kint), parameter, private                          &
     &                      :: n_label_vizs = 22
      integer(kind = kint), parameter, private                          &
     &                      :: n_label_vizs_w_dep = 25
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_read_viz_controls                                    &
     &         (id_control, hd_block, viz_ctls, c_buf)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use ctl_file_map_renderings_IO
      use ctl_file_fieldlines_IO
      use ctl_file_volume_repart_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(viz_ctls%i_viz_control .gt. 0) return
      viz_ctls%block_name = trim(hd_block)
      call init_psf_ctls_labels(hd_section_ctl, viz_ctls%psf_ctls)
      call init_iso_ctls_labels(hd_isosurf_ctl, viz_ctls%iso_ctls)
      call init_map_ctls_labels(hd_map_rendering, viz_ctls%map_ctls)
      call init_pvr_ctls_labels(hd_pvr_ctl, viz_ctls%pvr_ctls)
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call sel_read_ctl_file_vol_repart(id_control, hd_viz_partition, &
     &      viz_ctls%fname_vol_repart_ctl, viz_ctls%repart_ctl, c_buf)
        call sel_read_ctl_file_vol_repart(id_control, hd_lic_partition, &
     &      viz_ctls%fname_vol_repart_ctl, viz_ctls%repart_ctl, c_buf)
!
        call read_files_4_psf_ctl(id_control, hd_psf_ctl,               &
     &                            viz_ctls%psf_ctls, c_buf)
        call read_files_4_psf_ctl(id_control, hd_section_ctl,           &
     &                            viz_ctls%psf_ctls, c_buf)
!
        call read_files_4_iso_ctl(id_control, hd_iso_ctl,               &
     &                            viz_ctls%iso_ctls, c_buf)
        call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,           &
     &                            viz_ctls%iso_ctls, c_buf)
!
        call read_files_4_map_ctl(id_control, hd_map_rendering,         &
     &                            viz_ctls%map_ctls, c_buf)
!
        call read_files_4_pvr_ctl(id_control, hd_pvr_ctl,               &
     &                            viz_ctls%pvr_ctls, c_buf)
!
        call read_files_4_fline_ctl(id_control, hd_fline_ctl,           &
     &                              viz_ctls%fline_ctls, c_buf)
!
        call read_files_4_lic_ctl(id_control, hd_lic_ctl,               &
     &                            viz_ctls%lic_ctls, c_buf)
!
        call read_integer_ctl_type(c_buf, hd_i_step_section,            &
     &                             viz_ctls%i_step_psf_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_isosurf,            &
     &                             viz_ctls%i_step_iso_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_map_projection,     &
     &                             viz_ctls%i_step_map_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_pvr,                &
     &                             viz_ctls%i_step_pvr_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_lic,                &
     &                             viz_ctls%i_step_lic_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_fline,              &
     &                             viz_ctls%i_step_fline_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_ucd,                &
     &                             viz_ctls%i_step_ucd_v_ctl)
!
        call read_real_ctl_type(c_buf, hd_delta_t_section,              &
     &                          viz_ctls%delta_t_psf_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_isosurf,              &
     &                          viz_ctls%delta_t_iso_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_map_projection,       &
     &                          viz_ctls%delta_t_map_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_pvr,                  &
     &                          viz_ctls%delta_t_pvr_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_fline,                &
     &                          viz_ctls%delta_t_fline_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_lic,                  &
     &                          viz_ctls%delta_t_lic_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_ucd,                  &
     &                          viz_ctls%delta_t_ucd_v_ctl)
!
        call read_chara_ctl_type(c_buf, hd_output_fld_file_fmt,         &
     &      viz_ctls%output_field_file_fmt_ctl)
      end do
      viz_ctls%i_viz_control = 1
!
      end subroutine s_read_viz_controls
!
!  ---------------------------------------------------------------------
!
      subroutine write_viz_controls(id_control, viz_ctls, level)
!
      use t_read_control_elements
      use ctl_file_sections_IO
      use ctl_file_isosurfaces_IO
      use ctl_file_map_renderings_IO
      use ctl_file_fieldlines_IO
      use ctl_file_volume_repart_IO
      use write_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      type(visualization_controls), intent(in) :: viz_ctls
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(viz_ctls%i_viz_control .le. 0) return
!
      maxlen = len_trim(hd_delta_t_section)
      maxlen = max(maxlen, len_trim(hd_i_step_section))
      maxlen = max(maxlen, len_trim(hd_delta_t_isosurf))
      maxlen = max(maxlen, len_trim(hd_i_step_isosurf))
      maxlen = max(maxlen, len_trim(hd_delta_t_map_projection))
      maxlen = max(maxlen, len_trim(hd_i_step_map_projection))
      maxlen = max(maxlen, len_trim(hd_delta_t_pvr))
      maxlen = max(maxlen, len_trim(hd_i_step_pvr))
      maxlen = max(maxlen, len_trim(hd_delta_t_lic))
      maxlen = max(maxlen, len_trim(hd_i_step_lic))
      maxlen = max(maxlen, len_trim(hd_delta_t_fline))
      maxlen = max(maxlen, len_trim(hd_i_step_fline))
      maxlen = max(maxlen, len_trim(hd_delta_t_ucd))
      maxlen = max(maxlen, len_trim(hd_i_step_ucd))
      maxlen = max(maxlen, len_trim(hd_output_fld_file_fmt))
!
      level = write_begin_flag_for_ctl(id_control, level,               &
     &                                 viz_ctls%block_name)
      call sel_write_ctl_file_vol_repart(id_control, hd_viz_partition,  &
     &    viz_ctls%fname_vol_repart_ctl, viz_ctls%repart_ctl, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_psf_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_psf_v_ctl)
      call write_files_4_psf_ctl(id_control, hd_section_ctl,            &
     &                           viz_ctls%psf_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_iso_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_iso_v_ctl)
      call write_files_4_iso_ctl(id_control, hd_isosurf_ctl,            &
     &                           viz_ctls%iso_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_map_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_map_v_ctl)
      call write_files_4_map_ctl(id_control, hd_map_rendering,          &
     &                           viz_ctls%map_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_pvr_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_pvr_v_ctl)
      call write_files_4_pvr_ctl(id_control, hd_pvr_ctl,                &
     &                           viz_ctls%pvr_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_lic_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_lic_v_ctl)
      call write_files_4_lic_ctl(id_control, hd_lic_ctl,                &
     &                           viz_ctls%lic_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_fline_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_fline_v_ctl)
      call write_files_4_fline_ctl(id_control, hd_fline_ctl,            &
     &                             viz_ctls%fline_ctls, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    viz_ctls%delta_t_ucd_v_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    viz_ctls%i_step_ucd_v_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    viz_ctls%output_field_file_fmt_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level,                &
     &                             viz_ctls%block_name)
!
      end subroutine write_viz_controls
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_vizs()
      num_label_vizs = n_label_vizs
      return
      end function num_label_vizs
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_vizs_w_dep()
      num_label_vizs_w_dep = n_label_vizs_w_dep
      return
      end function num_label_vizs_w_dep
!
! ----------------------------------------------------------------------
!
      subroutine set_label_vizs(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_vizs_w_dep)
!
!
      call set_control_labels(hd_i_step_section,  names( 1))
      call set_control_labels(hd_delta_t_section, names( 2))
      call set_control_labels(hd_section_ctl,     names( 3))
!
      call set_control_labels(hd_i_step_isosurf,  names( 4))
      call set_control_labels(hd_delta_t_isosurf, names( 5))
      call set_control_labels(hd_isosurf_ctl,     names( 6))
!
      call set_control_labels(hd_i_step_map_projection,  names( 7))
      call set_control_labels(hd_delta_t_map_projection, names( 8))
      call set_control_labels(hd_map_rendering,          names( 9))
!
      call set_control_labels(hd_i_step_pvr,       names(10))
      call set_control_labels(hd_delta_t_pvr,      names(11))
      call set_control_labels(hd_pvr_ctl,          names(12))
!
      call set_control_labels(hd_i_step_lic,       names(13))
      call set_control_labels(hd_delta_t_lic,      names(14))
      call set_control_labels(hd_lic_ctl,          names(15))
!
      call set_control_labels(hd_i_step_fline,     names(16))
      call set_control_labels(hd_delta_t_fline,    names(17))
      call set_control_labels(hd_fline_ctl,        names(18))
!
      call set_control_labels(hd_i_step_ucd,          names(19))
      call set_control_labels(hd_delta_t_ucd,         names(20))
      call set_control_labels(hd_output_fld_file_fmt, names(21))
!
      call set_control_labels(hd_viz_partition,       names(22))
!
      call set_control_labels(hd_psf_ctl,             names(23))
      call set_control_labels(hd_iso_ctl,             names(24))
      call set_control_labels(hd_lic_partition,       names(25))
!
      end subroutine set_label_vizs
!
!  ---------------------------------------------------------------------
!
      end module ctl_data_visualiser_IO
