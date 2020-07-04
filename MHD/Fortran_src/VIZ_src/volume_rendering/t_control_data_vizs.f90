!>@file   t_control_data_vizs.f90
!!@brief  module t_control_data_vizs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine read_viz_controls(id_control, viz_ctls, c_buf)
!!      subroutine bcast_viz_controls(viz_ctls)
!!      subroutine dealloc_viz_controls(viz_ctls)
!!       type(visualization_controls), intent(inout) :: viz_ctls
!!       type(buffer_for_control), intent(inout)  :: c_buf
!!
!!      integer(kind = kint) function num_label_vizs()
!!      integer(kind = kint) function num_label_vizs_w_dep()
!!      subroutine set_label_vizs(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array  cross_section_ctl  1
!!      ....
!!    end array cross_section_ctl
!!
!!    array  isosurface_ctl  1
!!      ....
!!    end array isosurface_ctl
!!
!!    array  volume_rendering  1
!!      ....
!!    end array volume_rendering
!!
!!    array  fieldline  1
!!      ....
!!    end array fieldline
!!
!!    array  LIC_rendering  1
!!      ....
!!    end array LIC_rendering
!!
!!    delta_t_sectioning_ctl   1.0e-3
!!    i_step_sectioning_ctl    400
!!    delta_t_isosurface_ctl   1.0e-3
!!    i_step_isosurface_ctl    400
!!    delta_t_pvr_ctl          1.0e-2
!!    i_step_pvr_ctl           400
!!    delta_t_fline_ctl        1.0e-1
!!    i_step_fline_ctl         400
!!    delta_t_LIC_ctl          1.0e-1
!!    i_step_LIC_ctl           400
!!    delta_t_field_ctl        1.0e-3
!!    i_step_field_ctl         800
!!    output_field_file_fmt_ctl   'VTK'
!!  end visual_control
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
!
      module t_control_data_vizs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_sections
      use t_control_data_isosurfaces
      use t_control_data_pvrs
      use t_control_data_flines
      use t_control_data_LIC_pvrs
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!>        Structures of visualization controls
      type visualization_controls
!>        Structures of setioning controls
        type(section_controls) :: psf_ctls
!>        Structures of isosurface controls
        type(isosurf_controls) :: iso_ctls
!>        Structures of volume rendering controls
        type(volume_rendering_controls) :: pvr_ctls
!>        Structures of fieldline controls
        type(fieldline_controls) :: fline_ctls
!>        Structures of LIC rendering controls
        type(lic_rendering_controls) :: lic_ctls
!
!>   Increment for sectioning
        type(read_integer_item) :: i_step_psf_v_ctl
!>   Increment for isosurface
        type(read_integer_item) :: i_step_iso_v_ctl
!>   Increment for volume rendering
        type(read_integer_item) :: i_step_pvr_v_ctl
!>   Increment for LIC rendering
        type(read_integer_item) :: i_step_lic_v_ctl
!>   Increment for field line
        type(read_integer_item) :: i_step_fline_v_ctl
!>   Increment for field data output
        type(read_integer_item) :: i_step_ucd_v_ctl
!
!>   time interval for sectioning
        type(read_real_item) :: delta_t_psf_v_ctl
!>   time interval for isosurface
        type(read_real_item) :: delta_t_iso_v_ctl
!>   time interval for volume rendering
        type(read_real_item) :: delta_t_pvr_v_ctl
!>   time interval for LIC rendering
        type(read_real_item) :: delta_t_lic_v_ctl
!>   time interval for field line
        type(read_real_item) :: delta_t_fline_v_ctl
!>   time interval for field data output
        type(read_real_item) :: delta_t_ucd_v_ctl
!
!>   File format for field data output
        type(read_character_item) :: output_field_file_fmt_ctl
!
        integer (kind=kint) :: i_viz_control = 0
      end type visualization_controls
!
!     Top level
      character(len=kchara), parameter, private                         &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_lic_ctl = 'LIC_rendering'
      character(len=kchara), parameter :: hd_fline_ctl =  'fieldline'
!
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_section =   'i_step_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_isosurf =   'i_step_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_pvr =       'i_step_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_lic =       'i_step_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_fline =     'i_step_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_i_step_ucd =       'i_step_field_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_section =   'delta_t_sectioning_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_isosurf =   'delta_t_isosurface_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_pvr =       'delta_t_pvr_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_lic =       'delta_t_LIC_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_fline =     'delta_t_fline_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_delta_t_ucd =       'delta_t_field_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_output_fld_file_fmt = 'output_field_file_fmt_ctl'
!
!
!      Deprecated labels
      character(len=kchara), parameter, private                         &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_ctl = 'isosurf_rendering'
!
      integer(kind = kint), parameter, private                          &
     &                      :: n_label_vizs = 18
      integer(kind = kint), parameter, private                          &
     &                      :: n_label_vizs_w_dep = 20
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_viz_controls                                      &
     &         (id_control, hd_block, viz_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control 
      character(len=kchara), intent(in) :: hd_block
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(viz_ctls%i_viz_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        if(check_array_flag(c_buf, hd_psf_ctl)) then
          call read_files_4_psf_ctl(id_control, hd_psf_ctl,             &
     &        viz_ctls%psf_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_section_ctl)) then
          call read_files_4_psf_ctl(id_control, hd_section_ctl,         &
     &        viz_ctls%psf_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_iso_ctl)) then
          call read_files_4_iso_ctl(id_control, hd_iso_ctl,             &
     &        viz_ctls%iso_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_isosurf_ctl)) then
          call read_files_4_iso_ctl(id_control, hd_isosurf_ctl,         &
     &        viz_ctls%iso_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_pvr_ctl)) then
          call read_files_4_pvr_ctl(id_control, hd_pvr_ctl,             &
     &        viz_ctls%pvr_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_fline_ctl)) then
          call read_files_4_fline_ctl(id_control, hd_fline_ctl,         &
     &        viz_ctls%fline_ctls, c_buf)
        end if
!
        if(check_array_flag(c_buf, hd_lic_ctl)) then
          call read_files_4_lic_ctl(id_control, hd_lic_ctl,             &
     &        viz_ctls%lic_ctls, c_buf)
        end if
!
        call read_integer_ctl_type(c_buf, hd_i_step_section,            &
     &      viz_ctls%i_step_psf_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_isosurf,            &
     &      viz_ctls%i_step_iso_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_pvr,                &
     &      viz_ctls%i_step_pvr_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_lic,                &
     &      viz_ctls%i_step_lic_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_fline,              &
     &      viz_ctls%i_step_fline_v_ctl)
        call read_integer_ctl_type(c_buf, hd_i_step_ucd,                &
     &      viz_ctls%i_step_ucd_v_ctl)
!
        call read_real_ctl_type(c_buf, hd_delta_t_section,              &
     &      viz_ctls%delta_t_psf_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_isosurf,              &
     &      viz_ctls%delta_t_iso_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_pvr,                  &
     &      viz_ctls%delta_t_pvr_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_fline,                &
     &      viz_ctls%delta_t_fline_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_lic,                  &
     &      viz_ctls%delta_t_lic_v_ctl)
        call read_real_ctl_type(c_buf, hd_delta_t_ucd,                  &
     &      viz_ctls%delta_t_ucd_v_ctl)
!
        call read_chara_ctl_type(c_buf, hd_output_fld_file_fmt,         &
     &      viz_ctls%output_field_file_fmt_ctl)
      end do
      viz_ctls%i_viz_control = 1
!
      end subroutine read_viz_controls
!
!   --------------------------------------------------------------------
!
      subroutine bcast_viz_controls(viz_ctls)
!
      use bcast_control_arrays
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call bcast_files_4_psf_ctl(viz_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz_ctls%iso_ctls)
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_ctls)
      call bcast_files_4_fline_ctl(viz_ctls%fline_ctls)
      call bcast_files_4_lic_ctl(viz_ctls%lic_ctls)
!
      call bcast_ctl_type_r1(viz_ctls%delta_t_psf_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_iso_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_pvr_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_fline_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_lic_v_ctl)
      call bcast_ctl_type_r1(viz_ctls%delta_t_ucd_v_ctl)
!
      call bcast_ctl_type_i1(viz_ctls%i_step_psf_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_iso_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_pvr_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_lic_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_fline_v_ctl)
      call bcast_ctl_type_i1(viz_ctls%i_step_ucd_v_ctl)
!
      call bcast_ctl_type_c1(viz_ctls%output_field_file_fmt_ctl)
!
      call MPI_BCAST(viz_ctls%i_viz_control, 1,                         &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_viz_controls
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_viz_controls(viz_ctls)
!
      type(visualization_controls), intent(inout) :: viz_ctls
!
!
      call dealloc_psf_ctl_stract(viz_ctls%psf_ctls)
      call dealloc_iso_ctl_stract(viz_ctls%iso_ctls)
      call dealloc_pvr_ctl_struct(viz_ctls%pvr_ctls)
      call dealloc_fline_fhead_ctl(viz_ctls%fline_ctls)
      call dealloc_lic_ctl_struct(viz_ctls%lic_ctls)
!
      viz_ctls%delta_t_psf_v_ctl%iflag =   0
      viz_ctls%delta_t_iso_v_ctl%iflag =   0
      viz_ctls%delta_t_pvr_v_ctl%iflag =   0
      viz_ctls%delta_t_fline_v_ctl%iflag = 0
      viz_ctls%delta_t_lic_v_ctl%iflag =   0
      viz_ctls%delta_t_ucd_v_ctl%iflag =   0
!
      viz_ctls%i_step_psf_v_ctl%iflag =   0
      viz_ctls%i_step_iso_v_ctl%iflag =   0
      viz_ctls%i_step_pvr_v_ctl%iflag =   0
      viz_ctls%i_step_lic_v_ctl%iflag =   0
      viz_ctls%i_step_fline_v_ctl%iflag = 0
      viz_ctls%i_step_ucd_v_ctl%iflag =   0
!
      viz_ctls%i_viz_control = 0
!
      end subroutine dealloc_viz_controls
!
!   --------------------------------------------------------------------
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
      call set_control_labels(hd_i_step_pvr,          names( 7))
      call set_control_labels(hd_delta_t_pvr,         names( 8))
      call set_control_labels(hd_output_fld_file_fmt, names( 9))
!
      call set_control_labels(hd_i_step_lic,          names(10))
      call set_control_labels(hd_delta_t_lic,         names(11))
      call set_control_labels(hd_output_fld_file_fmt, names(12))
!
      call set_control_labels(hd_i_step_fline,        names(13))
      call set_control_labels(hd_delta_t_fline,       names(14))
      call set_control_labels(hd_output_fld_file_fmt, names(15))
!
      call set_control_labels(hd_i_step_ucd,          names(16))
      call set_control_labels(hd_delta_t_ucd,         names(17))
      call set_control_labels(hd_output_fld_file_fmt, names(18))
!
      call set_control_labels(hd_psf_ctl,         names(19))
      call set_control_labels(hd_iso_ctl,         names(20))
!
      end subroutine set_label_vizs
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_vizs
