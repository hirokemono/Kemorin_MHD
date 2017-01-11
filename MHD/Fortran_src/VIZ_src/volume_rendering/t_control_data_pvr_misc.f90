!t_control_data_pvr_misc.f90
!      module t_control_data_pvr_misc
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine read_control_pvr_section_def(pvr_sect_ctl)
!!      subroutine read_pvr_section_ctl(pvr_sect_ctl)
!!      subroutine bcast_pvr_section_ctl(pvr_sect_ctl)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!!
!!      subroutine read_pvr_isosurface_ctl(pvr_iso_ctl)
!!      subroutine bcast_pvr_isosurface_ctl(pvr_iso_ctl)
!!        type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!!
!!      subroutine read_pvr_colorbar_ctl(colorbar)
!!      subroutine bcast_pvr_colorbar_ctl(colorbar)
!!        type(pvr_colorbar_ctl), intent(inout) :: colorbar
!!
!!      subroutine read_pvr_rotation_ctl(movie)
!!      subroutine bcast_pvr_rotation_ctl(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      subroutine reset_pvr_misc_control_flags(colorbar, movie)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin colorbar_ctl
!!    colorbar_switch_ctl    ON
!!    colorbar_scale_ctl     ON
!!    iflag_zeromarker       ON
!!    colorbar_range     0.0   1.0
!!    font_size_ctl         3
!!    num_grid_ctl     4
!!
!!    axis_label_switch      ON
!!  end colorbar_ctl
!!
!!  begin image_rotation_ctl
!!    rotation_axis_ctl       z
!!    rotation_axis_ctl       1
!!  end image_rotation_ctl
!!!
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_pvr_misc
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_data_4_psf
      use t_control_elements
      use t_read_control_arrays
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_sections_ctl
        character(len = kchara) :: fname_sect_ctl
        type(psf_ctl) :: psf
        type(read_real_item) :: opacity_ctl
      end type pvr_sections_ctl
!
      type pvr_isosurf_ctl
        type(read_character_item) :: isosurf_type_ctl
        type(read_real_item) :: isosurf_value_ctl
        type(read_real_item) :: opacity_ctl
      end type pvr_isosurf_ctl
!
!
      type pvr_colorbar_ctl
        type(read_character_item) :: colorbar_switch_ctl
        type(read_character_item) :: colorbar_scale_ctl
        type(read_character_item) :: zeromarker_flag_ctl
        type(read_integer_item) ::   font_size_ctl
        type(read_integer_item) ::   ngrid_cbar_ctl
        type(read_real2_item) ::     cbar_range_ctl
!
        type(read_character_item) :: axis_switch_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_colorbar = 0
      end type pvr_colorbar_ctl
!
!
      type pvr_movie_ctl
        type(read_character_item) :: rotation_axis_ctl
        type(read_integer_item) ::   num_frames_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_pvr_rotation = 0
      end type pvr_movie_ctl
!
!     2nd level for volume_rendering
!
      character(len=kchara) :: hd_pvr_sections = 'section_ctl'
      character(len=kchara) :: hd_pvr_isosurf =  'isosurface_ctl'
      character(len=kchara) :: hd_pvr_colorbar =  'colorbar_ctl'
      character(len=kchara) :: hd_pvr_rotation =  'image_rotation_ctl'
!
!     3rd level for isosurface
!
      character(len=kchara) :: hd_isosurf_value = 'isosurf_value'
      character(len=kchara) :: hd_pvr_opacity =   'opacity_ctl'
      character(len=kchara) :: hd_iso_direction = 'surface_direction'
!
!     3rd level for colorbar
!
      character(len=kchara)                                             &
     &                    :: hd_colorbar_switch = 'colorbar_switch_ctl'
      character(len=kchara) :: hd_colorbar_scale = 'colorbar_scale_ctl'
      character(len=kchara) :: hd_pvr_font_size = 'font_size_ctl'
      character(len=kchara) :: hd_pvr_numgrid_cbar = 'num_grid_ctl'
      character(len=kchara) :: hd_zeromarker_flag = 'iflag_zeromarker'
      character(len=kchara) :: hd_cbar_range = 'colorbar_range'
!
      character(len=kchara)                                             &
     &                    :: hd_axis_switch = 'axis_label_switch'
!     3rd level for rotation
!
      character(len=kchara) :: hd_movie_rot_axis =  'rotation_axis_ctl'
      character(len=kchara) :: hd_movie_rot_frame = 'num_frames_ctl'
!
      private :: hd_pvr_colorbar, hd_pvr_rotation, hd_isosurf_value
      private :: hd_colorbar_switch, hd_pvr_opacity, hd_iso_direction
      private :: hd_pvr_numgrid_cbar, hd_zeromarker_flag
      private :: hd_colorbar_scale, hd_pvr_font_size, hd_cbar_range
      private :: hd_movie_rot_axis, hd_movie_rot_frame
      private :: hd_axis_switch
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_pvr_section_def(pvr_sect_ctl)
!
      use m_read_control_elements
!
      type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
      integer(kind = kint), parameter :: psf_ctl_file_code = 11
!
!
      if(pvr_sect_ctl%fname_sect_ctl .eq. 'NO_FILE') return
      if(my_rank .eq. 0) then
!
        ctl_file_code = psf_ctl_file_code
        write(*,*) 'Read ',  trim(pvr_sect_ctl%fname_sect_ctl),         &
     &             'for surface definition'
        open(ctl_file_code, file=pvr_sect_ctl%fname_sect_ctl,           &
     &       status='old')
!
        do
          call load_ctl_label_and_line
          if(right_begin_flag(hd_surface_define) .gt. 0) then
            call read_section_def_control(pvr_sect_ctl%psf)
            exit
          end if
        end do
!
        close(ctl_file_code)
      end if
!
      call bcast_section_def_control(pvr_sect_ctl%psf)
!
      end subroutine read_control_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_section_ctl(pvr_sect_ctl)
!
      type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
      integer(kind = kint) :: i_flag, i_psf_ctl1
!
!
      i_psf_ctl1 = 0
      i_flag = 0
      pvr_sect_ctl%psf%i_surface_define = 0
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_sections, i_flag)
        if(i_flag .gt. 0) exit
!
        if(right_file_flag(hd_surface_define) .gt. 0) then
          call read_file_name_from_ctl_line(i_psf_ctl1,                 &
     &        pvr_sect_ctl%fname_sect_ctl)
        else if(right_begin_flag(hd_surface_define) .gt. 0) then
          i_psf_ctl1 = i_psf_ctl1 + 1
          pvr_sect_ctl%fname_sect_ctl = 'NO_FILE'
          call read_section_def_control(pvr_sect_ctl%psf)
        end if
!
        call read_real_ctl_type                                         &
     &     (hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
      end do
!
      end subroutine read_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurface_ctl(pvr_iso_ctl)
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      integer(kind = kint) :: i_flag
!
!
      i_flag = 0
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_pvr_isosurf, i_flag)
        if(i_flag .gt. 0) exit
!
        call read_chara_ctl_type(hd_iso_direction,                      &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call read_real_ctl_type                                         &
     &     (hd_isosurf_value, pvr_iso_ctl%isosurf_value_ctl )
        call read_real_ctl_type                                         &
     &     (hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
      end do
!
      end subroutine read_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_colorbar_ctl(colorbar)
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
!
!
      if(right_begin_flag(hd_pvr_colorbar) .eq. 0) return
      if (colorbar%i_pvr_colorbar.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_colorbar, colorbar%i_pvr_colorbar)
        if(colorbar%i_pvr_colorbar .gt. 0) exit
!
!
        call read_integer_ctl_type                                      &
     &     (hd_pvr_font_size, colorbar%font_size_ctl)
        call read_integer_ctl_type(hd_pvr_numgrid_cbar,                 &
     &      colorbar%ngrid_cbar_ctl)
!
!
        call read_chara_ctl_type(hd_colorbar_switch,                    &
     &      colorbar%colorbar_switch_ctl)
        call read_chara_ctl_type(hd_colorbar_scale,                     &
     &      colorbar%colorbar_scale_ctl)
        call read_chara_ctl_type(hd_zeromarker_flag,                    &
     &      colorbar%zeromarker_flag_ctl)
!
        call read_chara_ctl_type(hd_axis_switch,                        &
     &      colorbar%axis_switch_ctl)
!!
        call read_real2_ctl_type                                        &
     &     (hd_cbar_range, colorbar%cbar_range_ctl)
      end do
!
      end subroutine read_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl(movie)
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      if(right_begin_flag(hd_pvr_rotation) .eq. 0) return
      if (movie%i_pvr_rotation.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag                                      &
     &     (hd_pvr_rotation, movie%i_pvr_rotation)
        if(movie%i_pvr_rotation .gt. 0) exit
!
        call read_integer_ctl_type(hd_movie_rot_frame,                  &
     &      movie%num_frames_ctl)
        call read_chara_ctl_type(hd_movie_rot_axis,                     &
     &      movie%rotation_axis_ctl)
      end do
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_section_ctl(pvr_sect_ctl)
!
      use bcast_control_arrays
!
      type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!
!
      call MPI_BCAST(pvr_sect_ctl%fname_sect_ctl, kchara,               &
     &               CALYPSO_CHARACTER, izero, CALYPSO_COMM, ierr_MPI)
      call MPI_BCAST(pvr_sect_ctl%psf%i_psf_ctl,  ione,                 &
     &               CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
      if(pvr_sect_ctl%fname_sect_ctl .eq. 'NO_FILE') then
        call bcast_section_def_control(pvr_sect_ctl%psf)
      end if
!
      call bcast_ctl_type_r1(pvr_sect_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurface_ctl(pvr_iso_ctl)
!
      use bcast_control_arrays
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_type_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%isosurf_value_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_colorbar_ctl(colorbar)
!
      use bcast_control_arrays
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
!
!
      call MPI_BCAST(colorbar%i_pvr_colorbar,  ione,                    &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(colorbar%font_size_ctl)
      call bcast_ctl_type_i1(colorbar%ngrid_cbar_ctl)
!
      call bcast_ctl_type_c1(colorbar%colorbar_switch_ctl)
      call bcast_ctl_type_c1(colorbar%colorbar_scale_ctl)
      call bcast_ctl_type_c1(colorbar%zeromarker_flag_ctl)
!
      call bcast_ctl_type_c1(colorbar%axis_switch_ctl)
!!
      call bcast_ctl_type_r2(colorbar%cbar_range_ctl)
!
      end subroutine bcast_pvr_colorbar_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_rotation_ctl(movie)
!
      use bcast_control_arrays
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call MPI_BCAST(movie%i_pvr_rotation,  ione,                       &
     &              CALYPSO_INTEGER, izero, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(movie%num_frames_ctl)
      call bcast_ctl_type_c1(movie%rotation_axis_ctl)
!
      end subroutine bcast_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_misc_control_flags(colorbar, movie)
!
      type(pvr_colorbar_ctl), intent(inout) :: colorbar
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      movie%num_frames_ctl%iflag =    0
      movie%rotation_axis_ctl%iflag = 0
!
      colorbar%colorbar_switch_ctl%iflag = 0
      colorbar%colorbar_scale_ctl%iflag =  0
      colorbar%font_size_ctl%iflag =       0
      colorbar%ngrid_cbar_ctl%iflag =      0
      colorbar%zeromarker_flag_ctl%iflag = 0
      colorbar%cbar_range_ctl%iflag =      0
!
      movie%i_pvr_rotation = 0
      colorbar%i_pvr_colorbar = 0
!
      end subroutine reset_pvr_misc_control_flags
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_misc
