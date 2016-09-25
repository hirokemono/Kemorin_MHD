!t_control_data_pvr_sect.f90
!      module t_control_data_pvr_sect
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_files_pvr_section_ctl(sect)
!!      subroutine read_files_pvr_isosurf_ctl(iso)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  array section_ctl   1
!!    begin section_ctl
!!      opacity_ctl       0.5
!!      file surface_define    ctl_z0.3
!!    end section_ctl
!!  end array section_ctl
!
!!  array isosurface_ctl   2
!!    begin isosurface_ctl
!!      opacity_ctl       0.5
!!      isosurf_value     0.2
!!      surface_direction    increase
!!    end isosurface_ctl
!!
!!    begin isosurface_ctl
!!      opacity_ctl       0.5
!!      isosurf_value     0.6
!!      surface_direction    decrease
!!    end isosurface_ctl
!!  end array isosurface_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module t_control_data_pvr_sect
!
      use m_precision
!
      use m_machine_parameter
      use m_control_data_4_psf
!
      use m_read_control_elements
      use skip_comment_f
!
!
      implicit  none
!
!
!>        Structure of sections in PVR
      type section_in_pvr_ctl
!>        Opacity of isosurface in PVR
        type(read_real_item) :: sect_opacity_ctl
!>        Structure of sections in PVR
        type(psf_ctl) ::        section_ctl
!>        Isosurface direction in PVR
        type(read_character_item) :: fname_sect_ctl
      end type section_in_pvr_ctl
!
!>        Structure of isosurface in PVR
      type isosurf_in_pvr_ctl
!>        Value for isosurface in PVR
        type(read_real_item) :: iso_value_ctl
!>        Opacity of isosurface in PVR
        type(read_real_item) :: iso_opacity_ctl
!>        Isosurface direction in PVR
        type(read_character_item) :: iso_direction_ctl
      end type isosurf_in_pvr_ctl
!
!
!   entry label
!
      character(len=kchara) :: hd_section_def = 'surface_define'
      character(len=kchara) :: hd_opacity =     'opacity_ctl'
      character(len=kchara) :: hd_iso_value =   'isosurf_value'
      character(len=kchara) :: hd_direction =   'surface_direction'
!
      private :: hd_section_def, hd_opacity, hd_iso_value, hd_direction
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_files_pvr_section_ctl(sect)
!
      type(section_in_pvr_ctl), intent(inout) :: sect
      integer (kind=kint) :: i_psf_ctl1 = 0
!
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_section_def, i_psf_ctl1)
        if(i_psf_ctl1 .gt. 0) exit
!
        if(right_file_flag(hd_section_def) .gt. 0) then
          call read_file_name_from_ctl_line                             &
     &       (i_psf_ctl1, sect%fname_sect_ctl%charavalue)
        else if(right_begin_flag(hd_section_def) .gt. 0) then
          i_psf_ctl1 = i_psf_ctl1 + 1
          sect%fname_sect_ctl%charavalue = 'NO_FILE'
          call read_psf_control_data(sect%section_ctl)
        end if
!
        call read_real_ctl_type(hd_opacity, sect%sect_opacity_ctl)
      end do
!
      end subroutine read_files_pvr_section_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_files_pvr_isosurf_ctl(iso)
!
      type(isosurf_in_pvr_ctl), intent(inout) :: iso
      integer (kind=kint) :: i_iso_ctl1 = 0
!
!
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_psf_ctl, i_iso_ctl1)
        if(i_iso_ctl1 .gt. 0) exit
!
        call read_real_ctl_type(hd_opacity, iso%iso_opacity_ctl)
        call read_real_ctl_type(hd_iso_value, iso%iso_value_ctl)
        call read_chara_ctl_type(hd_direction, iso%iso_direction_ctl)
      end do
!
      end subroutine read_files_pvr_isosurf_ctl
!
!   --------------------------------------------------------------------
!
      end module t_control_data_pvr_sect
