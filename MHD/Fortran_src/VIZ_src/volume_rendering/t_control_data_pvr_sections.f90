!>@file   t_control_data_pvr_sections.f90
!!@brief  module t_control_data_pvr_sections
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      subroutine read_control_pvr_section_def(pvr_sect_ctl)
!!      subroutine read_pvr_section_ctl(hd_pvr_sections, pvr_sect_ctl)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!!
!!      subroutine read_plot_area_ctl(hd_plot_area, i_plot_area,        &
!!     &           pvr_area_ctl, surf_enhanse_ctl)
!!        type(ctl_array_chara), intent(inout) :: pvr_area_ctl
!!        type(ctl_array_c2r), intent(inout) :: surf_enhanse_ctl
!!
!!      subroutine dup_pvr_section_ctl(org_pvr_sect_c, new_pvr_sect_c)
!!        type(pvr_sections_ctl), intent(in) :: org_pvr_sect_c
!!        type(pvr_sections_ctl), intent(inout) :: new_pvr_sect_c
!!
!!      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!!        type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin plot_area_ctl
!!    array chosen_ele_grp_ctl  1
!!      chosen_ele_grp_ctl   outer_core
!!    end array chosen_ele_grp_ctl
!!
!!    array surface_enhanse_ctl  2
!!      surface_enhanse_ctl   ICB   reverse_surface   0.7
!!      surface_enhanse_ctl   CMB   forward_surface   0.4
!!    end array surface_enhanse_ctl
!!  end  plot_area_ctl
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
!!end volume_rendering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_sections
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_read_control_elements
      use t_control_data_4_psf
      use t_control_elements
      use t_control_array_character
      use t_control_array_chara2real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_sections_ctl
        character(len = kchara) :: fname_sect_ctl
        type(psf_ctl) :: psf_c
        type(read_real_item) :: opacity_ctl
      end type pvr_sections_ctl
!
!     4th level for area group
!
      character(len=kchara) :: hd_plot_grp = 'chosen_ele_grp_ctl'
      character(len=kchara) :: hd_sf_enhanse = 'surface_enhanse_ctl'
      character(len=kchara) :: hd_pvr_opacity =   'opacity_ctl'
!
      private :: hd_plot_grp, hd_sf_enhanse, hd_pvr_opacity
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
      if(my_rank .gt. 0) return
      if(pvr_sect_ctl%fname_sect_ctl .eq. 'NO_FILE') return
!
      ctl_file_code = psf_ctl_file_code
      write(*,*) 'Read ',  trim(pvr_sect_ctl%fname_sect_ctl),           &
     &             'for surface definition'
      open(ctl_file_code, file=pvr_sect_ctl%fname_sect_ctl,             &
     &       status='old')
!
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        if(right_begin_flag(hd_surface_define) .gt. 0) then
          call read_section_def_control                                 &
     &       (ctl_file_code, pvr_sect_ctl%psf_c, c_buf1)
          exit
        end if
      end do
!
      close(ctl_file_code)
!
      end subroutine read_control_pvr_section_def
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_section_ctl(hd_pvr_sections, pvr_sect_ctl)
!
      character(len=kchara), intent(in) :: hd_pvr_sections
      type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!
!
      pvr_sect_ctl%psf_c%i_surface_define = 0
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        if(check_end_flag(c_buf1, hd_pvr_sections)) exit
!
!
        if(check_file_flag(c_buf1, hd_surface_define)) then
          pvr_sect_ctl%fname_sect_ctl = third_word(c_buf1)
        end if
        if(check_begin_flag(c_buf1, hd_surface_define)) then
          pvr_sect_ctl%fname_sect_ctl = 'NO_FILE'
          call read_section_def_control                                 &
     &       (ctl_file_code, pvr_sect_ctl%psf_c, c_buf1)
        end if
!
        call read_real_ctl_type                                         &
     &     (c_buf1, hd_pvr_opacity, pvr_sect_ctl%opacity_ctl)
      end do
      pvr_sect_ctl%psf_c%i_surface_define = 1
!
      end subroutine read_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_plot_area_ctl(hd_plot_area, i_plot_area,          &
     &           pvr_area_ctl, surf_enhanse_ctl)
!
      character(len=kchara), intent(in) :: hd_plot_area
      integer(kind=kint), intent(inout) :: i_plot_area
      type(ctl_array_chara), intent(inout) :: pvr_area_ctl
      type(ctl_array_c2r), intent(inout) :: surf_enhanse_ctl
!
!
      if(check_begin_flag(c_buf1, hd_plot_area) .eqv. .FALSE.) return
      if (i_plot_area.gt.0) return
      do
        call load_one_line_from_control(ctl_file_code, c_buf1)
        if(check_end_flag(c_buf1, hd_plot_area)) exit
!
!
        call read_control_array_c1(ctl_file_code,                       &
     &      hd_plot_grp, pvr_area_ctl, c_buf1)
        call read_control_array_c2_r(ctl_file_code,                     &
     &      hd_sf_enhanse, surf_enhanse_ctl, c_buf1)
      end do
      i_plot_area = 1
!
      end subroutine read_plot_area_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_section_ctl(org_pvr_sect_c, new_pvr_sect_c)
!
      use copy_control_elements
!
      type(pvr_sections_ctl), intent(in) :: org_pvr_sect_c
      type(pvr_sections_ctl), intent(inout) :: new_pvr_sect_c
!
!
      if(org_pvr_sect_c%fname_sect_ctl .eq. 'NO_FILE') then
        call dup_control_4_psf                                          &
     &     (org_pvr_sect_c%psf_c, new_pvr_sect_c%psf_c)
      end if
      new_pvr_sect_c%fname_sect_ctl = org_pvr_sect_c%fname_sect_ctl
!
      call copy_real_ctl(org_pvr_sect_c%opacity_ctl,                    &
     &                   new_pvr_sect_c%opacity_ctl)
!
      end subroutine dup_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_section_ctl(pvr_sect_ctl)
!
      type(pvr_sections_ctl), intent(inout) :: pvr_sect_ctl
!
!
      call dealloc_cont_dat_4_psf(pvr_sect_ctl%psf_c)
      pvr_sect_ctl%opacity_ctl%iflag =    0
!
      end subroutine dealloc_pvr_section_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_sections
