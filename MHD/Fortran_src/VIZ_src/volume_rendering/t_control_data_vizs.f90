!>@file   t_control_data_vizs.f90
!!@brief  module t_control_data_vizs
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for visualization controls
!!
!!@verbatim
!!      subroutine read_viz_controls(viz_ctls)
!!      subroutine bcast_viz_controls(viz_ctls)
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin visual_control
!!    array  cross_section_ctl  1
!!      ....
!!    end array cross_section_ctl
!!    array  isosurface_ctl  1
!!      ....
!!    end array isosurface_ctl
!!    array  volume_rendering  1
!!      ....
!!    end array volume_rendering
!!    array  fieldline  1
!!      ....
!!    end array fieldline
!!    array  LIC_rendering  1
!!      ....
!!    end array LIC_rendering
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
!
        integer (kind=kint) :: i_viz_control = 0
      end type visualization_controls
!
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
!
!     lavel for volume rendering
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_lic_ctl = 'LIC_rendering'
      character(len=kchara), parameter :: hd_fline_ctl =  'fieldline'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
!
      private :: hd_section_ctl, hd_psf_ctl, hd_lic_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
      private :: hd_viz_control
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_viz_controls(id_control, viz_ctls, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
!
      type(visualization_controls), intent(inout) :: viz_ctls
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_viz_control) .eqv. .FALSE.) return
      if(viz_ctls%i_viz_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_viz_control)) exit
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
      end do
      viz_ctls%i_viz_control = 1
!
      end subroutine read_viz_controls
!
!   --------------------------------------------------------------------
!
      subroutine bcast_viz_controls(viz_ctls)
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
      end subroutine bcast_viz_controls
!
!   --------------------------------------------------------------------
!
      end module t_control_data_vizs
