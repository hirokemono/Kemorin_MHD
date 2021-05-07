!>@file   t_ctl_data_pvr_isosurface.f90
!!@brief  module t_ctl_data_pvr_isosurface
!!
!!@author H. Matsui
!!@date Programmed in 2006
!!@date Modified in May, 2021
!!
!> @brief control data for isosurface in parallel volume rendering
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!      subroutine read_pvr_isosurface_ctl                              &
!!     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!!      subroutine bcast_pvr_isosurf_ctl(pvr_iso_ctl)
!!      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!!        type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
!!        type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!!
!!      integer(kind = kint) function num_label_pvr_isosurface()
!!      subroutine set_label_pvr_isosurface(names)
!!        character(len = kchara), intent(inout)                        &
!!     &                         :: names(n_label_pvr_isosurface)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin isosurface_ctl
!!      isosurf_field         magnetic_field
!!      isosurf_component     amplitude
!!      isosurf_value         0.3
!!      opacity_ctl           0.9
!!      surface_direction     normal
!!    end isosurface_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_pvr_isosurface
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use skip_comment_f
!
      implicit  none
!
!
      type pvr_isosurf_ctl
!>        Structure for field name for isosurface
        type(read_character_item) :: isosurf_data_ctl
!>        Structure for component name for isosurface
        type(read_character_item) :: isosurf_comp_ctl
!
!>        Structure for direction of isosurface
        type(read_character_item) :: isosurf_type_ctl
!>        Structure for value of isosurface
        type(read_real_item) :: iso_value_ctl
!>        Structure for opacity of isosurface
        type(read_real_item) :: opacity_ctl
      end type pvr_isosurf_ctl
!
!     3rd level for isosurface
!
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_field =  'isosurf_field'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_comp =   'isosurf_component'
      character(len=kchara), parameter, private                         &
     &             :: hd_isosurf_value = 'isosurf_value'
      character(len=kchara), parameter, private                         &
     &             :: hd_pvr_opacity =   'opacity_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_iso_direction = 'surface_direction'
!
      integer(kind = kint), parameter :: n_label_pvr_isosurface =   5
!
      private :: n_label_pvr_isosurface
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_isosurface_ctl                                &
     &         (id_control, hd_block, pvr_iso_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type(c_buf, hd_iso_field,                   &
     &      pvr_iso_ctl%isosurf_data_ctl)
        call read_chara_ctl_type(c_buf, hd_iso_comp,                    &
     &      pvr_iso_ctl%isosurf_comp_ctl)
        call read_chara_ctl_type(c_buf, hd_iso_direction,               &
     &      pvr_iso_ctl%isosurf_type_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_isosurf_value, pvr_iso_ctl%iso_value_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_pvr_opacity, pvr_iso_ctl%opacity_ctl)
      end do
!
      end subroutine read_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_isosurf_ctl(pvr_iso_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_data_ctl)
      call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_comp_ctl)
      call bcast_ctl_type_c1(pvr_iso_ctl%isosurf_type_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%iso_value_ctl)
      call bcast_ctl_type_r1(pvr_iso_ctl%opacity_ctl)
!
      end subroutine bcast_pvr_isosurf_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_isosurface_ctl(pvr_iso_ctl)
!
      type(pvr_isosurf_ctl), intent(inout) :: pvr_iso_ctl
!
!
      pvr_iso_ctl%isosurf_data_ctl%iflag =  0
      pvr_iso_ctl%isosurf_comp_ctl%iflag =  0
      pvr_iso_ctl%isosurf_type_ctl%iflag =  0
      pvr_iso_ctl%iso_value_ctl%iflag =     0
      pvr_iso_ctl%opacity_ctl%iflag =       0
!
      end subroutine reset_pvr_isosurface_ctl
!
! ----------------------------------------------------------------------
!
      subroutine dup_pvr_isosurface_ctl(org_pvr_iso_c, new_pvr_iso_c)
!
      type(pvr_isosurf_ctl), intent(in) :: org_pvr_iso_c
      type(pvr_isosurf_ctl), intent(inout) :: new_pvr_iso_c
!
!
      call copy_chara_ctl(org_pvr_iso_c%isosurf_data_ctl,               &
     &                    new_pvr_iso_c%isosurf_data_ctl)
      call copy_chara_ctl(org_pvr_iso_c%isosurf_comp_ctl,               &
     &                    new_pvr_iso_c%isosurf_comp_ctl)
      call copy_chara_ctl(org_pvr_iso_c%isosurf_type_ctl,               &
     &                    new_pvr_iso_c%isosurf_type_ctl)
      call copy_real_ctl(org_pvr_iso_c%iso_value_ctl,                   &
     &                   new_pvr_iso_c%iso_value_ctl)
      call copy_real_ctl(org_pvr_iso_c%opacity_ctl,                     &
     &                   new_pvr_iso_c%opacity_ctl)
!
      end subroutine dup_pvr_isosurface_ctl
!
!  ---------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      integer(kind = kint) function num_label_pvr_isosurface()
      num_label_pvr_isosurface = n_label_pvr_isosurface
      return
      end function num_label_pvr_isosurface
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_pvr_isosurface(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_pvr_isosurface)
!
!
      call set_control_labels(hd_iso_field,     names( 1))
      call set_control_labels(hd_iso_comp,      names( 2))
      call set_control_labels(hd_isosurf_value, names( 3))
      call set_control_labels(hd_pvr_opacity,   names( 4))
      call set_control_labels(hd_iso_direction, names( 5))
!
      end subroutine set_label_pvr_isosurface
!
! ----------------------------------------------------------------------
!
      end module t_ctl_data_pvr_isosurface
