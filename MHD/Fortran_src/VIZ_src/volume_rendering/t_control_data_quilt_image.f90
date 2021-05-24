!>@file   t_control_data_quilt_image.f90
!!@brief  module t_control_data_quilt_image
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR quilt_c from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      !!      subroutine read_quilt_image_ctl                                 &
!!     &         (id_control, hd_block, quilt_c, c_buf)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      subroutine dup_quilt_image_ctl(org_movie, new_movie)
!!        type(quilt_image_ctl), intent(in) :: org_movie
!!        type(quilt_image_ctl), intent(inout) :: new_movie
!!      subroutine reset_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      subroutine bcast_quilt_image_ctl(quilt_c)
!!        type(quilt_image_ctl), intent(inout) :: quilt_c
!!
!!      integer(kind = kint) function num_label_quilt_image()
!!      subroutine set_label_quilt_image(names)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    Avaiable parameters for quilt_mode_ctl: BITMAP
!!
!!  begin image_rotation_ctl
!!    quilt_mode_ctl           BITMAP
!!    num_frames_ctl           45
!!    num_row_column_ctl       5     9
!!    angle_range            -17.5  17.5
!!  end image_rotation_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    quilt_mode_ctl:   view_matrices, rotation, apature, LIC_kernel
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_quilt_image
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_integer2
      use t_control_array_real2
      use skip_comment_f
!
      implicit  none
!
!
      type quilt_image_ctl
!>        Structure of quilt_c mode control
        type(read_character_item) :: quilt_mode_ctl
!>        Structure of number of flame control
        type(read_integer_item) ::   num_frames_ctl
!
!>        Structure of start and end of angle
        type(read_real2_item) :: angle_range_ctl
!>        Structure of number of row and columns of image
        type(read_int2_item) :: num_row_column_ctl
!
!     2nd level for volume rendering
        integer (kind=kint) :: i_quilt_image = 0
      end type quilt_image_ctl
!
!     3rd level for rotation
!
      character(len=kchara), parameter, private                         &
     &             :: hd_quilt_mode =  'quilt_mode_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_quilt_num_frame = 'num_frames_ctl'
!
      character(len=kchara), parameter, private                         &
     &             :: hd_row_column =    'num_row_column_ctl'
      character(len=kchara), parameter, private                         &
     &             :: hd_angle_range =   'angle_range'
!
      integer(kind = kint), parameter :: n_label_quilt_ctl =   4
      private :: n_label_quilt_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_quilt_image_ctl                                   &
     &         (id_control, hd_block, quilt_c, c_buf)
!
      use read_control_pvr_modelview
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(quilt_image_ctl), intent(inout) :: quilt_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (quilt_c%i_quilt_image.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_quilt_mode,                  &
     &      quilt_c%quilt_mode_ctl)
!
        call read_integer_ctl_type(c_buf, hd_quilt_num_frame,           &
     &      quilt_c%num_frames_ctl)
!
        call read_real2_ctl_type(c_buf, hd_angle_range,                 &
     &      quilt_c%angle_range_ctl)
        call read_int2_ctl_type(c_buf, hd_row_column,                   &
     &      quilt_c%num_row_column_ctl)
      end do
      quilt_c%i_quilt_image = 1
!
      end subroutine read_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_quilt_image_ctl(org_movie, new_movie)
!
      use bcast_dup_view_transfer_ctl
!
      type(quilt_image_ctl), intent(in) :: org_movie
      type(quilt_image_ctl), intent(inout) :: new_movie
!
!
      call copy_chara_ctl(org_movie%quilt_mode_ctl,                     &
     &                    new_movie%quilt_mode_ctl)
      call copy_integer_ctl(org_movie%num_frames_ctl,                   &
     &                      new_movie%num_frames_ctl)
!
      call copy_real2_ctl(org_movie%angle_range_ctl,                    &
     &                    new_movie%angle_range_ctl)
      call copy_int2_ctl(org_movie%num_row_column_ctl,                  &
     &                    new_movie%num_row_column_ctl)
!
      new_movie%i_quilt_image = org_movie%i_quilt_image
!
      end subroutine dup_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine reset_quilt_image_ctl(quilt_c)
!
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      quilt_c%quilt_mode_ctl%iflag =     0
      quilt_c%num_frames_ctl%iflag =     0
      quilt_c%angle_range_ctl%iflag =    0
      quilt_c%num_row_column_ctl%iflag = 0
!
      quilt_c%i_quilt_image = 0
!
      end subroutine reset_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_quilt_image_ctl(quilt_c)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(quilt_image_ctl), intent(inout) :: quilt_c
!
!
      call calypso_mpi_bcast_one_int(quilt_c%i_quilt_image, 0)
!
      call bcast_ctl_type_c1(quilt_c%quilt_mode_ctl)
      call bcast_ctl_type_i1(quilt_c%num_frames_ctl)
!
      call bcast_ctl_type_r2(quilt_c%angle_range_ctl)
      call bcast_ctl_type_i2(quilt_c%num_row_column_ctl)
!
      end subroutine bcast_quilt_image_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function num_label_quilt_image()
      num_label_quilt_image = n_label_quilt_ctl
      return
      end function num_label_quilt_image
!
!  ---------------------------------------------------------------------
!
      subroutine set_label_quilt_image(names)
!
      character(len = kchara), intent(inout)                            &
     &                         :: names(n_label_quilt_ctl)
!
!
      call set_control_labels(hd_quilt_mode, names( 1))
      call set_control_labels(hd_quilt_num_frame, names( 1))
      call set_control_labels(hd_angle_range, names( 2))
      call set_control_labels(hd_row_column, names( 3))
!
      end subroutine set_label_quilt_image
!
! ----------------------------------------------------------------------
!
      end module t_control_data_quilt_image
