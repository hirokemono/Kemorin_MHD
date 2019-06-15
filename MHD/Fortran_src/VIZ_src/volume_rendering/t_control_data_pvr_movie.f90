!>@file   t_control_data_pvr_movie.f90
!!@brief  module t_control_data_pvr_movie
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief control data for PVR movie from snapshot
!!
!!@verbatim
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      !!      subroutine read_pvr_rotation_ctl                                &
!!     &         (id_control, hd_block, movie, c_buf)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      subroutine dup_pvr_movie_control_flags(movie)
!!        type(pvr_movie_ctl), intent(in) :: org_movie
!!        type(pvr_movie_ctl), intent(inout) :: new_movie
!!      subroutine reset_pvr_movie_control_flags(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!
!!      subroutine bcast_pvr_rotation_ctl(movie)
!!        type(pvr_movie_ctl), intent(inout) :: movie
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  begin image_rotation_ctl
!!    rotation_axis_ctl       z
!!    rotation_axis_ctl       1
!!  end image_rotation_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_control_data_pvr_movie
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use t_read_control_elements
      use t_control_data_4_psf
      use t_control_elements
      use skip_comment_f
!
      implicit  none
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
!     3rd level for rotation
!
      character(len=kchara) :: hd_movie_rot_axis =  'rotation_axis_ctl'
      character(len=kchara) :: hd_movie_rot_frame = 'num_frames_ctl'
!
      private :: hd_movie_rot_axis, hd_movie_rot_frame
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_pvr_rotation_ctl                                  &
     &         (id_control, hd_block, movie, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(pvr_movie_ctl), intent(inout) :: movie
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if (movie%i_pvr_rotation.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer_ctl_type(c_buf, hd_movie_rot_frame,           &
     &      movie%num_frames_ctl)
        call read_chara_ctl_type(c_buf, hd_movie_rot_axis,              &
     &      movie%rotation_axis_ctl)
      end do
      movie%i_pvr_rotation = 1
!
      end subroutine read_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dup_pvr_movie_control_flags(org_movie, new_movie)
!
      use copy_control_elements
!
      type(pvr_movie_ctl), intent(in) :: org_movie
      type(pvr_movie_ctl), intent(inout) :: new_movie
!
!
      call copy_chara_ctl(org_movie%rotation_axis_ctl,                  &
     &                    new_movie%rotation_axis_ctl)
      call copy_integer_ctl(org_movie%num_frames_ctl,                   &
     &                      new_movie%num_frames_ctl)
      new_movie%i_pvr_rotation = org_movie%i_pvr_rotation
!
      end subroutine dup_pvr_movie_control_flags
!
!  ---------------------------------------------------------------------
!
      subroutine reset_pvr_movie_control_flags(movie)
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      movie%num_frames_ctl%iflag =    0
      movie%rotation_axis_ctl%iflag = 0
!
      movie%i_pvr_rotation = 0
!
      end subroutine reset_pvr_movie_control_flags
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine bcast_pvr_rotation_ctl(movie)
!
      use calypso_mpi
      use bcast_control_arrays
!
      type(pvr_movie_ctl), intent(inout) :: movie
!
!
      call MPI_BCAST(movie%i_pvr_rotation,  1,                          &
     &              CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      call bcast_ctl_type_i1(movie%num_frames_ctl)
      call bcast_ctl_type_c1(movie%rotation_axis_ctl)
!
      end subroutine bcast_pvr_rotation_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_control_data_pvr_movie
