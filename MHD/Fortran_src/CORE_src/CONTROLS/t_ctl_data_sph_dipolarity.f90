!>@file   t_ctl_data_sph_dipolarity.f90
!!        module t_ctl_data_sph_dipolarity
!!
!! @author H. Matsui
!! @date   Programmed in 2012
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_sph_dipolarity_ctl                              &
!!     &         (id_control, hd_block, fdip_ctl, c_buf)
!!      subroutine dealloc_sph_dipolarity_ctl(fdip_ctl)
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spherical harmonics
!!
!!  begin sph_dipolarity_ctl
!!    dipolarity_file_prefix        'monitor/dipolarity'
!!
!!    array dipolarity_truncation_ctl
!!      dipolarity_truncation_ctl     13
!!    end array dipolarity_truncation_ctl
!!  end sph_dipolarity_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_sph_dipolarity
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_ctl_data_sph_vol_spectr
      use t_ctl_data_pick_sph_spectr
      use t_mid_equator_control
      use skip_comment_f
!
      implicit  none
!
!
!>        Structure for dipolarity setting
      type sph_dipolarity_control
!>        Structure for truncation lavel for dipolarity
        type(ctl_array_int) :: fdip_truncation_ctl
!
!>        Structure for dipolarity file prefix
        type(read_character_item) :: fdip_file_prefix_ctl
!
        integer (kind = kint) :: i_dipolarity_ctl = 0
      end type sph_dipolarity_control
!
!
!   labels for item
!
      character(len=kchara), parameter, private                         &
     &            :: hd_fdip_truncation = 'dipolarity_truncation_ctl'
      character(len=kchara), parameter, private                         &
     &            :: hd_fdip_file_prefix = 'dipolarity_file_prefix'
!
!   labels for item
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_sph_dipolarity_ctl                                &
     &         (id_control, hd_block, fdip_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(sph_dipolarity_control), intent(inout) :: fdip_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fdip_ctl%i_dipolarity_ctl  .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_i1(id_control, hd_fdip_truncation,      &
     &                             fdip_ctl%fdip_truncation_ctl, c_buf)
        call read_chara_ctl_type(c_buf, hd_fdip_file_prefix,            &
     &      fdip_ctl%fdip_file_prefix_ctl)
      end do
      fdip_ctl%i_dipolarity_ctl = 1
!
      end subroutine read_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_dipolarity_ctl(fdip_ctl)
!
      type(sph_dipolarity_control), intent(inout) :: fdip_ctl
!
!
      fdip_ctl%i_dipolarity_ctl = 0
!
      call dealloc_control_array_int(fdip_ctl%fdip_truncation_ctl)
      fdip_ctl%fdip_file_prefix_ctl%iflag = 0
!
      end subroutine dealloc_sph_dipolarity_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_sph_dipolarity
