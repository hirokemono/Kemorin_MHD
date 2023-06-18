!>@file   t_ctl_pick_rayleigh_spectr.f90
!!        module t_ctl_pick_rayleigh_spectr
!!
!! @author H. Matsui
!! @date   Programmed in 2016
!!
!!
!> @brief Control data for spectr data monitoring
!!
!!@verbatim
!!      subroutine dealloc_pick_rayleigh_spectr(g_pwr)
!!      subroutine read_rayleigh_pick_mode_ctl                          &
!!     &         (id_control, control_name, pick_ctl)
!!
!! -----------------------------------------------------------------
!!
!!      control block for pickup spectr data
!!
!!  begin pickup_spectr_ctl
!!      Rayleigh_version_ctl       0   99
!!      Rayleigh_checkpoint_dir           'Checkpoint'
!!      time_step_ctl               1000
!!
!!      picked_data_file_name           'picked_spectr.dat'
!!      array pick_sph_spectr_ctl
!!        pick_sph_spectr_ctl   1   0
!!        pick_sph_spectr_ctl   4  -4
!!        pick_sph_spectr_ctl   4   4
!!      end array pick_sph_spectr_ctl
!!    end pickup_spectr_ctl
!!
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_pick_rayleigh_spectr
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_integer
      use t_control_array_integer2
      use skip_comment_f
!
      implicit  none
!
!
      type pick_rayleigh_spectr_control
!>        Structure for gauss coefficient file prefix
        type(read_character_item) :: Rayleigh_rst_dir_ctl
!>        Structure for time step number
        type(read_integer_item) :: Rayleigh_step_ctl
!
!>        Structure for version number  (integers for Mejor and Minor)
        type(read_int2_item) :: Rayleigh_version_ctl
!
!>        Structure for picked spectr data file name
        type(read_character_item) :: picked_data_file_name
!>        Structure for list of mode of Gauss coefficients output
!!@n        idx_rayleigh_ctl%num:   Number of mode
!!@n        idx_rayleigh_ctl%int1: list of degree of Gauss coefficients
!!@n        idx_rayleigh_ctl%int2: list of order of Gauss coefficients
        type(ctl_array_i2) :: idx_rayleigh_ctl
!
        integer (kind = kint) :: i_pick_rayleigh_spectr = 0
      end type pick_rayleigh_spectr_control
!
!
!   labels for item
!
      character(len=kchara), parameter                                  &
     &            :: hd_pick_sph_ctl =     'pickup_spectr_ctl'
!
      character(len=kchara), parameter                                  &
     &           :: hd_Rayleigh_rst_dir = 'Rayleigh_checkpoint_dir'
      character(len=kchara), parameter                                  &
     &           :: hd_Rayleigh_step =    'time_step_ctl'
      character(len=kchara), parameter                                  &
     &           :: hd_Rayleigh_version = 'Rayleigh_version'
!
      character(len=kchara), parameter                                  &
     &           :: hd_picked_data_file = 'picked_data_file_name'
      character(len=kchara), parameter                                  &
     &           :: hd_pick_sph_lm =    'pick_sph_spectr_ctl'
!
      private :: hd_pick_sph_ctl, hd_Rayleigh_step
      private :: hd_Rayleigh_rst_dir, hd_Rayleigh_version
      private :: hd_pick_sph_lm, hd_picked_data_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_rayleigh_spectr(pick_ctl)
!
      type(pick_rayleigh_spectr_control), intent(inout) :: pick_ctl
!
!
      call dealloc_control_array_i2(pick_ctl%idx_rayleigh_ctl)
!
      pick_ctl%Rayleigh_version_ctl%iflag =  0
      pick_ctl%Rayleigh_rst_dir_ctl%iflag =  0
      pick_ctl%Rayleigh_step_ctl%iflag =     0
      pick_ctl%picked_data_file_name%iflag = 0
      pick_ctl%i_pick_rayleigh_spectr = 0
!
      end subroutine dealloc_pick_rayleigh_spectr
!
! -----------------------------------------------------------------------
!
      subroutine read_rayleigh_pick_mode_ctl                            &
     &         (id_control, control_name, pick_ctl)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: control_name
!
      type(pick_rayleigh_spectr_control), intent(inout) :: pick_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      open(id_control, file = control_name)
      call load_one_line_from_control(id_control, c_buf1)
      call read_pick_rayleigh_ctl(id_control, hd_pick_sph_ctl,          &
     &                            pick_ctl, c_buf1)
      close(id_control)
!
      end subroutine read_rayleigh_pick_mode_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_pick_rayleigh_ctl                                 &
     &         (id_control, hd_block, pick_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(pick_rayleigh_spectr_control), intent(inout) :: pick_ctl
      type(buffer_for_control), intent(inout) :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(pick_ctl%i_pick_rayleigh_spectr .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_array_i2(id_control,                          &
     &      hd_pick_sph_lm, pick_ctl%idx_rayleigh_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_picked_data_file,            &
     &      pick_ctl%picked_data_file_name)
        call read_chara_ctl_type(c_buf, hd_Rayleigh_rst_dir,            &
     &      pick_ctl%Rayleigh_rst_dir_ctl)
        call read_integer_ctl_type(c_buf, hd_Rayleigh_step,             &
     &      pick_ctl%Rayleigh_step_ctl)
        call read_integer2_ctl_type(c_buf, hd_Rayleigh_version,         &
     &      pick_ctl%Rayleigh_version_ctl)
      end do
      pick_ctl%i_pick_rayleigh_spectr = 1
!
      end subroutine read_pick_rayleigh_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_pick_rayleigh_ctl(my_rank, pick_ctl)
!
      integer, intent(in) :: my_rank
      type(pick_rayleigh_spectr_control), intent(inout) :: pick_ctl
!
      integer(kind = kint) :: i
!
      write(*,*) my_rank, 'Rayleigh_rst_dir_ctl: ',                     &
     &          trim(pick_ctl%Rayleigh_rst_dir_ctl%charavalue)
      write(*,*) my_rank, 'Rayleigh_step_ctl: ',                        &
     &          pick_ctl%Rayleigh_step_ctl%intvalue
      write(*,*) my_rank, 'Rayleigh_version_ctl: ',                     &
     &          pick_ctl%Rayleigh_version_ctl%intvalue(1:2)
      write(*,*) my_rank, 'picked_data_file_name: ',                    &
     &          trim(pick_ctl%picked_data_file_name%charavalue)
!
      do i = 1, pick_ctl%idx_rayleigh_ctl%num
        write(*,*) my_rank, i, 'st of idx_rayleigh_ctl: ',              &
     &           pick_ctl%idx_rayleigh_ctl%int1(i),                     &
     &           pick_ctl%idx_rayleigh_ctl%int2(i)
      end do
!
      end subroutine check_pick_rayleigh_ctl
!
! -----------------------------------------------------------------------
!
      end module t_ctl_pick_rayleigh_spectr
