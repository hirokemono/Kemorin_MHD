!>@file   t_ctl_data_DJDS_ordering.f90
!!        module t_ctl_data_DJDS_ordering
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine read_control_DJDS_solver(hd_block, iflag, DJDS_ctl)
!!      subroutine write_control_DJDS_solver                            &
!!     &         (id_file, hd_block, DJDS_ctl, level)
!!      subroutine reset_control_DJDS_solver(DJDS_ctl)
!!        type(DJDS_control), intent(out) :: DJDS_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!  setting for Ordering for SMP solver !!!!!!!!!!!!!!!!!!!!!
!!
!!    order_method:  method for ordering
!!                  RCM_DJDS or MC_DJDS
!!    min_color_ctl: minimum num. of color for multi color
!!    mc_color_ctl:  color number for MC ordering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!      begin DJDS_solver_ctl
!!        order_method     RCM_DJDS
!!        min_color_ctl    60
!!        mc_color_ctl     100
!!      end DJDS_solver_ctl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_DJDS_ordering
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_ctl_data_4_Multigrid
!
      implicit  none
!
!>      Structure for DJDS solver control
      type DJDS_control
!>        Structure for number of MC/RCM colorling
        type(read_integer_item) :: min_color_ctl
!>        Structure for number of multi colorling
        type(read_integer_item) :: mc_color_ctl
!>        Structure for ordering method
        type(read_character_item) :: order_method_ctl
!
        integer (kind=kint) :: i_DJDS_params = 0
      end type DJDS_control
!
!   4th level for SMP solver control
!
      character(len=kchara), parameter, private                         &
     &         :: hd_order_method = 'order_method'
      character(len=kchara), parameter, private                         &
     &         :: hd_min_color =    'min_color_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_mc_color =     'mc_color_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_DJDS_solver                               &
     &         (id_control, hd_block, DJDS_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(DJDS_control), intent(inout) :: DJDS_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(DJDS_ctl%i_DJDS_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_min_color, DJDS_ctl%min_color_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_mc_color, DJDS_ctl%mc_color_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_order_method, DJDS_ctl%order_method_ctl)
      end do
      DJDS_ctl%i_DJDS_params = 1
!
      end subroutine read_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      subroutine write_control_DJDS_solver                              &
     &         (id_control, hd_block, DJDS_ctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(DJDS_control), intent(in) :: DJDS_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(DJDS_ctl%i_DJDS_params .le. 0) return
!
      maxlen = max(maxlen, len_trim(hd_min_color))
      maxlen = max(maxlen, len_trim(hd_mc_color))
      maxlen = max(maxlen, len_trim(hd_order_method))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    DJDS_ctl%min_color_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    DJDS_ctl%mc_color_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    DJDS_ctl%order_method_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      subroutine reset_control_DJDS_solver(DJDS_ctl)
!
      type(DJDS_control), intent(out) :: DJDS_ctl
!
      DJDS_ctl%min_color_ctl%iflag = 0
      DJDS_ctl%mc_color_ctl%iflag = 0
      DJDS_ctl%order_method_ctl%iflag = 0
!
      DJDS_ctl%i_DJDS_params = 0
!
      end subroutine reset_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_DJDS_ordering
