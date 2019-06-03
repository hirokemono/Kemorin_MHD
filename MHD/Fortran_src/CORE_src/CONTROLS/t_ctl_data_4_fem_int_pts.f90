!>@file   t_ctl_data_4_fem_int_pts.f90
!!@brief  module t_ctl_data_4_fem_int_pts
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in 2002
!!@n    Mmodified by H. Matsui in March, 2006
!
!> @brief REad integration points for FEM
!!
!!@verbatim
!!      subroutine read_control_fem_int_points                          &
!!     &         (hd_block, iflag, fint_ctl)
!!        type(fem_intergration_control), intent(inout) :: fint_ctl
!!      subroutine write_control_fem_int_points                         &
!!     &         (id_file, hd_block, fint_ctl, level)
!! ----------------------------------------------------------------------
!!
!!    begin intg_point_num_ctl
!!       integrate_points_ctl     2
!!
!!       intg_point_poisson_ctl   2
!!       intg_point_t_evo_ctl     2
!!    end intg_point_num_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_4_fem_int_pts
!
      use m_precision
      use t_control_elements
!
      implicit  none
!
!
      type fem_intergration_control
!>        Structure for read # of integration points
        type(read_integer_item)  :: integration_points_ctl
!
!>        Structure for read # of integration points for Poisson eq.
        type(read_integer_item) :: intg_point_poisson_ctl
!>        Structure for read # of integration points for time integration
        type(read_integer_item) :: intg_point_t_evo_ctl
      end type fem_intergration_control
!
!    4th level for integeration
!
      character(len=kchara), parameter :: hd_intgration_points          &
     &         = 'integration_points_ctl'
      character(len=kchara), parameter :: hd_intg_point_poisson         &
     &         = 'intg_point_poisson_ctl'
      character(len=kchara), parameter :: hd_intg_point_t_evo           &
     &         = 'intg_point_t_evo_ctl'
!
!
      private :: hd_intgration_points
      private :: hd_intg_point_poisson, hd_intg_point_t_evo
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_control_fem_int_points                            &
     &         (hd_block, iflag, fint_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(fem_intergration_control), intent(inout) :: fint_ctl
!
!
      if(right_begin_flag(hd_block) .eq. 0) return
      if (iflag .gt. 0) return
      do
        call load_ctl_label_and_line
!
        iflag = find_control_end_flag(hd_block)
        if(iflag .gt. 0) exit
!
        call read_integer_ctl_type(c_buf1, hd_intgration_points,        &
     &      fint_ctl%integration_points_ctl)
        call read_integer_ctl_type(c_buf1, hd_intg_point_poisson,       &
     &      fint_ctl%intg_point_poisson_ctl)
        call read_integer_ctl_type(c_buf1, hd_intg_point_t_evo,         &
     &      fint_ctl%intg_point_t_evo_ctl)
      end do
!
      end subroutine read_control_fem_int_points
!
!   --------------------------------------------------------------------
!
      subroutine write_control_fem_int_points                           &
     &         (id_file, hd_block, fint_ctl, level)
!
      use m_machine_parameter
      use m_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(fem_intergration_control), intent(in) :: fint_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      maxlen = max(maxlen, len_trim(hd_intgration_points))
      maxlen = max(maxlen, len_trim(hd_intg_point_poisson))
      maxlen = max(maxlen, len_trim(hd_intg_point_t_evo))
!
      write(id_file,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_file, level, hd_block)
!
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_intgration_points, fint_ctl%integration_points_ctl)
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_intg_point_poisson, fint_ctl%intg_point_poisson_ctl)
      call write_integer_ctl_type(id_file, level, maxlen,               &
     &    hd_intg_point_t_evo, fint_ctl%intg_point_t_evo_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_control_fem_int_points
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_4_fem_int_pts
