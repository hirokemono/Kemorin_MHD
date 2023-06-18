!>@file   t_ctl_data_dipole_fit_ratio.f90
!!        module t_ctl_data_dipole_fit_ratio
!!
!! @author H. Matsui
!! @date   Programmed in 2022
!!
!!
!> @brief Monitoring section IO for Control data
!!
!!@verbatim
!!      subroutine read_ctl_file_dipole_fit_ratio(my_rank, d_fit_ctl)
!!      subroutine read_ctl_dipole_fit_ratio                            &
!!        type(dipole_fit_ratio_ctl), intent(inout) :: d_fit_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine reset_ctl_dipole_fit_ratio(d_fit_ctl)
!!        type(dipole_fit_ratio_ctl), intent(inout) :: d_fit_ctl
!! -----------------------------------------------------------------
!!
!!      control block for dipole fitting
!!
!!  begin sph_fitted_dipole_ratio
!!    start_time_ctl     2.0
!!    end_time_ctl       2.5
!!  
!!    old_format_flag     'Off'
!!    layer_degree_spectr_file_name     'sph_pwr_layer_l.dat.gz'
!!    layer_order_spectr_file_name      'sph_pwr_layer_m.dat.gz'
!!    fitted_ratio_file_name            'fitted_ratio.dat'
!!
!!    odd_mode_only_ctl        'Yes'
!!    fit_truncation_ctl         25
!!    dipolarity_truncation_ctl  13
!!  end sph_fitted_dipole_ratio
!! -----------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_dipole_fit_ratio
!
      use m_precision
!
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
!
      implicit  none
!
!>        Control file name
      character(len = kchara), parameter, private                       &
     &           :: fname_ctl_dipole_fit = 'control_dipole_fit_ratio'
!>        Control file ID
      integer(kind = kint), parameter, private :: id_control = 11
!
!
      type dipole_fit_ratio_ctl
!>        Structure for start time
        type(read_real_item) :: start_time_ctl
!>        Structure for end time
        type(read_real_item) :: end_time_ctl
!
!>        Character flag for old format
        type(read_character_item) :: old_format_ctl
!
!>        Structure for Elsasser number
        type(read_character_item) :: fitted_ratio_file_name
!
!>        Structure for Elsasser number
        type(read_character_item) :: layer_degree_spectr_file_name
!>        Structure for Elsasser number
        type(read_character_item) :: layer_order_spectr_file_name
!
!>        Character flag to use odd modes only for fitting
        type(read_character_item) :: odd_mode_only_ctl
!>        truncation for fitting
        type(read_integer_item) ::   fit_truncation_ctl
!>        truncation for dipolarity
        type(read_integer_item) ::   fdip_truncation_ctl
!
        integer (kind = kint) :: i_gen_dyn_elsasser = 0
      end type dipole_fit_ratio_ctl
!
!
!    label for entry
!
      character(len=kchara), parameter, private                         &
     &             :: hd_fitted_ratio = 'sph_fitted_dipole_ratio'
!
      character(len=kchara), parameter, private                         &
     &           :: hd_start_time_ctl = 'start_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_end_time_ctl = 'end_time_ctl'
      character(len=kchara), parameter, private                         &
     &           :: hd_old_format =   'old_format_flag'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_layer_spec_l_file = 'layer_degree_spectr_file_name'
      character(len=kchara), parameter, private                         &
     &        :: hd_layer_spec_m_file = 'layer_order_spectr_file_name'
      character(len=kchara), parameter, private                         &
     &        :: hd_fitted_file =       'fitted_ratio_file_name'
!
      character(len=kchara), parameter, private                         &
     &        :: hd_odd_mode_only =       'odd_mode_only_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_fit_truncation =      'fit_truncation_ctl'
      character(len=kchara), parameter, private                         &
     &        :: hd_fdip_truncation =     'dipolarity_truncation_ctl'
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_file_dipole_fit_ratio(my_rank, d_fit_ctl)
!
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(dipole_fit_ratio_ctl), intent(inout) :: d_fit_ctl
!
      type(buffer_for_control) :: c_buf1
!
!
      c_buf1%level = 0
      if(my_rank .eq. 0) then
        open(id_control, file = fname_ctl_dipole_fit, status='old')
        do
          call load_one_line_from_control                               &
     &       (id_control, hd_fitted_ratio, c_buf1)
          if(c_buf1%iend .gt. 0) exit
!
          call read_ctl_dipole_fit_ratio                                &
     &       (id_control, hd_fitted_ratio, d_fit_ctl, c_buf1)
          if(d_fit_ctl%i_gen_dyn_elsasser .gt. 0) exit
        end do
        close(id_control)
      end if
!
      end subroutine read_ctl_file_dipole_fit_ratio 
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_dipole_fit_ratio                              &
     &         (id_control, hd_block, d_fit_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(dipole_fit_ratio_ctl), intent(inout) :: d_fit_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(d_fit_ctl%i_gen_dyn_elsasser  .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_start_time_ctl, d_fit_ctl%start_time_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_end_time_ctl, d_fit_ctl%end_time_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_old_format, d_fit_ctl%old_format_ctl)
!
        call read_chara_ctl_type(c_buf, hd_fitted_file,                 &
     &      d_fit_ctl%fitted_ratio_file_name)
!
        call read_chara_ctl_type(c_buf, hd_layer_spec_l_file,           &
     &      d_fit_ctl%layer_degree_spectr_file_name)
        call read_chara_ctl_type(c_buf, hd_layer_spec_m_file,           &
     &      d_fit_ctl%layer_order_spectr_file_name)
!
        call read_chara_ctl_type(c_buf, hd_odd_mode_only,               &
     &      d_fit_ctl%odd_mode_only_ctl)
        call read_integer_ctl_type(c_buf, hd_fit_truncation,            &
     &      d_fit_ctl%fit_truncation_ctl)
        call read_integer_ctl_type(c_buf, hd_fdip_truncation,           &
     &      d_fit_ctl%fdip_truncation_ctl)
      end do
      d_fit_ctl%i_gen_dyn_elsasser = 1
!
      end subroutine read_ctl_dipole_fit_ratio
!
! -----------------------------------------------------------------------
!
      subroutine reset_ctl_dipole_fit_ratio(d_fit_ctl)
!
      type(dipole_fit_ratio_ctl), intent(inout) :: d_fit_ctl
!
!
      d_fit_ctl%start_time_ctl%iflag =   0
      d_fit_ctl%end_time_ctl%iflag =     0
      d_fit_ctl%old_format_ctl%iflag =   0
!
      d_fit_ctl%fitted_ratio_file_name%iflag = 0
!
      d_fit_ctl%layer_degree_spectr_file_name%iflag = 0
      d_fit_ctl%layer_order_spectr_file_name%iflag =  0
!
      d_fit_ctl%odd_mode_only_ctl%iflag =   0
      d_fit_ctl%fit_truncation_ctl%iflag =  0
      d_fit_ctl%fdip_truncation_ctl%iflag = 0
!
      d_fit_ctl%i_gen_dyn_elsasser = 0
!
      end subroutine reset_ctl_dipole_fit_ratio
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_dipole_fit_ratio
