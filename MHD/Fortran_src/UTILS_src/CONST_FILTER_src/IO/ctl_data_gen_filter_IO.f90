!>@file   ctl_data_gen_filter_IO.f90
!!@brief  module ctl_data_gen_filter_IO
!!
!!@author H. Matsui
!!@date Programmed in March, 2004
!
!> @brief  filter control data IO
!!
!!@verbatim
!!      subroutine read_filter_param_ctl                                &
!!     &         (id_control, hd_block, gen_f_ctl, c_buf)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
!!        type(buffer_for_control), intent(inout)  :: c_buf
!!      subroutine write_filter_param_ctl                               &
!!     &         (id_control, hd_block, gen_f_ctl, level)
!!        integer(kind = kint), intent(in) :: id_control
!!        character(len=kchara), intent(in) :: hd_block
!!        type(ctl_data_gen_filter), intent(in) :: gen_f_ctl
!!        integer(kind = kint), intent(inout) :: level
!!@endverbatim
!
      module ctl_data_gen_filter_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_gen_filter
      use t_read_control_elements
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use t_control_array_charareal
      use t_control_array_intcharreal
      use t_ctl_data_4_solvers
!
      implicit  none
!
!     3rd level for filter_control
!
      character(len=kchara), parameter, private                         &
     &         :: hd_tgt_filter_type =   'target_filter_type'
      character(len=kchara), parameter, private                         &
     &         :: hd_momentum_type =     'momentum_type'
      character(len=kchara), parameter, private                         &
     &         :: hd_num_int_points =   'num_int_points'
      character(len=kchara), parameter, private                         &
     &         :: hd_maximum_neighbour = 'maximum_neighbour'
      character(len=kchara), parameter, private                         &
     &         :: hd_omitted_ratio =    'omitted_ratio'
      character(len=kchara), parameter, private                         &
     &         :: hd_minimum_comp =     'minimum_components'
      character(len=kchara), parameter, private                         &
     &         :: hd_ordering_list =    'ordering_list_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_minimum_det =       'minimum_det_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_maximum_rms =       'maximum_rms_coef_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_filter_corection =  'filter_correction_mode'
      character(len=kchara), parameter, private                         &
     &         :: hd_filter_fixed_point = 'fixed_points_mode'
      character(len=kchara), parameter, private                         &
     &         :: hd_filter_negative_center = 'allow_negative_center'
      character(len=kchara), parameter, private                         &
     &         :: hd_err_level_commute =  'filter_error_info_level'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_start_node_ctl =    'start_node_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_end_node_ctl =      'end_node_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_start_nfree_mat =   'start_mat_freedom_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_end_nfree_mat =     'end_mat_freedom_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_ref_filter =        'ref_filter_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_order_moments =     'moments_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_solver_type =       'solver_type'
      character(len=kchara), parameter, private                         &
     &       :: hd_solver_ctl =     'solver_ctl'
!
      character(len=kchara), parameter, private                         &
     &         :: hd_nele_filtering =    'num_element_4_filter_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_horiz_filter =  'num_horiz_filter'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_param_ctl                                  &
     &         (id_control, hd_block, gen_f_ctl, c_buf)
!
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_gen_filter), intent(inout) :: gen_f_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(gen_f_ctl%i_filter_param_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_CG_solver_param_ctl(id_control, hd_solver_ctl,        &
     &      gen_f_ctl%CG_filter_ctl, c_buf)
!
        call read_control_array_i_c_r(id_control,                       &
     &      hd_order_moments, gen_f_ctl%ref_filter_mom_ctl, c_buf)
!
        call read_control_array_c_r(id_control,                         &
     &      hd_ref_filter, gen_f_ctl%reference_filter_ctl, c_buf)
        call read_control_array_c_r(id_control,                         &
     &      hd_horiz_filter, gen_f_ctl%horizontal_filter_ctl, c_buf)
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_solver_type, gen_f_ctl%f_solver_type_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ordering_list, gen_f_ctl%ordering_list_ctl)
        call read_chara_ctl_type(c_buf, hd_tgt_filter_type,             &
     &      gen_f_ctl%tgt_filter_type_ctl)
        call read_chara_ctl_type(c_buf, hd_momentum_type,               &
     &      gen_f_ctl%momentum_type_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_corection,            &
     &      gen_f_ctl%filter_correction_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_fixed_point,          &
     &      gen_f_ctl%filter_fixed_point_ctl)
        call read_chara_ctl_type(c_buf, hd_filter_negative_center,      &
     &      gen_f_ctl%negative_center_ctl)
!
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_omitted_ratio, gen_f_ctl%omitted_ratio_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_minimum_det, gen_f_ctl%minimum_det_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_maximum_rms, gen_f_ctl%maximum_rms_ctl)
!
!
        call read_integer_ctl_type(c_buf, hd_num_int_points,            &
     &      gen_f_ctl%num_int_points_ctl)
        call read_integer_ctl_type(c_buf, hd_minimum_comp,              &
     &      gen_f_ctl%minimum_comp_ctl)
        call read_integer_ctl_type(c_buf, hd_nele_filtering,            &
     &      gen_f_ctl%num_ele_4_filter_ctl)
        call read_integer_ctl_type(c_buf, hd_maximum_neighbour,         &
     &      gen_f_ctl%maximum_neighbour_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_start_node_ctl, gen_f_ctl%start_node_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_end_node_ctl, gen_f_ctl%end_node_ctl)
        call read_integer_ctl_type(c_buf, hd_start_nfree_mat,           &
     &      gen_f_ctl%ist_num_free_ctl)
        call read_integer_ctl_type(c_buf, hd_end_nfree_mat,             &
     &      gen_f_ctl%ied_num_free_ctl)
        call read_integer_ctl_type(c_buf, hd_err_level_commute,         &
     &      gen_f_ctl%ilevel_filter_error_info)
      end do
      gen_f_ctl%i_filter_param_ctl = 1
!
      end subroutine read_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_filter_param_ctl                                 &
     &         (id_control, hd_block, gen_f_ctl, level)
!
      use skip_comment_f
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_gen_filter), intent(in) :: gen_f_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(gen_f_ctl%i_filter_param_ctl .le. 0) return
!
      maxlen = len_trim(hd_tgt_filter_type)
      maxlen = max(maxlen, len_trim(hd_tgt_filter_type))
      maxlen = max(maxlen, len_trim(hd_momentum_type))
      maxlen = max(maxlen, len_trim(hd_num_int_points))
      maxlen = max(maxlen, len_trim(hd_maximum_neighbour))
      maxlen = max(maxlen, len_trim(hd_omitted_ratio))
      maxlen = max(maxlen, len_trim(hd_ordering_list))
      maxlen = max(maxlen, len_trim(hd_minimum_det))
      maxlen = max(maxlen, len_trim(hd_maximum_rms))
      maxlen = max(maxlen, len_trim(hd_filter_corection))
      maxlen = max(maxlen, len_trim(hd_filter_fixed_point))
      maxlen = max(maxlen, len_trim(hd_filter_negative_center))
      maxlen = max(maxlen, len_trim(hd_err_level_commute))
      maxlen = max(maxlen, len_trim(hd_start_node_ctl))
      maxlen = max(maxlen, len_trim(hd_end_node_ctl))
      maxlen = max(maxlen, len_trim(hd_start_nfree_mat))
      maxlen = max(maxlen, len_trim(hd_end_nfree_mat))
      maxlen = max(maxlen, len_trim(hd_ref_filter))
      maxlen = max(maxlen, len_trim(hd_solver_type))
      maxlen = max(maxlen, len_trim(hd_nele_filtering))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%tgt_filter_type_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%momentum_type_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%num_int_points_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%maximum_neighbour_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    gen_f_ctl%omitted_ratio_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%minimum_comp_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%ordering_list_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    gen_f_ctl%minimum_det_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    gen_f_ctl%maximum_rms_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%filter_correction_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%filter_fixed_point_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%negative_center_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%ilevel_filter_error_info)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%start_node_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%end_node_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%ist_num_free_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%ied_num_free_ctl)
!
      call write_control_array_c_r(id_control, level,                   &
     &    gen_f_ctl%reference_filter_ctl)
      call write_control_array_i_c_r(id_control, level,                 &
     &    hd_order_moments, gen_f_ctl%ref_filter_mom_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    gen_f_ctl%f_solver_type_ctl)
      call write_CG_solver_param_ctl(id_control,                        &
     &    hd_solver_ctl, gen_f_ctl%CG_filter_ctl, level)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    gen_f_ctl%num_ele_4_filter_ctl)
      call write_control_array_c_r(id_control, level,                   &
     &    gen_f_ctl%horizontal_filter_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_filter_param_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_gen_filter_IO
