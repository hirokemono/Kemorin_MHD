!>@file   ctl_data_3d_filter_IO.f90
!!@brief  module ctl_data_3d_filter_IO
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief control data for 3D filter functions
!!
!!@verbatim
!!      subroutine read_filter_area_ctl                                 &
!!     &         (id_control, hd_block, fil3_ctl, c_buf)
!!      subroutine write_filter_area_ctl                                &
!!     &         (id_control, hd_block, fil3_ctl, level)
!!
!!      subroutine read_element_size_ctl                                &
!!     &         (id_control, hd_block, fil3_ctl, c_buf)
!!      subroutine write_element_size_ctl                               &
!!     &         (id_control, hd_block, fil3_ctl, level)
!!@endverbatim
      module ctl_data_3d_filter_IO
!
      use m_precision
!
      use m_machine_parameter
      use t_ctl_data_3d_filter
      use t_read_control_elements
      use t_ctl_data_gen_filter
      use t_control_array_character
      use t_ctl_data_filter_files
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
      use skip_comment_f
!
      implicit  none
!
!     3rd level for filter_area
!
      character(len=kchara), parameter, private                         &
     &         :: hd_filter_area = 'filter_ele_grp_ctl'
!
!     3rd level for mass matrix
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mass_matrix_type = 'mass_matrix_type_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_esize_solver = 'esize_solver_control'
!
!     4th level for solver_control for element size
!
      character(len=kchara), parameter, private                         &
     &         :: hd_method_esize =     'method_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_precond_esize =    'precond_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_itr_esize =        'itr_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_eps_esize =        'eps_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_sigma_esize =      'sigma_ctl'
      character(len=kchara), parameter, private                         &
     &         :: hd_sigma_diag_esize = 'sigma_diag_ctl'
!
      private :: read_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_area_ctl                                   &
     &         (id_control, hd_block, fil3_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fil3_ctl%i_filter_area_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control, hd_filter_area,          &
     &      fil3_ctl%filter_area_ctl, c_buf)
      end do
      fil3_ctl%i_filter_area_ctl = 1
!
      end subroutine read_filter_area_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_filter_area_ctl                                  &
     &         (id_control, hd_block, fil3_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_3d_filter), intent(in) :: fil3_ctl
      integer(kind = kint), intent(inout) :: level
!
!
      if(fil3_ctl%i_filter_area_ctl .le. 0) return
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
      call write_control_array_c1(id_control, level,                    &
     &    fil3_ctl%filter_area_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_filter_area_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_element_size_ctl                                  &
     &         (id_control, hd_block, fil3_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fil3_ctl%i_deltax_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_dx_solver_param_ctl                                   &
     &     (id_control, hd_esize_solver, fil3_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_mass_matrix_type,            &
     &      fil3_ctl%mass_matrix_type_ctl)
      end do
      fil3_ctl%i_deltax_ctl = 1
!
      end subroutine read_element_size_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_element_size_ctl                                 &
     &         (id_control, hd_block, fil3_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_3d_filter), intent(in) :: fil3_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fil3_ctl%i_deltax_ctl .le. 0) return
!
      maxlen = len_trim(hd_mass_matrix_type)
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_dx_solver_param_ctl                                    &
     &   (id_control, hd_esize_solver, fil3_ctl, level)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fil3_ctl%mass_matrix_type_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_element_size_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_dx_solver_param_ctl                               &
     &         (id_control, hd_block, fil3_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(fil3_ctl%i_esize_solver_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_method_esize, fil3_ctl%method_esize_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_precond_esize, fil3_ctl%precond_esize_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_eps_esize, fil3_ctl%eps_esize_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_sigma_esize, fil3_ctl%sigma_esize_ctl)
        call read_real_ctl_type(c_buf, hd_sigma_diag_esize,             &
     &      fil3_ctl%sigma_diag_esize_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_itr_esize, fil3_ctl%itr_esize_ctl)
      end do
      fil3_ctl%i_esize_solver_ctl = 1
!
      end subroutine read_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine write_dx_solver_param_ctl                              &
     &         (id_control, hd_block, fil3_ctl, level)
!
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(ctl_data_3d_filter), intent(in) :: fil3_ctl
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(fil3_ctl%i_esize_solver_ctl .le. 0) return
!
      maxlen = len_trim(hd_method_esize)
      maxlen = max(maxlen, len_trim(hd_precond_esize))
      maxlen = max(maxlen, len_trim(hd_eps_esize))
      maxlen = max(maxlen, len_trim(hd_itr_esize))
      maxlen = max(maxlen, len_trim(hd_sigma_esize))
      maxlen = max(maxlen, len_trim(hd_sigma_diag_esize))
      maxlen = max(maxlen, len_trim(hd_itr_esize))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fil3_ctl%method_esize_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    fil3_ctl%precond_esize_ctl)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps_esize, fil3_ctl%eps_esize_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    fil3_ctl%itr_esize_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_sigma_esize, fil3_ctl%sigma_esize_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_sigma_diag_esize, fil3_ctl%sigma_diag_esize_ctl)
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      end module ctl_data_3d_filter_IO
