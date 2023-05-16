!>@file   t_ctl_data_3d_filter.f90
!!@brief  module t_ctl_data_3d_filter
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief control data for 3D filter functions
!!
!!@verbatim
!!      subroutine dealloc_filter_area_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!      subroutine reset_element_size_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!@endverbatim
      module t_ctl_data_3d_filter
!
      use m_precision
!
      use m_machine_parameter
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
!
      type ctl_data_3d_filter
!>        Structure for filtering groups
!!@n        filter_area_ctl%c_tbl: Name of force
        type(ctl_array_chara) :: filter_area_ctl
!>        Structure for filtering files
        type(filter_file_control) :: ffile_3d_ctl
!
        type(read_character_item) :: mass_matrix_type_ctl
!
        type(read_character_item) :: method_esize_ctl
        type(read_character_item) :: precond_esize_ctl
        type(read_integer_item) :: itr_esize_ctl
        type(read_real_item) :: eps_esize_ctl
        type(read_real_item) :: sigma_esize_ctl
        type(read_real_item) :: sigma_diag_esize_ctl
!
        integer (kind=kint) :: i_filter_area_ctl =   0
        integer (kind=kint) :: i_deltax_ctl =        0
        integer (kind=kint) :: i_esize_solver_ctl =  0
      end type ctl_data_3d_filter
!
      private :: reset_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_filter_area_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call dealloc_control_array_chara(fil3_ctl%filter_area_ctl)
      fil3_ctl%i_filter_area_ctl = 0
!
      end subroutine dealloc_filter_area_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_element_size_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call reset_dx_solver_param_ctl(fil3_ctl)
      fil3_ctl%mass_matrix_type_ctl%iflag = 0
      fil3_ctl%i_deltax_ctl = 0
!
      end subroutine reset_element_size_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_dx_solver_param_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      fil3_ctl%mass_matrix_type_ctl%iflag = 0
!
      fil3_ctl%method_esize_ctl%iflag =     0
      fil3_ctl%precond_esize_ctl%iflag =    0
      fil3_ctl%itr_esize_ctl%iflag =        0
      fil3_ctl%eps_esize_ctl%iflag =        0
      fil3_ctl%sigma_esize_ctl%iflag =      0
      fil3_ctl%sigma_diag_esize_ctl%iflag = 0
!
      fil3_ctl%i_esize_solver_ctl = 1
!
      end subroutine reset_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_3d_filter
