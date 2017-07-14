!>@file   fem_mhd_rst_IO_control.f90
!!@brief  module fem_mhd_rst_IO_control
!!
!!@author H. Matsui
!!@date   programmed by H.Matsui and H.Okuda
!!@n                           on July 2000 (ver 1.1)
!!@n      modified by H. Matsui on Sep., 2006
!!@n      modified by H. Matsui on Dec., 2007
!
!> @brief Call restart data IO routines
!!
!!@verbatim
!!      subroutine output_MHD_restart_file_ctl                          &
!!     &         (i_step, SGS_par, time_d, node, nod_comm, iphys,       &
!!     &          wk_sgs, wk_diff, nod_fld, rst_step)
!!      subroutine elspased_MHD_restart_ctl                             &
!!     &         (SGS_par, time_d, node, nod_comm, iphys,               &
!!     &          wk_sgs, wk_diff, nod_fld)
!!
!!      subroutine input_MHD_restart_file_ctl(rst_step, layer_tbl,      &
!!     &         node, ele, fluid, SGS_par, wk_sgs, wk_diff,            &
!!     &         sgs_coefs, diff_coefs, nod_fld, init_d, time_d, flex_p)
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(time_data), intent(inout) :: time_d
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!@endverbatim
!
      module fem_mhd_rst_IO_control
!
      use m_precision
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_time_data
      use t_comm_table
      use t_geometry_data
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_flex_delta_t_data
      use t_IO_step_parameter
!
      implicit  none
!
      private :: set_step_4_restart
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_MHD_restart_file_ctl                            &
     &         (i_step, SGS_par, time_d, node, nod_comm, iphys,         &
     &          wk_sgs, wk_diff, nod_fld, rst_step)
!
      use m_fem_mhd_restart
      use model_coef_file_IO
!
      integer(kind = kint), intent(in) :: i_step
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(phys_data), intent(inout) :: nod_fld
      type(IO_step_param), intent(inout) :: rst_step
!
!
      if (set_IO_step_flag(i_step,rst_step) .ne. 0) return
!
      call output_restart_files                                         &
     &   (rst_step%istep_file, time_d, node, nod_comm, iphys, nod_fld)
      call output_model_coef_file(rst_step%istep_file,                  &
     &    SGS_par%i_step_sgs_coefs, SGS_par%model_p, SGS_par%commute_p, &
     &    time_d, wk_sgs, wk_diff)
!
      end subroutine output_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
!
      subroutine elspased_MHD_restart_ctl                               &
     &         (SGS_par, time_d, node, nod_comm, iphys,                 &
     &          wk_sgs, wk_diff, nod_fld)
!
      use m_fem_mhd_restart
      use model_coef_file_IO
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
!
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint), parameter :: index_rst = -1
!
!
      call output_restart_files                                         &
     &   (index_rst, time_d, node, nod_comm, iphys, nod_fld)
      call output_model_coef_file(index_rst,                            &
     &    SGS_par%i_step_sgs_coefs, SGS_par%model_p, SGS_par%commute_p, &
     &    time_d, wk_sgs, wk_diff)
!
      end subroutine elspased_MHD_restart_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_MHD_restart_file_ctl(rst_step, layer_tbl,        &
     &         node, ele, fluid, SGS_par, wk_sgs, wk_diff,              &
     &         sgs_coefs, diff_coefs, nod_fld, init_d, time_d, flex_p)
!
      use t_geometry_data_MHD
      use t_SGS_model_coefs
      use m_fem_mhd_restart
      use model_coef_file_IO
!
      type(IO_step_param), intent(in) :: rst_step
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: init_d, time_d
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      integer(kind = kint) :: istep_rst
!
!
      call set_step_4_restart(rst_step, init_d%i_time_step, istep_rst)
      call input_restart_files                                          &
     &   (istep_rst, node, nod_fld, init_d, time_d, flex_p)
      call input_model_coef_file(istep_rst,                             &
     &    SGS_par%model_p, SGS_par%commute_p, ele, fluid, layer_tbl,    &
     &    SGS_par%i_step_sgs_coefs, wk_sgs, wk_diff,                    &
     &    sgs_coefs, diff_coefs)
!
      end subroutine input_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_step_4_restart(rst_step, i_step, istep_rst)
!
      use t_IO_step_parameter
!
      type(IO_step_param), intent(in) :: rst_step
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: istep_rst
!
!
      if(rst_step%increment .gt. 0) then
        istep_rst = int(i_step / rst_step%increment)
      else
        istep_rst = i_step
      end if
!
      if(i_step .eq. -1)   istep_rst = -1
!
      end subroutine set_step_4_restart
!
! -----------------------------------------------------------------------
!
      end module fem_mhd_rst_IO_control
