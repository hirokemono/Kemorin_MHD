!>@file   model_coef_file_IO.f90
!!@brief  module model_coef_file_IO
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
!!      subroutine output_model_coef_file(index_rst, i_step_Csim,       &
!!     &          SGS_param, cmt_param, time_d, wk_sgs, wk_diff)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(time_data), intent(in) :: time_d
!!        type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!!      subroutine input_model_coef_file(istep_rst,                     &
!!     &          SGS_param, cmt_param, ele, fluid, layer_tbl,          &
!!     &          i_step_Csim, wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(commutation_control_params), intent(in) :: cmt_param
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_coefficients_type), intent(inout) :: diff_coefs
!!@endverbatim
!
      module model_coef_file_IO
!
      use m_precision
!
      use calypso_mpi
!
      use t_SGS_control_parameter
      use t_time_data
      use t_geometry_data
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_flex_delta_t_data
      use t_IO_step_parameter
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_model_coef_file(index_rst, i_step_Csim,         &
     &          SGS_param, cmt_param, time_d, wk_sgs, wk_diff)
!
      use t_ele_info_4_dynamic
!
      use sgs_ini_model_coefs_IO
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: index_rst
      integer(kind = kint), intent(in) :: i_step_Csim
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(time_data), intent(in) :: time_d
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      character(len=kchara) :: fn_tmp
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
!
      if(index_rst .lt. 0) then
        call add_elaps_postfix(rst_sgs_coef_head, fn_tmp)
      else
        call add_int_suffix(index_rst, rst_sgs_coef_head, fn_tmp)
      end if
      call add_dat_extension(fn_tmp, rst_sgs_coef_name)
!
      call output_ini_model_coefs                                       &
     &   (i_step_Csim, time_d%i_time_step, time_d%time,                 &
     &    cmt_param, wk_sgs, wk_diff)
!
      end subroutine output_model_coef_file
!
! -----------------------------------------------------------------------
!
      subroutine input_model_coef_file(istep_rst,                       &
     &          SGS_param, cmt_param, ele, fluid, layer_tbl,            &
     &          i_step_Csim, wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      use t_geometry_data_MHD
      use t_SGS_model_coefs
      use t_ele_info_4_dynamic
!
      use set_parallel_file_name
      use sgs_ini_model_coefs_IO
!
      integer(kind = kint), intent(in) :: istep_rst
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(commutation_control_params), intent(in) :: cmt_param
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
!
      integer(kind = kint), intent(inout) :: i_step_Csim
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_coefficients_type), intent(inout) :: diff_coefs
!
      character(len=kchara) :: fn_tmp
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
      if(iflag_rst_sgs_coef_code .eq. 0) return
!
      if (istep_rst .eq. -1) then
        call add_elaps_postfix(rst_sgs_coef_head, fn_tmp)
      else
        call add_int_suffix(istep_rst, rst_sgs_coef_head, fn_tmp)
      end if
!
      call add_dat_extension(fn_tmp, rst_sgs_coef_name)
      call input_ini_model_coefs(cmt_param, ele, fluid, layer_tbl,      &
     &    i_step_Csim, wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine input_model_coef_file
!
! -----------------------------------------------------------------------
!
      end module model_coef_file_IO
