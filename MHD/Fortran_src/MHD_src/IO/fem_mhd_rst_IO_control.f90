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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_MHD_restart_file_ctl                            &
     &         (SGS_par, time_d, rst_step, node, nod_comm, iphys,       &
     &          wk_sgs, wk_diff, nod_fld)
!
      use m_fem_mhd_restart
      use sgs_ini_model_coefs_IO
!
      type(SGS_paremeters), intent(in) :: SGS_par
      type(time_data), intent(in) :: time_d
      type(IO_step_param), intent(in) :: rst_step
      type(node_data), intent(in) :: node
      type(communication_table), intent(in) :: nod_comm
      type(phys_address), intent(in) :: iphys
      type(dynamic_model_data), intent(in) :: wk_sgs, wk_diff
!
      type(phys_data), intent(inout) :: nod_fld
!
!
      call output_restart_files                                         &
     &   (rst_step%istep_file, time_d, node, nod_comm, iphys, nod_fld)
      call write_FEM_Csim_file                                          &
     &   (SGS_par%i_step_sgs_coefs, time_d, rst_step,                   &
     &    SGS_par%model_p, SGS_par%commute_p, wk_sgs, wk_diff)
!
      end subroutine output_MHD_restart_file_ctl
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
      use sgs_ini_model_coefs_IO
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
!
      call input_restart_files                                          &
     &   (rst_step%istep_file, node, nod_fld, init_d, time_d, flex_p)
      call read_alloc_FEM_Csim_file(rst_step, init_d,                   &
     &    SGS_par%model_p, SGS_par%commute_p, ele, fluid, layer_tbl,    &
     &    SGS_par%i_step_sgs_coefs, wk_sgs, wk_diff,                    &
     &    sgs_coefs, diff_coefs)
!
      end subroutine input_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
!
      end module fem_mhd_rst_IO_control
