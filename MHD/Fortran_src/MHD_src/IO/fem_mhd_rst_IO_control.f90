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
!!      subroutine output_MHD_restart_file_ctl(retval, SGS_par,         &
!!     &          MHD_files, time_d, flex_p, geofem, iphys, FEM_SGS_wk, &
!!     &          rst_step, nod_fld, fem_fst_IO, v_sol, SR_sig, SR_r)
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(time_data), intent(in) :: time_d
!!        type(mesh_geometry), intent(in) :: geofem
!!        type(phys_address), intent(in) :: iphys
!!        type(work_FEM_dynamic_SGS), intent(in) :: FEM_SGS_wk
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(field_IO), intent(inout) :: fem_fst_IO
!!        type(vectors_4_solver), intent(inout) :: v_sol
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine input_MHD_restart_file_ctl(rst_step, MHD_files,      &
!!     &         layer_tbl, node, ele, fluid, SGS_par, wk_sgs, wk_diff, &
!!     &         sgs_coefs, diff_coefs, nod_fld, init_d, time_d, flex_p)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(IO_step_param), intent(in) :: rst_step
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(layering_tbl), intent(in) :: layer_tbl
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
!!        type(SGS_coefficients_type), intent(inout) :: sgs_coefs
!!        type(SGS_commutation_coefs), intent(inout) :: diff_coefs
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(time_data), intent(inout) :: init_d, time_d
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
      use t_mesh_data
      use t_phys_data
      use t_field_data_IO
      use t_layering_ele_list
      use t_ele_info_4_dynamic
      use t_flex_delta_t_parameter
      use t_work_FEM_dynamic_SGS
      use t_MHD_file_parameter
      use t_field_data_IO
      use t_IO_step_parameter
      use t_vector_for_solver
      use t_solver_SR
!
      implicit  none
!
      private :: output_MHD_restart_file
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine output_MHD_restart_file_ctl(retval, SGS_par,           &
     &          MHD_files, time_d, flex_p, geofem, iphys, FEM_SGS_wk,   &
     &          rst_step, nod_fld, fem_fst_IO, v_sol, SR_sig, SR_r)
!
      use m_fem_mhd_restart
      use FEM_sgs_ini_model_coefs_IO
!
      integer(kind = kint), intent(in) :: retval
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(time_data), intent(in) :: time_d
      type(flexible_stepping_parameter), intent(in)  :: flex_p
      type(mesh_data), intent(in) :: geofem
      type(phys_address), intent(in) :: iphys
      type(work_FEM_dynamic_SGS), intent(in) :: FEM_SGS_wk
      type(IO_step_param), intent(in) :: rst_step
!
      type(phys_data), intent(inout) :: nod_fld
      type(field_IO), intent(inout) :: fem_fst_IO
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
      integer(kind = kint) :: istep_rst
!
!
      if(output_IO_flag(flex_p%istep_max_dt,rst_step)                   &
     &     .eqv. .FALSE.) then
        istep_rst = IO_step_exc_zero_inc(flex_p%istep_max_dt, rst_step)
        call output_MHD_restart_file                                    &
     &     (istep_rst, time_d, SGS_par, MHD_files, geofem%mesh, iphys,  &
     &      FEM_SGS_wk, nod_fld, fem_fst_IO, v_sol, SR_sig, SR_r)
      end if
!
!   Finish by elapsed time
      if(retval .eq. 0) then
        istep_rst = -1
        call output_MHD_restart_file                                    &
     &     (istep_rst, time_d, SGS_par, MHD_files, geofem%mesh, iphys,  &
     &      FEM_SGS_wk, nod_fld, fem_fst_IO, v_sol, SR_sig, SR_r)
      end if
!
      end subroutine output_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine output_MHD_restart_file                                &
     &         (istep_rst, time_d, SGS_par, MHD_files,                  &
     &          mesh, iphys, FEM_SGS_wk, nod_fld, fem_fst_IO,           &
     &          v_sol, SR_sig, SR_r)
!
      use m_fem_mhd_restart
      use FEM_sgs_ini_model_coefs_IO
!
      integer(kind = kint), intent(in) :: istep_rst
      type(SGS_paremeters), intent(in) :: SGS_par
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(time_data), intent(in) :: time_d
      type(mesh_geometry), intent(in) :: mesh
      type(phys_address), intent(in) :: iphys
      type(work_FEM_dynamic_SGS), intent(in) :: FEM_SGS_wk
!
      type(phys_data), intent(inout) :: nod_fld
      type(field_IO), intent(inout) :: fem_fst_IO
      type(vectors_4_solver), intent(inout) :: v_sol
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call output_restart_files                                         &
     &   (istep_rst, MHD_files%fst_file_IO, time_d,                     &
     &    mesh%node, mesh%nod_comm, iphys, nod_fld, fem_fst_IO,         &
     &    v_sol, SR_sig, SR_r)
      call write_FEM_Csim_file                                          &
     &   (SGS_par%i_step_sgs_coefs, istep_rst, time_d,                  &
     &    MHD_files%Csim_file_IO, MHD_files%Cdiff_file_IO,              &
     &    SGS_par%model_p, SGS_par%commute_p, FEM_SGS_wk%wk_sgs,        &
     &    FEM_SGS_wk%wk_diff)
!
      end subroutine output_MHD_restart_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine input_MHD_restart_file_ctl(rst_step, MHD_files,        &
     &         layer_tbl, node, ele, fluid, SGS_par, wk_sgs, wk_diff,   &
     &         sgs_coefs, diff_coefs, nod_fld, init_d, time_d, flex_p)
!
      use t_geometry_data_MHD
      use t_SGS_model_coefs
      use m_fem_mhd_restart
      use FEM_sgs_ini_model_coefs_IO
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(field_geometry_data), intent(in) :: fluid
      type(layering_tbl), intent(in) :: layer_tbl
      type(IO_step_param), intent(in) :: rst_step
!
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_model_data), intent(inout) :: wk_sgs, wk_diff
      type(SGS_coefficients_type), intent(inout) :: sgs_coefs
      type(SGS_commutation_coefs), intent(inout) :: diff_coefs
      type(phys_data), intent(inout) :: nod_fld
      type(time_data), intent(inout) :: init_d, time_d
      type(flexible_stepping_parameter), intent(inout) :: flex_p
!
      integer(kind = kint) :: istep_rst
!
!
      istep_rst = set_IO_step(init_d%i_time_step, rst_step)
      call input_restart_files(istep_rst, MHD_files%fst_file_IO,        &
     &    node, nod_fld, init_d, time_d, flex_p)
      call read_alloc_FEM_Csim_file(istep_rst, init_d,                  &
     &    MHD_files%Csim_file_IO, MHD_files%Cdiff_file_IO, ele, fluid,  &
     &    layer_tbl, SGS_par%i_step_sgs_coefs, SGS_par%model_p,         &
     &    SGS_par%commute_p, wk_sgs, wk_diff, sgs_coefs, diff_coefs)
!
      end subroutine input_MHD_restart_file_ctl
!
! -----------------------------------------------------------------------
!
      end module fem_mhd_rst_IO_control
