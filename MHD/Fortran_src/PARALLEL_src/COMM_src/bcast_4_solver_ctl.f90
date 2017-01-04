!>@file   bcast_4_solver_ctl.f90
!!@brief  module bcast_4_solver_ctl
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  bcast structure of reading parameters for solvers
!!
!!@verbatim
!!      subroutine bcast_CG_solver_param_ctl(CG_ctl)
!!        type(solver_control), intent(inout) :: CG_ctl
!!@endverbatim
!
      module bcast_4_solver_ctl
!
      use m_precision
      use t_ctl_data_4_solvers
      use t_ctl_data_4_Multigrid
!
      implicit  none
!
      private :: bcast_control_DJDS_solver, bcast_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine bcast_CG_solver_param_ctl(CG_ctl)
!
      use bcast_control_arrays
!
      type(solver_control), intent(inout) :: CG_ctl
!
!
      call bcast_control_DJDS_solver(CG_ctl%DJDS_ctl)
      call bcast_control_Multigrid(CG_ctl%MG_ctl)
!
!
      call bcast_ctl_type_r1(CG_ctl%eps_ctl)
      call bcast_ctl_type_r1(CG_ctl%sigma_ctl)
      call bcast_ctl_type_r1(CG_ctl%sigma_diag_ctl)
!
      call bcast_ctl_type_i1(CG_ctl%itr_ctl)
!
      call bcast_ctl_type_c1(CG_ctl%method_ctl)
      call bcast_ctl_type_c1(CG_ctl%precond_ctl)
!
      end subroutine bcast_CG_solver_param_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_control_DJDS_solver(DJDS_ctl)
!
      use bcast_control_arrays
!
      type(DJDS_control), intent(inout) :: DJDS_ctl
!
!
      call bcast_ctl_type_i1(DJDS_ctl%min_color_ctl)
      call bcast_ctl_type_i1(DJDS_ctl%mc_color_ctl)
!
      call bcast_ctl_type_c1(DJDS_ctl%order_method_ctl)
!
      end subroutine bcast_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      subroutine bcast_control_Multigrid(MG_ctl)
!
      use bcast_control_arrays
!
      type(MGCG_control), intent(inout) :: MG_ctl
!
!
      call bcast_ctl_type_c1(MG_ctl%MG_METHOD_ctl)
      call bcast_ctl_type_c1(MG_ctl%MG_PRECOND_ctl)
!
      call bcast_ctl_type_r1(MG_ctl%MG_residual_ctl)
!
      call bcast_ctl_type_i1(MG_ctl%maxiter_mid_ctl)
      call bcast_ctl_type_i1(MG_ctl%maxiter_coarsest_ctl)
!
      call bcast_ctl_type_i1(MG_ctl%num_multigrid_level_ctl)
!
!
      call bcast_ctl_array_i1(MG_ctl%num_MG_subdomain_ctl)
!
      call bcast_ctl_array_c1(MG_ctl%MG_mesh_prefix_ctl)
!
      call bcast_ctl_array_c1(MG_ctl%MG_fine_2_coarse_tbl_ctl)
      call bcast_ctl_array_c1(MG_ctl%MG_coarse_2_fine_tbl_ctl)
      call bcast_ctl_array_c1(MG_ctl%MG_f2c_ele_tbl_ctl)
!
      call bcast_ctl_array_c1(MG_ctl%MG_mesh_fmt_ctl)
      call bcast_ctl_array_c1(MG_ctl%MG_table_fmt_ctl)
!
      end subroutine bcast_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module bcast_4_solver_ctl
