!>@file   t_ctl_data_4_solvers.f90
!!@brief  module t_ctl_data_4_solvers
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine read_CG_solver_param_ctl(hd_block, iflag, CG_ctl)
!!      subroutine read_crs_solver_param_ctl
!!      subroutine read_control_DJDS_solver(hd_block, iflag, DJDS_ctl)
!!        type(DJDS_control), intent(inout) :: DJDS_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!  parameter for solver 
!!
!!      method_ctl:     Method for CG solver
!!          (CG, BiCGSTAB, GPBiCG, MGCG, GAUSS, JACOBI)
!!      precond_ctl:    Preconditioning for CG solver
!!          (DIAG, SSOR, ILU, Gauss, Jacobi)
!!      itr_ctl:        Maximum iteration
!!      eps_ctl:        Residual ratio
!!      sigma_ctl:      Coefficient for for ILU
!!      sigma_diag_ctl: Coeffisient for SSOR preconditioning
!!
!!!!!!!  setting for Ordering for SMP solver !!!!!!!!!!!!!!!!!!!!!
!!
!!    order_method:  method for ordering
!!                  RCM_DJDS or MC_DJDS
!!    min_color_ctl: minimum num. of color for multi color
!!    mc_color_ctl:  color number for MC ordering
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!  setting for MGCG solver !!!!!!!!!!!!!!!!!!!!!
!!
!!   num_multigrid_level_ctl: multigrid level
!!   num_MG_subdomain_ctl  : number of domains for each level
!!   MG_mesh_header_ctl: mesh data for coarse mesh
!!        (MG_mesh_header_ctl.domain#)
!!
!!   MG_coarse_2_fine_tbl_ctl: prolongation data
!!        (MG_coarse_2_fine_tbl_ctl.domain#)
!!   MG_fine_2_coarse_tbl_ctl: restriction table
!!        (MG_fine_2_coarse_tbl_ctl.domain#)
!!
!!   MG_fine_2_coarse_ele_tbl_ctl: element restriction table
!!        (MG_fine_2_coarse_ele_tbl_ctl.domain#)
!!
!!   MG_METHOD_ctl:     Method for Multigrid iteration
!!          (CG, BiCGSTAB, GPBiCG, MGCG, GAUSS, JACOBI)
!!      precond_ctl:    Preconditioning Multigrid iteration
!!          (DIAG, SSOR, ILU, Gauss, Jacobi)
!!    Aviable METHOD  (MG_METHOD_ctl, MG_PRECOND_ctl)
!!        (CG, DIAG),  (CG, SSOR),  (GAUSS, GAUSS), (JACOBI, JACOBI)
!!
!!
!!    maxiter_mid_ctl:     iteration count for mid level
!!    maxiter_coarsest_ctl: iteration count for coarsest level
!!    MG_residual_ctl:      residual level for Multigrid iteration
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin solver_ctl
!!      itr_ctl              1000
!!      eps_ctl              1.0E-4
!!      sigma_ctl               1.0
!!      sigma_diag_ctl          1.0
!!      method_ctl              CG
!!      precond_ctl             SSOR
!!
!!      begin DJDS_solver_ctl
!!        order_method     RCM_DJDS
!!        min_color_ctl    60
!!        mc_color_ctl     100
!!      end DJDS_solver_ctl
!!
!!      begin MGCG_parameter_ctl
!!        ...
!!      end MGCG_parameter_ctl
!!    end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_4_solvers
!
      use m_precision
      use t_control_elements
      use t_ctl_data_4_Multigrid
!
      implicit  none
!
!>      Structure for DJDS solver control
      type DJDS_control
!>        Structure for number of MC/RCM colorling
        type(read_integer_item) :: min_color_ctl
!>        Structure for number of multi colorling
        type(read_integer_item) :: mc_color_ctl
!>        Structure for ordering method
        type(read_character_item) :: order_method_ctl
      end type DJDS_control
!
!>      Structure for CG solver control
      type solver_control
!>        Structure for maximum iteration counts
        type(read_integer_item) :: itr_ctl
!
!>        Structure for error torrance
        type(read_real_item) :: eps_ctl
!>        Structure for coefficient for SSOR
        type(read_real_item) :: sigma_ctl
!>        Structure for coefficient for diagonal
        type(read_real_item) :: sigma_diag_ctl
!
!>        Structure for method
        type(read_character_item) :: method_ctl
!>        Structure for preconditioning
        type(read_character_item) :: precond_ctl
!
!>        Structure for DJDS solver control
        type(DJDS_control) :: DJDS_ctl
!>        Structure for MGCG control
        type(MGCG_control) :: MG_ctl
      end type solver_control
!
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_DJDS_params =      'DJDS_solver_ctl'
      integer (kind=kint) :: i_DJDS_params = 0
!
      character(len=kchara)                                             &
     &       :: hd_Multigrid_params = 'MGCG_parameter_ctl'
      integer (kind=kint) :: i_Multigrid_params = 0
!
!   4th level for ICCG
!
      character(len=kchara), parameter :: hd_itr =        'itr_ctl'
      character(len=kchara), parameter :: hd_eps =        'eps_ctl'
      character(len=kchara), parameter :: hd_sigma =      'sigma_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_sigma_diag = 'sigma_diag_ctl'
      character(len=kchara), parameter :: hd_method =     'method_ctl'
      character(len=kchara), parameter :: hd_precond =    'precond_ctl'
!
!   4th level for SMP solver control
!
      character(len=kchara), parameter                                  &
     &         :: hd_order_method = 'order_method'
      character(len=kchara), parameter                                  &
     &         :: hd_min_color =    'min_color_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_mc_color =     'mc_color_ctl'
!
      private :: hd_DJDS_params,  i_DJDS_params
      private :: hd_Multigrid_params,  i_Multigrid_params
      private :: hd_itr, hd_eps, hd_sigma, hd_sigma_diag
      private :: hd_method, hd_precond
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_CG_solver_param_ctl(hd_block, iflag, CG_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(solver_control), intent(inout) :: CG_ctl
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
!
        call read_control_DJDS_solver                                   &
     &     (hd_DJDS_params, i_DJDS_params, CG_ctl%DJDS_ctl)
        call read_control_Multigrid                                     &
     &     (hd_Multigrid_params, i_Multigrid_params, CG_ctl%MG_ctl)
!
!
        call read_real_ctl_type(hd_eps, CG_ctl%eps_ctl)
        call read_real_ctl_type(hd_sigma, CG_ctl%sigma_ctl)
        call read_real_ctl_type(hd_sigma_diag, CG_ctl%sigma_diag_ctl)
!
        call read_integer_ctl_type(hd_itr, CG_ctl%itr_ctl)
!
        call read_chara_ctl_type(hd_method, CG_ctl%method_ctl)
        call read_chara_ctl_type(hd_precond, CG_ctl%precond_ctl)
      end do
!
      end subroutine read_CG_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_control_DJDS_solver(hd_block, iflag, DJDS_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(DJDS_control), intent(inout) :: DJDS_ctl
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
!
        call read_integer_ctl_type                                      &
     &     (hd_min_color, DJDS_ctl%min_color_ctl)
        call read_integer_ctl_type(hd_mc_color, DJDS_ctl%mc_color_ctl)
!
        call read_chara_ctl_type                                        &
     &     (hd_order_method, DJDS_ctl%order_method_ctl)
      end do
!
      end subroutine read_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_solvers
