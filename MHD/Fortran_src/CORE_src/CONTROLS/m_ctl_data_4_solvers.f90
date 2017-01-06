!>@file   m_ctl_data_4_solvers.f90
!!@brief  module m_ctl_data_4_solvers
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine read_crs_solver_param_ctl
!!      subroutine read_DJDS_solver_param_ctl
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
!!   MG_elem_header_ctl:  element mesh data header
!!        (MG_elem_header_ctl.domain#)
!!   MG_surf_header_ctl: surface mesh data header
!!        (MG_surf_header_ctl.domain#)
!!   MG_edge_header_ctl: edge mesh data header
!!        (MG_edge_header_ctl.domain#)
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
!!        MG_METHOD_ctl     CG
!!        MG_PRECOND_ctl    DIAG
!!
!!        maxiter_mid_ctl          2
!!        maxiter_coarsest_ctl    30
!!        MG_residual_ctl         1.0e-20
!!
!!        num_multigrid_level_ctl           2
!!
!!        array num_MG_subdomain_ctl    2
!!          num_MG_subdomain_ctl           8
!!          num_MG_subdomain_ctl           2
!!        end array num_MG_subdomain_ctl
!!
!!        array MG_mesh_header_ctl    2
!!          MG_mesh_header_ctl          'mesh_1st/in'
!!          MG_mesh_header_ctl          'mesh_2nd/in'
!!        end array MG_mesh_header_ctl
!!
!!
!!        array MG_fine_2_coarse_tbl_ctl    2
!!          MG_fine_2_coarse_tbl_ctl          'mesh_1st/fine_2_coarse'
!!          MG_fine_2_coarse_tbl_ctl          'mesh_2nd/fine_2_coarse'
!!        end array MG_fine_2_coarse_tbl_ctl
!!
!!        array MG_coarse_2_fine_tbl_ctl    2
!!          MG_coarse_2_fine_tbl_ctl          'mesh_1st/coarse_2_fine'
!!          MG_coarse_2_fine_tbl_ctl          'mesh_2nd/coarse_2_fine'
!!        end array MG_coarse_2_fine_tbl_ctl
!!
!!        array MG_fine_2_coarse_ele_tbl_ctl    2
!!          MG_fine_2_coarse_ele_tbl_ctl    'mesh_1st/fine_2_coarse_ele'
!!          MG_fine_2_coarse_ele_tbl_ctl    'mesh_2nd/fine_2_coarse_ele'
!!        end array MG_fine_2_coarse_ele_tbl_ctl
!!
!!
!!        array MG_mesh_file_fmt_ctl    2
!!          MG_mesh_file_fmt_ctl           'ascii'
!!          MG_mesh_file_fmt_ctl           'ascii'
!!        end array MG_mesh_file_fmt_ctl
!!
!!        array MG_table_file_fmt_ctl    2
!!          MG_table_file_fmt_ctl           'ascii'
!!          MG_table_file_fmt_ctl           'ascii'
!!        end array MG_table_file_fmt_ctl
!!      end MGCG_parameter_ctl
!!    end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_ctl_data_4_solvers
!
      use m_precision
      use t_ctl_data_4_solvers
      use t_ctl_data_4_Multigrid
      use t_control_elements
!
      implicit  none
!
!>      Structure for DJDS solver control
      type(DJDS_control), save :: DJDS_ctl1
!
!>        Structure for MGCG control
      type(MGCG_control), save :: MG_ctl1
!
!
!>      Structure for maximum iteration counts
      type(read_integer_item), save :: itr_ctl
!
!>      Structure for error torrance
      type(read_real_item), save :: eps_ctl
!>      Structure for coefficient for SSOR
      type(read_real_item), save :: sigma_ctl
!>      Structure for coefficient for diagonal
      type(read_real_item), save :: sigma_diag_ctl
! 
!>      Structure for method
      type(read_character_item), save :: method_ctl
!>      Structure for preconditioning
      type(read_character_item), save :: precond_ctl
!
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      integer (kind=kint) :: i_solver_ctl =     0
!
!      character(len=kchara) :: hd_Multigrid_params = 'MGCG_parameter_ctl'
!      integer (kind=kint) :: i_Multigrid_params = 0
!
      character(len=kchara), parameter                                  &
     &       :: hd_DJDS_params =      'DJDS_solver_ctl'
      integer (kind=kint) :: i_DJDS_params = 0
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
      private :: hd_solver_ctl, hd_DJDS_params
      private :: i_solver_ctl,  i_DJDS_params
      private :: hd_Multigrid_params, i_Multigrid_params
      private :: hd_itr, hd_eps, hd_sigma, hd_sigma_diag
      private :: hd_method, hd_precond
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_crs_solver_param_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_solver_ctl) .eq. 0) return
      if (i_solver_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_solver_ctl, i_solver_ctl)
        if(i_solver_ctl .gt. 0) exit
!
!
        call read_control_DJDS_solver                                   &
     &     (hd_DJDS_params, i_DJDS_params, DJDS_ctl1)
        call read_control_Multigrid                                     &
     &     (hd_Multigrid_params, i_Multigrid_params, MG_ctl1)
!
!
        call read_real_ctl_type(hd_eps, eps_ctl)
        call read_real_ctl_type(hd_sigma, sigma_ctl)
        call read_real_ctl_type(hd_sigma_diag, sigma_diag_ctl)
!
        call read_integer_ctl_type(hd_itr, itr_ctl)
!
        call read_chara_ctl_type(hd_method, method_ctl)
        call read_chara_ctl_type(hd_precond, precond_ctl)
      end do
!
      end subroutine read_crs_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_solvers
