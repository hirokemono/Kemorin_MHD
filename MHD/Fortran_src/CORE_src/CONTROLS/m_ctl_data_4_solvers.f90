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
!>      Structure for CG solver control
      type(solver_control), save :: CG_ctl1
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      integer (kind=kint) :: i_solver_ctl =     0
!
!
      private :: hd_solver_ctl, i_solver_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_crs_solver_param_ctl
!
!
      call read_CG_solver_param_ctl                                     &
     &   (hd_solver_ctl, i_solver_ctl, CG_ctl1)
!
      end subroutine read_crs_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_solvers
