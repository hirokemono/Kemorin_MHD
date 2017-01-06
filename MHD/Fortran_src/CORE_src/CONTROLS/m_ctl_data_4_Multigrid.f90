!>@file   m_ctl_data_4_Multigrid.f90
!!@brief  module m_ctl_data_4_Multigrid
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!!!!!!!  setting for MGCG solver !!!!!!!!!!!!!!!!!!!!!
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    begin MGCG_parameter_ctl
!!      MG_METHOD_ctl     CG
!!      MG_PRECOND_ctl    DIAG
!!
!!      maxiter_mid_ctl          2
!!      maxiter_coarsest_ctl    30
!!      MG_residual_ctl         1.0e-20
!!
!!      num_multigrid_level_ctl           2
!!
!!      array num_MG_subdomain_ctl    2
!!        num_MG_subdomain_ctl           8
!!        num_MG_subdomain_ctl           2
!!      end array
!!
!!      array MG_mesh_header_ctl    2
!!        MG_mesh_header_ctl          'mesh_1st/in'
!!        MG_mesh_header_ctl          'mesh_2nd/in'
!!      end array
!!
!!      array MG_elem_header_ctl    2
!!        MG_elem_header_ctl          'mesh_1st/in_element'
!!        MG_elem_header_ctl          'mesh_2nd/in_element'
!!      end array
!!
!!      array MG_surf_header_ctl    2
!!        MG_surf_header_ctl          'mesh_1st/in_surface'
!!        MG_surf_header_ctl          'mesh_2nd/in_surface'
!!      end array
!!
!!      array MG_edge_header_ctl    2
!!        MG_edge_header_ctl          'mesh_1st/in_edge'
!!        MG_edge_header_ctl          'mesh_2nd/in_edge'
!!      end array
!!
!!
!!      array MG_fine_2_coarse_tbl_ctl    2
!!        MG_fine_2_coarse_tbl_ctl          'mesh_1st/fine_2_coarse'
!!        MG_fine_2_coarse_tbl_ctl          'mesh_2nd/fine_2_coarse'
!!      end array
!!
!!      array MG_coarse_2_fine_tbl_ctl    2
!!        MG_coarse_2_fine_tbl_ctl          'mesh_1st/coarse_2_fine'
!!        MG_coarse_2_fine_tbl_ctl          'mesh_2nd/coarse_2_fine'
!!      end array
!!
!!      array MG_fine_2_coarse_ele_tbl_ctl    2
!!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_1st/fine_2_coarse_ele'
!!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_2nd/fine_2_coarse_ele'
!!      end array
!!
!!
!!      array MG_mesh_file_fmt_ctl    2
!!        MG_mesh_file_fmt_ctl           'ascii'
!!        MG_mesh_file_fmt_ctl           'ascii'
!!      end array
!!
!!      array MG_table_file_fmt_ctl    2
!!        MG_table_file_fmt_ctl           'ascii'
!!        MG_table_file_fmt_ctl           'ascii'
!!      end array
!!    end
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module m_ctl_data_4_Multigrid
!
      use m_precision
      use t_ctl_data_4_Multigrid
!
      implicit  none
!
!
!>        Structure for MGCG control
      type(MGCG_control), save :: MG_ctl1
!
!   label for entry of group
!
      character(len=kchara) :: hd_Multigrid_params = 'MGCG_parameter_ctl'
      integer (kind=kint) :: i_Multigrid_params = 0
!
      private :: hd_Multigrid_params, i_Multigrid_params
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_Multigrid
!
!
      call read_control_Multigrid                                       &
     &   (hd_Multigrid_params, i_Multigrid_params, MG_ctl1)
!
      end subroutine read_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module  m_ctl_data_4_Multigrid
