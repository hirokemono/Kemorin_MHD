!>@file   t_ctl_data_4_Multigrid.f90
!!        module t_ctl_data_4_Multigrid
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine dealloc_control_Multigrid(MG_ctl)
!!        type(MGCG_control), intent(inout) :: MG_ctl
!!!!!!!!  setting for MGCG solver !!!!!!!!!!!!!!!!!!!!!
!!
!!   num_multigrid_level_ctl: multigrid level
!!   num_MG_subdomain_ctl  : number of domains for each level
!!   MG_mesh_header_ctl: mesh data for coarse mesh
!!        (MG_mesh_header_ctl.domain#)
!!
!!   MG_coarse_2_fine_tbl: prolongation data
!!        (MG_coarse_2_fine_tbl.domain#)
!!   MG_fine_2_coarse_tbl: restriction table
!!        (MG_fine_2_coarse_tbl.domain#)
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
!!      end array num_MG_subdomain_ctl
!!
!!      array MG_mesh_header_ctl    2
!!        MG_mesh_header_ctl          'mesh_1st/in'
!!        MG_mesh_header_ctl          'mesh_2nd/in'
!!      end array MG_mesh_header_ctl
!!
!!      array MG_fine_2_coarse_tbl_ctl    2
!!        MG_fine_2_coarse_tbl_ctl          'mesh_1st/fine_2_coarse'
!!        MG_fine_2_coarse_tbl_ctl          'mesh_2nd/fine_2_coarse'
!!      end array MG_fine_2_coarse_tbl_ctl
!!
!!      array MG_coarse_2_fine_tbl_ctl    2
!!        MG_coarse_2_fine_tbl_ctl          'mesh_1st/coarse_2_fine'
!!        MG_coarse_2_fine_tbl_ctl          'mesh_2nd/coarse_2_fine'
!!      end array MG_coarse_2_fine_tbl_ctl
!!
!!      array MG_fine_2_coarse_ele_tbl_ctl    2
!!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_1st/fine_2_coarse_ele'
!!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_2nd/fine_2_coarse_ele'
!!      end array MG_fine_2_coarse_ele_tbl_ctl
!!
!!
!!      array MG_mesh_file_fmt_ctl    2
!!        MG_mesh_file_fmt_ctl           'ascii'
!!        MG_mesh_file_fmt_ctl           'ascii'
!!      end array MG_mesh_file_fmt_ctl
!!
!!      array MG_table_file_fmt_ctl    2
!!        MG_table_file_fmt_ctl           'ascii'
!!        MG_table_file_fmt_ctl           'ascii'
!!      end array MG_table_file_fmt_ctl
!!    end MGCG_parameter_ctl
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!@endverbatim
!
      module t_ctl_data_4_Multigrid
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
!
      implicit  none
!
!
!>        Structure for MGCG control
      type MGCG_control
!>        Block name
        character(len=kchara) :: block_name = 'MGCG_parameter_ctl'
!
!>        Structure for number of multigrid levels
        type(read_integer_item) ::  num_multigrid_level_ctl
!
!>        Structure for number of subdomains for MG
!!@n       num_MG_subdomain_ctl%ivec:  number of subdomains for MG
        type(ctl_array_int) :: num_MG_subdomain_ctl
!
!>        Structure for mesh file prefix for MG
!!@n       MG_mesh_prefix_ctl%c_tbl:  mesh file prefix for MG
        type(ctl_array_chara) :: MG_mesh_prefix_ctl
!
!>        Structure for interpolation table from fine to course grid
!!@n       MG_fine_2_coarse_tbl%c_tbl: file prefix for table data
        type(ctl_array_chara) :: MG_fine_2_coarse_tbl
!>        Structure for interpolation table from course to fine grid
!!@n       MG_coarse_2_fine_tbl%c_tbl: file prefix for table data
        type(ctl_array_chara) :: MG_coarse_2_fine_tbl
!
!>        Structure for interpolation table from fine to cource elements
!!@n       MG_f2c_ele_tbl_ctl%c_tbl: file prefix for table data
        type(ctl_array_chara) :: MG_f2c_ele_tbl_ctl
!
!>        Structure for mesh file format for MG
!!@n       MG_mesh_fmt_ctl%c_tbl:mesh file format for MG
        type(ctl_array_chara) :: MG_mesh_fmt_ctl
!
!>        Structure for interpolation table file format for MG
!!@n       MG_table_fmt_ctl%c_tbl:interpolation table file format for MG
        type(ctl_array_chara) :: MG_table_fmt_ctl
!
!
!>        Structure for method for each level
        type(read_character_item) :: MG_METHOD_ctl
!
!>        Structure for preconditioning for each level
        type(read_character_item) :: MG_PRECOND_ctl
!
!>        Structure for iteration for each level
        type(read_integer_item) :: maxiter_mid_ctl
!>        Structure for iteration for coarsest level
        type(read_integer_item) :: maxiter_coarsest_ctl
!
!>        Structure for error torrance for each step
        type(read_real_item) :: MG_residual_ctl
!
        integer (kind=kint) :: i_Multigrid_params = 0
      end type MGCG_control
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_control_Multigrid(MG_ctl)
!
      type(MGCG_control), intent(inout) :: MG_ctl
!
      call dealloc_control_array_int(MG_ctl%num_MG_subdomain_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_mesh_prefix_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_fine_2_coarse_tbl)
      call dealloc_control_array_chara(MG_ctl%MG_coarse_2_fine_tbl)
      call dealloc_control_array_chara(MG_ctl%MG_f2c_ele_tbl_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_mesh_fmt_ctl)
      call dealloc_control_array_chara(MG_ctl%MG_table_fmt_ctl)
!
      MG_ctl%num_multigrid_level_ctl%iflag = 0
!
      MG_ctl%MG_METHOD_ctl%iflag =           0
      MG_ctl%MG_PRECOND_ctl%iflag =          0
      MG_ctl%maxiter_mid_ctl%iflag =         0
      MG_ctl%maxiter_coarsest_ctl%iflag =    0
      MG_ctl%MG_residual_ctl%iflag =         0
!
      MG_ctl%i_Multigrid_params = 0
!
      end subroutine dealloc_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_Multigrid
