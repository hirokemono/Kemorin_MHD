!
!      module m_ctl_data_4_Multigrid
!
!        programmed by H.Matsui on July, 2007
!
!      subroutine read_ctl_data_4_Multigrid(ierr, e_message)
!
!      Example of this block
!!
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
!
!    begin MGCG_parameter_ctl
!      MG_METHOD_ctl     CG
!      MG_PRECOND_ctl    DIAG
!
!      maxiter_mid_ctl          2
!      maxiter_coarsest_ctl    30
!      MG_residual_ctl         1.0e-20
!
!      num_multigrid_level_ctl           2
!
!      array num_MG_subdomain_ctl    2
!        num_MG_subdomain_ctl           8
!        num_MG_subdomain_ctl           2
!      end array
!
!      array MG_mesh_header_ctl    2
!        MG_mesh_header_ctl          'mesh_1st/in'
!        MG_mesh_header_ctl          'mesh_2nd/in'
!      end array
!
!      array MG_elem_header_ctl    2
!        MG_elem_header_ctl          'mesh_1st/in_element'
!        MG_elem_header_ctl          'mesh_2nd/in_element'
!      end array
!
!      array MG_surf_header_ctl    2
!        MG_surf_header_ctl          'mesh_1st/in_surface'
!        MG_surf_header_ctl          'mesh_2nd/in_surface'
!      end array
!
!      array MG_edge_header_ctl    2
!        MG_edge_header_ctl          'mesh_1st/in_edge'
!        MG_edge_header_ctl          'mesh_2nd/in_edge'
!      end array
!
!
!      array MG_fine_2_coarse_tbl_ctl    2
!        MG_fine_2_coarse_tbl_ctl          'mesh_1st/fine_2_coarse'
!        MG_fine_2_coarse_tbl_ctl          'mesh_2nd/fine_2_coarse'
!      end array
!
!      array MG_coarse_2_fine_tbl_ctl    2
!        MG_coarse_2_fine_tbl_ctl          'mesh_1st/coarse_2_fine'
!        MG_coarse_2_fine_tbl_ctl          'mesh_2nd/coarse_2_fine'
!      end array
!
!      array MG_fine_2_coarse_ele_tbl_ctl    2
!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_1st/fine_2_coarse_ele'
!        MG_fine_2_coarse_ele_tbl_ctl      'mesh_2nd/fine_2_coarse_ele'
!      end array
!
!
!      array MG_mesh_file_fmt_ctl    2
!        MG_mesh_file_fmt_ctl           'ascii'
!        MG_mesh_file_fmt_ctl           'ascii'
!      end array
!
!      array MG_table_file_fmt_ctl    2
!        MG_table_file_fmt_ctl           'ascii'
!        MG_table_file_fmt_ctl           'ascii'
!      end array
!    end
!
      module m_ctl_data_4_Multigrid
!
      use m_precision
      use t_read_control_arrays
!
      implicit  none
!
!
      integer(kind = kint) ::  num_multigrid_level_ctl = 0
!
!>      Structure for number of subdomains for MG
!!@n      num_MG_subdomain_ctl%ivec:  number of subdomains for MG
      type(ctl_array_int) :: num_MG_subdomain_ctl
!
!>      Structure for mesh file prefix for MG
!!@n      MG_mesh_prefix_ctl%c_tbl:  mesh file prefix for MG
      type(ctl_array_chara) :: MG_mesh_prefix_ctl
!
!>      Structure for element mesh file prefix for MG
!!@n      MG_elem_prefix_ctl%c_tbl: element mesh file prefix for MG
      type(ctl_array_chara) :: MG_elem_prefix_ctl
!>      Structure for surface mesh file prefix for MG
!!@n      MG_mesh_prefix_ctl%c_tbl: surface mesh file prefix for MG
      type(ctl_array_chara) :: MG_surf_prefix_ctl
!>      Structure for edge mesh file prefix for MG
!!@n      MG_edge_prefix_ctl%c_tbl: edge mesh file prefix for MG
      type(ctl_array_chara) :: MG_edge_prefix_ctl
!
!>      Structure for interpolation table from fine to course grid
!!@n      MG_fine_2_coarse_tbl_ctl%c_tbl: file prefix for table data
      type(ctl_array_chara) :: MG_fine_2_coarse_tbl_ctl
!>      Structure for interpolation table from course to fine grid
!!@n      MG_coarse_2_fine_tbl_ctl%c_tbl: file prefix for table data
      type(ctl_array_chara) :: MG_coarse_2_fine_tbl_ctl
!
!>      Structure for interpolation table from fine to cource elements
!!@n      MG_f2c_ele_tbl_ctl%c_tbl: file prefix for table data
      type(ctl_array_chara) :: MG_f2c_ele_tbl_ctl
!
!>      Structure for mesh file format for MG
!!@n      MG_mesh_fmt_ctl%c_tbl:mesh file format for MG
      type(ctl_array_chara) :: MG_mesh_fmt_ctl
!
!>      Structure for interpolation table file format for MG
!!@n      MG_table_fmt_ctl%c_tbl:interpolation table file format for MG
      type(ctl_array_chara) :: MG_table_fmt_ctl
!
      character(len=kchara) :: MG_METHOD_ctl =  'CG'
      character(len=kchara) :: MG_PRECOND_ctl = 'DIAG'
      integer(kind = kint) :: maxiter_mid_ctl =      2
      integer(kind = kint) :: maxiter_coarsest_ctl = 30
      real(kind = kreal) ::   MG_residual_ctl = 1.0d-30
!
!   label for entry of group
!
      character(len=kchara) :: hd_Multigrid_params = 'MGCG_parameter_ctl'
      integer (kind=kint) :: i_Multigrid_params = 0
!
!   file and domain controls
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_MG_level = 'num_multigrid_level_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_num_MG_subdomain = 'num_MG_subdomain_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_MG_mesh_header = 'MG_mesh_header_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_elem_header = 'MG_elem_header_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_surf_header = 'MG_surf_header_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_edge_header = 'MG_edge_header_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_MG_fine_2_coarse_tbl =  'MG_fine_2_coarse_tbl_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_coarse_2_fine_tbl =  'MG_coarse_2_fine_tbl_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_f2c_ele_tbl =    'MG_fine_2_coarse_ele_tbl_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_MG_mesh_file_fmt =        'MG_mesh_file_fmt_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_tbl_file_fmt =         'MG_table_file_fmt_ctl'
!
      character(len=kchara), parameter                                  &
     &       :: hd_MG_METHOD =        'MG_METHOD_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_PRECOND =       'MG_PRECOND_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_maxiter_mid =      'maxiter_mid_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_maxiter_coarsest = 'maxiter_coarsest_ctl'
      character(len=kchara), parameter                                  &
     &       :: hd_MG_residual =      'MG_residual_ctl'
!
!
      integer(kind = kint) :: i_num_MG_level =     0
!
      integer(kind = kint) :: i_MG_METHOD = 0
      integer(kind = kint) :: i_MG_PRECOND = 0
      integer(kind = kint) :: i_maxiter_mid = 0
      integer(kind = kint) :: i_maxiter_coarsest = 0
      integer(kind = kint) :: i_MG_residual = 0
!
!
      private :: hd_Multigrid_params, i_Multigrid_params
      private :: hd_num_MG_level, hd_num_MG_subdomain
      private :: hd_MG_mesh_header, hd_MG_elem_header
      private :: hd_MG_surf_header, hd_MG_edge_header
      private :: hd_MG_fine_2_coarse_tbl, hd_MG_coarse_2_fine_tbl
      private :: hd_MG_mesh_file_fmt, hd_MG_tbl_file_fmt
      private :: hd_MG_f2c_ele_tbl
      private :: hd_MG_METHOD, hd_MG_PRECOND, hd_MG_residual
      private :: hd_maxiter_mid, hd_maxiter_coarsest
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_data_4_Multigrid(ierr)
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(right_begin_flag(hd_Multigrid_params) .eq. 0) return
      if (i_Multigrid_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_Multigrid_params,                 &
     &                             i_Multigrid_params)
        if(i_Multigrid_params .gt. 0) exit
!
!
        call read_character_ctl_item(hd_MG_METHOD,                      &
     &          i_MG_METHOD, MG_METHOD_ctl)
        call read_character_ctl_item(hd_MG_PRECOND,                     &
     &          i_MG_PRECOND, MG_PRECOND_ctl)
!
        call read_real_ctl_item(hd_MG_residual,                         &
     &          i_MG_residual, MG_residual_ctl)
!
        call read_integer_ctl_item(hd_maxiter_mid,                      &
     &        i_maxiter_mid, maxiter_mid_ctl)
        call read_integer_ctl_item(hd_maxiter_coarsest,                 &
     &        i_maxiter_coarsest, maxiter_coarsest_ctl)
!
        call read_integer_ctl_item(hd_num_MG_level,                     &
     &        i_num_MG_level, num_multigrid_level_ctl)
!
!
        call read_control_array_i1                                      &
     &     (hd_num_MG_subdomain, num_MG_subdomain_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_mesh_header, MG_mesh_prefix_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_elem_header, MG_elem_prefix_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_surf_header, MG_surf_prefix_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_edge_header, MG_edge_prefix_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_fine_2_coarse_tbl, MG_fine_2_coarse_tbl_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_coarse_2_fine_tbl, MG_coarse_2_fine_tbl_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_f2c_ele_tbl, MG_f2c_ele_tbl_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_mesh_file_fmt, MG_mesh_fmt_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_tbl_file_fmt, MG_table_fmt_ctl)
      end do
!
      end subroutine read_ctl_data_4_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module  m_ctl_data_4_Multigrid
