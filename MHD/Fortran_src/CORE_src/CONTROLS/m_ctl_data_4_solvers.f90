!
!      module m_ctl_data_4_solvers
!
!        programmed by H.Matsui on June, 2007
!
!      subroutine read_crs_solver_param_ctl(ierr, e_message)
!      subroutine read_DJDS_solver_param_ctl
!
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
!
!    begin solver_ctl
!      itr_ctl              1000
!      eps_ctl              1.0E-4
!      sigma_ctl               1.0
!      sigma_diag_ctl          1.0
!      method_ctl              CG
!      precond_ctl             SSOR
!
!      begin DJDS_solver_ctl
!        order_method     RCM_DJDS
!        min_color_ctl    60
!        mc_color_ctl     100
!      end
!
!      begin MGCG_parameter_ctl
!        MG_METHOD_ctl     CG
!        MG_PRECOND_ctl    DIAG
!
!        maxiter_mid_ctl          2
!        maxiter_coarsest_ctl    30
!        MG_residual_ctl         1.0e-20
!
!        num_multigrid_level_ctl           2
!
!        array num_MG_subdomain_ctl    2
!          num_MG_subdomain_ctl           8
!          num_MG_subdomain_ctl           2
!        end array
!
!        array MG_mesh_header_ctl    2
!          MG_mesh_header_ctl          'mesh_1st/in'
!          MG_mesh_header_ctl          'mesh_2nd/in'
!        end array
!
!        array MG_elem_header_ctl    2
!          MG_elem_header_ctl          'mesh_1st/in_element'
!          MG_elem_header_ctl          'mesh_2nd/in_element'
!        end array
!
!        array MG_surf_header_ctl    2
!          MG_surf_header_ctl          'mesh_1st/in_surface'
!          MG_surf_header_ctl          'mesh_2nd/in_surface'
!        end array
!
!        array MG_edge_header_ctl    2
!          MG_edge_header_ctl          'mesh_1st/in_edge'
!          MG_edge_header_ctl          'mesh_2nd/in_edge'
!        end array
!
!
!        array MG_fine_2_coarse_tbl_ctl    2
!          MG_fine_2_coarse_tbl_ctl          'mesh_1st/fine_2_coarse'
!          MG_fine_2_coarse_tbl_ctl          'mesh_2nd/fine_2_coarse'
!        end array
!
!        array MG_coarse_2_fine_tbl_ctl    2
!          MG_coarse_2_fine_tbl_ctl          'mesh_1st/coarse_2_fine'
!          MG_coarse_2_fine_tbl_ctl          'mesh_2nd/coarse_2_fine'
!        end array
!
!        array MG_fine_2_coarse_ele_tbl_ctl    2
!          MG_fine_2_coarse_ele_tbl_ctl      'mesh_1st/fine_2_coarse_ele'
!          MG_fine_2_coarse_ele_tbl_ctl      'mesh_2nd/fine_2_coarse_ele'
!        end array
!
!
!        array MG_mesh_file_fmt_ctl    2
!          MG_mesh_file_fmt_ctl           'ascii'
!          MG_mesh_file_fmt_ctl           'ascii'
!        end array
!
!        array MG_table_file_fmt_ctl    2
!          MG_table_file_fmt_ctl           'ascii'
!          MG_table_file_fmt_ctl           'ascii'
!        end array
!      end
!    end
!
      module m_ctl_data_4_solvers
!
      use m_precision
!
      implicit  none
!
      integer(kind=kint) :: itr_ctl
      integer(kind=kint) :: nset_ctl
!
      real(kind=kreal) :: eps_ctl
      real(kind=kreal) :: sigma_ctl
      real(kind=kreal) :: sigma_diag_ctl
! 
      character(len=kchara) :: method_ctl
      character(len=kchara) :: precond_ctl
!
!
      integer(kind=kint) :: min_color_ctl
      integer(kind=kint) :: mc_color_ctl
      character (len=kchara)   :: order_method_ctl
! 
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_solver_ctl =     'solver_ctl'
      integer (kind=kint) :: i_solver_ctl =     0
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
      character(len=kchara), parameter :: hd_nset =       'nset_ctl'
!
!
      integer (kind=kint) :: i_itr =        0
      integer (kind=kint) :: i_eps =        0
      integer (kind=kint) :: i_sigma =      0
      integer (kind=kint) :: i_sigma_diag = 0
      integer (kind=kint) :: i_method =     0
      integer (kind=kint) :: i_precond =    0
      integer (kind=kint) :: i_nset =       0
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
      integer (kind=kint) :: i_order_method = 0
      integer (kind=kint) :: i_min_color =    0
      integer (kind=kint) :: i_mc_color =     0
!
      private :: hd_solver_ctl, hd_DJDS_params
      private :: i_solver_ctl,  i_DJDS_params
      private :: hd_itr, hd_eps, hd_sigma, hd_sigma_diag
      private :: hd_method, hd_precond, hd_nset
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine read_crs_solver_param_ctl(ierr)
!
      use m_machine_parameter
      use m_read_control_elements
      use m_ctl_data_4_Multigrid
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(right_begin_flag(hd_solver_ctl) .eq. 0) return
      if (i_solver_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_solver_ctl, i_solver_ctl)
        if(i_solver_ctl .gt. 0) exit
!
!
        call read_DJDS_solver_param_ctl
        call read_ctl_data_4_Multigrid(ierr)
        if(ierr .gt. 0) return
!
!
        call read_real_ctl_item(hd_eps, i_eps, eps_ctl)
        call read_real_ctl_item(hd_sigma, i_sigma, sigma_ctl)
        call read_real_ctl_item(hd_sigma_diag, i_sigma_diag,            &
     &        sigma_diag_ctl)
!
        call read_integer_ctl_item(hd_itr, i_itr, itr_ctl)
        call read_integer_ctl_item(hd_nset, i_nset, nset_ctl)
!
        call read_character_ctl_item(hd_method, i_method, method_ctl)
        call read_character_ctl_item(hd_precond, i_precond,             &
     &          precond_ctl)
      end do
!
      end subroutine read_crs_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_DJDS_solver_param_ctl
!
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      if(right_begin_flag(hd_DJDS_params) .eq. 0) return
      if (i_DJDS_params .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_DJDS_params, i_DJDS_params)
        if(i_DJDS_params .gt. 0) exit
!
!
        call read_integer_ctl_item(hd_min_color,                        &
     &        i_min_color, min_color_ctl)
        call read_integer_ctl_item(hd_mc_color,                         &
     &        i_mc_color, mc_color_ctl)
!
        call read_character_ctl_item(hd_order_method,                   &
     &        i_order_method, order_method_ctl)
      end do
!
      end subroutine read_DJDS_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      end module m_ctl_data_4_solvers
