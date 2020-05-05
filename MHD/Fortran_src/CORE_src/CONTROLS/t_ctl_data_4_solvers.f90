!>@file   t_ctl_data_4_solvers.f90
!!        module t_ctl_data_4_solvers
!!
!!@author H. Matsui
!!@date Programmed in June, 2007
!!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine dealloc_CG_solver_param_ctl(CG_ctl)
!!
!!      subroutine read_CG_solver_param_ctl                             &
!!     &         (id_control, hd_block, CG_ctl, c_buf)
!!        type(solver_control), intent(inout) :: CG_ctl
!!      subroutine read_control_DJDS_solver(hd_block, iflag, DJDS_ctl)
!!        type(DJDS_control), intent(inout) :: DJDS_ctl
!!
!!      subroutine write_CG_solver_param_ctl                            &
!!     &         (id_file, hd_block, CG_ctl, level)
!!      subroutine write_control_DJDS_solver                            &
!!     &         (id_file, hd_block, DJDS_ctl, level)
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
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_integer
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
!
        integer (kind=kint) :: i_DJDS_params = 0
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
!
        integer (kind=kint) :: i_solver_ctl =     0
      end type solver_control
!
!
!  labels for entry groups
!
      character(len=kchara), parameter                                  &
     &       :: hd_DJDS_params =      'DJDS_solver_ctl'
!
      character(len=kchara)                                             &
     &       :: hd_Multigrid_params = 'MGCG_parameter_ctl'
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
      private :: hd_DJDS_params
      private :: hd_Multigrid_params
      private :: hd_itr, hd_eps, hd_sigma, hd_sigma_diag
      private :: hd_method, hd_precond
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_CG_solver_param_ctl(CG_ctl)
!
      type(solver_control), intent(inout) :: CG_ctl
!
!
      call dealloc_control_Multigrid(CG_ctl%MG_ctl)
!
      end subroutine dealloc_CG_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_CG_solver_param_ctl                               &
     &         (id_control, hd_block, CG_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(solver_control), intent(inout) :: CG_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(CG_ctl%i_solver_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_DJDS_solver                                   &
     &     (id_control, hd_DJDS_params, CG_ctl%DJDS_ctl, c_buf)
        call read_control_Multigrid                                     &
     &     (id_control, hd_Multigrid_params, CG_ctl%MG_ctl, c_buf)
!
!
        call read_real_ctl_type(c_buf, hd_eps, CG_ctl%eps_ctl)
        call read_real_ctl_type(c_buf, hd_sigma, CG_ctl%sigma_ctl)
        call read_real_ctl_type                                         &
     &     (c_buf, hd_sigma_diag, CG_ctl%sigma_diag_ctl)
!
        call read_integer_ctl_type(c_buf, hd_itr, CG_ctl%itr_ctl)
!
        call read_chara_ctl_type(c_buf, hd_method, CG_ctl%method_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_precond, CG_ctl%precond_ctl)
      end do
      CG_ctl%i_solver_ctl = 1
!
      end subroutine read_CG_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine read_control_DJDS_solver                               &
     &         (id_control, hd_block, DJDS_ctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(DJDS_control), intent(inout) :: DJDS_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(DJDS_ctl%i_DJDS_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_min_color, DJDS_ctl%min_color_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_mc_color, DJDS_ctl%mc_color_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_order_method, DJDS_ctl%order_method_ctl)
      end do
      DJDS_ctl%i_DJDS_params = 1
!
      end subroutine read_control_DJDS_solver
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_CG_solver_param_ctl                              &
     &         (id_control, hd_block, CG_ctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(solver_control), intent(in) :: CG_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      maxlen = max(maxlen, len_trim(hd_DJDS_params))
      maxlen = max(maxlen, len_trim(hd_Multigrid_params))
      maxlen = max(maxlen, len_trim(hd_eps))
      maxlen = max(maxlen, len_trim(hd_sigma))
      maxlen = max(maxlen, len_trim(hd_sigma_diag))
      maxlen = max(maxlen, len_trim(hd_itr))
      maxlen = max(maxlen, len_trim(hd_method))
      maxlen = max(maxlen, len_trim(hd_precond))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_control_DJDS_solver(id_control, hd_DJDS_params,        &
     &    CG_ctl%DJDS_ctl, level)
      call write_control_Multigrid(id_control, hd_Multigrid_params,     &
     &    CG_ctl%MG_ctl, level)
!
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_eps, CG_ctl%eps_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_sigma, CG_ctl%sigma_ctl)
      call write_real_ctl_type(id_control, level, maxlen,               &
     &    hd_sigma_diag, CG_ctl%sigma_diag_ctl)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_itr, CG_ctl%itr_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_method, CG_ctl%method_ctl)
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_precond, CG_ctl%precond_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_CG_solver_param_ctl
!
! -----------------------------------------------------------------------
!
      subroutine write_control_DJDS_solver                              &
     &         (id_control, hd_block, DJDS_ctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(DJDS_control), intent(in) :: DJDS_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
      maxlen = max(maxlen, len_trim(hd_min_color))
      maxlen = max(maxlen, len_trim(hd_mc_color))
      maxlen = max(maxlen, len_trim(hd_order_method))
!
      write(id_control,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_min_color, DJDS_ctl%min_color_ctl)
      call write_integer_ctl_type(id_control, level, maxlen,            &
     &    hd_mc_color, DJDS_ctl%mc_color_ctl)
!
      call write_chara_ctl_type(id_control, level, maxlen,              &
     &    hd_order_method, DJDS_ctl%order_method_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_DJDS_solver
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_4_solvers
