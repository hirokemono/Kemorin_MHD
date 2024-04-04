!>@file   ctl_data_Multigrid_IO.f90
!!        module ctl_data_Multigrid_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine init_Multigrid_ctl_label(hd_block, MG_ctl)
!!      subroutine read_control_Multigrid                               &
!!     &         (id_control, hd_block, MG_ctl, c_buf)
!!        type(MGCG_control), intent(inout) :: MG_ctl
!!      subroutine write_control_Multigrid                              &
!!     &         (id_control, hd_block, MG_ctl, level)
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
      module ctl_data_Multigrid_IO
!
      use m_precision
      use m_machine_parameter
      use t_control_array_character
      use t_control_array_real
      use t_control_array_character
      use t_control_array_integer
      use t_ctl_data_4_Multigrid
!
      implicit  none
!
!   file and domain controls
!
      character(len=kchara), parameter, private                         &
     &       :: hd_num_MG_level = 'num_multigrid_level_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_num_MG_subdomain = 'num_MG_subdomain_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_mesh_header = 'MG_mesh_header_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_fine_2_coarse_tbl =  'MG_fine_2_coarse_tbl_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_coarse_2_fine_tbl =  'MG_coarse_2_fine_tbl_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_f2c_ele_tbl =    'MG_fine_2_coarse_ele_tbl_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_mesh_file_fmt =        'MG_mesh_file_fmt_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_tbl_file_fmt =         'MG_table_file_fmt_ctl'
!
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_METHOD =        'MG_METHOD_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_PRECOND =       'MG_PRECOND_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_maxiter_mid =      'maxiter_mid_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_maxiter_coarsest = 'maxiter_coarsest_ctl'
      character(len=kchara), parameter, private                         &
     &       :: hd_MG_residual =      'MG_residual_ctl'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_Multigrid                                 &
     &         (id_control, hd_block, MG_ctl, c_buf)
!
      use t_read_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(MGCG_control), intent(inout) :: MG_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(MG_ctl%i_Multigrid_params .gt. 0) return
      do
        call load_one_line_from_control(id_control, hd_block, c_buf)
        if(c_buf%iend .gt. 0) exit
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_MG_METHOD, MG_ctl%MG_METHOD_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_MG_PRECOND, MG_ctl%MG_PRECOND_ctl)
!
        call read_real_ctl_type                                         &
     &     (c_buf, hd_MG_residual, MG_ctl%MG_residual_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_maxiter_mid, MG_ctl%maxiter_mid_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_maxiter_coarsest, MG_ctl%maxiter_coarsest_ctl)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_num_MG_level, MG_ctl%num_multigrid_level_ctl)
!
!
        call read_control_array_i1(id_control,                          &
     &      hd_num_MG_subdomain, MG_ctl%num_MG_subdomain_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_MG_mesh_header, MG_ctl%MG_mesh_prefix_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_MG_fine_2_coarse_tbl, MG_ctl%MG_fine_2_coarse_tbl,       &
     &      c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_MG_coarse_2_fine_tbl, MG_ctl%MG_coarse_2_fine_tbl,       &
     &      c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_MG_f2c_ele_tbl, MG_ctl%MG_f2c_ele_tbl_ctl, c_buf)
!
        call read_control_array_c1(id_control,                          &
     &      hd_MG_mesh_file_fmt, MG_ctl%MG_mesh_fmt_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_MG_tbl_file_fmt, MG_ctl%MG_table_fmt_ctl, c_buf)
      end do
      MG_ctl%i_Multigrid_params = 1
!
      end subroutine read_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_Multigrid                                &
     &         (id_control, hd_block, MG_ctl, level)
!
      use t_read_control_elements
      use write_control_elements
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
      type(MGCG_control), intent(in) :: MG_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      if(MG_ctl%i_Multigrid_params .le. 0) return
!
      maxlen = len_trim(hd_MG_METHOD)
      maxlen = max(maxlen, len_trim(hd_MG_PRECOND))
      maxlen = max(maxlen, len_trim(hd_MG_residual))
      maxlen = max(maxlen, len_trim(hd_maxiter_mid))
      maxlen = max(maxlen, len_trim(hd_maxiter_coarsest))
      maxlen = max(maxlen, len_trim(hd_num_MG_level))
      maxlen = max(maxlen, len_trim(hd_num_MG_subdomain))
      maxlen = max(maxlen, len_trim(hd_num_MG_subdomain))
      maxlen = max(maxlen, len_trim(hd_MG_mesh_header))
      maxlen = max(maxlen, len_trim(hd_MG_fine_2_coarse_tbl))
      maxlen = max(maxlen, len_trim(hd_MG_coarse_2_fine_tbl))
      maxlen = max(maxlen, len_trim(hd_MG_f2c_ele_tbl))
      maxlen = max(maxlen, len_trim(hd_MG_mesh_file_fmt))
      maxlen = max(maxlen, len_trim(hd_MG_tbl_file_fmt))
!
      level = write_begin_flag_for_ctl(id_control, level, hd_block)
!
        call write_chara_ctl_type(id_control, level, maxlen,            &
     &      MG_ctl%MG_METHOD_ctl)
        call write_chara_ctl_type(id_control, level, maxlen,            &
     &      MG_ctl%MG_PRECOND_ctl)
!
        call write_real_ctl_type(id_control, level, maxlen,             &
     &      MG_ctl%MG_residual_ctl)
!
        call write_integer_ctl_type(id_control, level, maxlen,          &
     &      MG_ctl%maxiter_mid_ctl)
        call write_integer_ctl_type(id_control, level, maxlen,          &
     &      MG_ctl%maxiter_coarsest_ctl)
!
        call write_integer_ctl_type(id_control, level, maxlen,          &
     &      MG_ctl%num_multigrid_level_ctl)
!
!
        call write_control_array_i1(id_control, level,                  &
     &      MG_ctl%num_MG_subdomain_ctl)
!
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_mesh_prefix_ctl)
!
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_fine_2_coarse_tbl)
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_coarse_2_fine_tbl)
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_f2c_ele_tbl_ctl)
!
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_mesh_fmt_ctl)
        call write_control_array_c1(id_control, level,                  &
     &      MG_ctl%MG_table_fmt_ctl)
!
      level =  write_end_flag_for_ctl(id_control, level, hd_block)
!
      end subroutine write_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      subroutine init_Multigrid_ctl_label(hd_block, MG_ctl)
!
      character(len=kchara), intent(in) :: hd_block
      type(MGCG_control), intent(inout) :: MG_ctl
!
!
      MG_ctl%block_name = hd_block
        call init_chara_ctl_item_label                                  &
     &     (hd_MG_METHOD, MG_ctl%MG_METHOD_ctl)
        call init_chara_ctl_item_label                                  &
     &     (hd_MG_PRECOND, MG_ctl%MG_PRECOND_ctl)
!
        call init_real_ctl_item_label                                   &
     &     (hd_MG_residual, MG_ctl%MG_residual_ctl)
!
        call init_int_ctl_item_label                                    &
     &     (hd_maxiter_mid, MG_ctl%maxiter_mid_ctl)
        call init_int_ctl_item_label                                    &
     &     (hd_maxiter_coarsest, MG_ctl%maxiter_coarsest_ctl)
!
        call init_int_ctl_item_label                                    &
     &     (hd_num_MG_level, MG_ctl%num_multigrid_level_ctl)
!
!
        call init_int_ctl_array_label                                   &
     &     (hd_num_MG_subdomain, MG_ctl%num_MG_subdomain_ctl)
!
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_mesh_header, MG_ctl%MG_mesh_prefix_ctl)
!
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_fine_2_coarse_tbl, MG_ctl%MG_fine_2_coarse_tbl)
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_coarse_2_fine_tbl, MG_ctl%MG_coarse_2_fine_tbl)
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_f2c_ele_tbl, MG_ctl%MG_f2c_ele_tbl_ctl)
!
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_mesh_file_fmt, MG_ctl%MG_mesh_fmt_ctl)
        call init_chara_ctl_array_label                                 &
     &     (hd_MG_tbl_file_fmt, MG_ctl%MG_table_fmt_ctl)
!
      end subroutine init_Multigrid_ctl_label
!
!  ---------------------------------------------------------------------
!
      end module  ctl_data_Multigrid_IO
