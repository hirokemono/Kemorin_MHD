!>@file   t_ctl_data_4_Multigrid.f90
!!@brief  module t_ctl_data_4_Multigrid
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for reading parameters for MGCG
!!
!!@verbatim
!!      subroutine dealloc_control_Multigrid(MG_ctl)
!!      subroutine read_control_Multigrid(hd_block, iflag, MG_ctl)
!!        type(MGCG_control), intent(inout) :: MG_ctl
!!      subroutine write_control_Multigrid                              &
!!     &         (id_file, hd_block, MG_ctl, level)
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
      use t_control_elements
      use t_read_control_arrays
!
      implicit  none
!
!
!>        Structure for MGCG control
      type MGCG_control
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
      end type MGCG_control
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
      private :: hd_num_MG_level, hd_num_MG_subdomain
      private :: hd_MG_mesh_header
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
      end subroutine dealloc_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_Multigrid(hd_block, iflag, MG_ctl)
!
      use m_machine_parameter
      use m_read_control_elements
      use write_control_arrays
!
      character(len=kchara), intent(in) :: hd_block
!
      integer(kind = kint), intent(inout) :: iflag
      type(MGCG_control), intent(inout) :: MG_ctl
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
        call read_chara_ctl_type(hd_MG_METHOD, MG_ctl%MG_METHOD_ctl)
        call read_chara_ctl_type(hd_MG_PRECOND, MG_ctl%MG_PRECOND_ctl)
!
        call read_real_ctl_type(hd_MG_residual, MG_ctl%MG_residual_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_maxiter_mid, MG_ctl%maxiter_mid_ctl)
        call read_integer_ctl_type                                      &
     &     (hd_maxiter_coarsest, MG_ctl%maxiter_coarsest_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_num_MG_level, MG_ctl%num_multigrid_level_ctl)
!
!
        call read_control_array_i1                                      &
     &     (hd_num_MG_subdomain, MG_ctl%num_MG_subdomain_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_mesh_header, MG_ctl%MG_mesh_prefix_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_fine_2_coarse_tbl, MG_ctl%MG_fine_2_coarse_tbl)
        call read_control_array_c1                                      &
     &     (hd_MG_coarse_2_fine_tbl, MG_ctl%MG_coarse_2_fine_tbl)
        call read_control_array_c1                                      &
     &     (hd_MG_f2c_ele_tbl, MG_ctl%MG_f2c_ele_tbl_ctl)
!
        call read_control_array_c1                                      &
     &     (hd_MG_mesh_file_fmt, MG_ctl%MG_mesh_fmt_ctl)
        call read_control_array_c1                                      &
     &     (hd_MG_tbl_file_fmt, MG_ctl%MG_table_fmt_ctl)
      end do
!
      end subroutine read_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      subroutine write_control_Multigrid                                &
     &         (id_file, hd_block, MG_ctl, level)
!
      use m_machine_parameter
      use m_read_control_elements
      use write_control_elements
      use write_control_arrays
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: hd_block
      type(MGCG_control), intent(in) :: MG_ctl
!
      integer(kind = kint), intent(inout) :: level
!
      integer(kind = kint) :: maxlen = 0
!
!
      maxlen = max(maxlen, len_trim(hd_MG_METHOD))
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
      write(id_file,'(a1)') '!'
      level = write_begin_flag_for_ctl(id_file, level, hd_block)
!
        call write_chara_ctl_type(id_file, level, maxlen,               &
     &      hd_MG_METHOD, MG_ctl%MG_METHOD_ctl)
        call write_chara_ctl_type(id_file, level, maxlen,               &
     &      hd_MG_PRECOND, MG_ctl%MG_PRECOND_ctl)
!
        call write_real_ctl_type(id_file, level, maxlen,                &
     &      hd_MG_residual, MG_ctl%MG_residual_ctl)
!
        call write_integer_ctl_type(id_file, level, maxlen,             &
     &      hd_maxiter_mid, MG_ctl%maxiter_mid_ctl)
        call write_integer_ctl_type(id_file, level, maxlen,             &
     &      hd_maxiter_coarsest, MG_ctl%maxiter_coarsest_ctl)
!
        call write_integer_ctl_type(id_file, level, maxlen,             &
     &      hd_num_MG_level, MG_ctl%num_multigrid_level_ctl)
!
!
        call write_control_array_i1(id_file, level,                     &
     &      hd_num_MG_subdomain, MG_ctl%num_MG_subdomain_ctl)
!
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_mesh_header, MG_ctl%MG_mesh_prefix_ctl)
!
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_fine_2_coarse_tbl, MG_ctl%MG_fine_2_coarse_tbl)
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_coarse_2_fine_tbl, MG_ctl%MG_coarse_2_fine_tbl)
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_f2c_ele_tbl, MG_ctl%MG_f2c_ele_tbl_ctl)
!
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_mesh_file_fmt, MG_ctl%MG_mesh_fmt_ctl)
        call write_control_array_c1(id_file, level,                     &
     &      hd_MG_tbl_file_fmt, MG_ctl%MG_table_fmt_ctl)
!
      level =  write_end_flag_for_ctl(id_file, level, hd_block)
!
      end subroutine write_control_Multigrid
!
!  ---------------------------------------------------------------------
!
      end module  t_ctl_data_4_Multigrid
