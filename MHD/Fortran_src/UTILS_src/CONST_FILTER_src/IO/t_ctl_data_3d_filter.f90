!
!      module t_ctl_data_3d_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_filter_area_ctl(fil3_ctl)
!!      subroutine read_element_size_ctl(fil3_ctl)
!!      subroutine read_dx_solver_param_ctl(fil3_ctl)
!!      subroutine dealloc_dx_solver_param_ctl(fil3_ctl)
!!        type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!!
!!      subroutine read_org_filter_fnames_ctl(org_fil_files_ctl)
!!        type(org_filter_prefix_ctls), intent(inout)                   &
!!       &                             :: org_fil_files_ctl
!!
!!
!!      begin org_filter_filtes_ctl
!!        org_filter_file_header       'org/filter_node'
!!        org_filter_elength_header    'org/filter_elength'
!!        org_filter_moment_header     'org/filter_moms'
!!        org_filter_coefs_header      'org/filter_coef'
!!      end org_filter_filtes_ctl
!!
      module t_ctl_data_3d_filter
!
      use m_precision
      use m_read_control_elements
      use m_machine_parameter
      use t_ctl_data_gen_filter
      use t_ctl_data_4_platforms
      use t_control_array_character
      use t_ctl_data_filter_files
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      type ctl_data_3d_filter
!>        Structure for file settings
        type(platform_data_control) :: gen_filter_plt
!>        Structure for filtering groups
!!@n        filter_area_ctl%c_tbl: Name of force
        type(ctl_array_chara) :: filter_area_ctl
!>        Structure for filtering files
        type(filter_file_control) :: ffile_3d_ctl
!
        type(read_character_item) :: mass_matrix_type_ctl
!
        type(read_character_item) :: method_esize_ctl
        type(read_character_item) :: precond_esize_ctl
        type(read_integer_item) :: itr_esize_ctl
        type(read_real_item) :: eps_esize_ctl
        type(read_real_item) :: sigma_esize_ctl
        type(read_real_item) :: sigma_diag_esize_ctl
      end type ctl_data_3d_filter
!
!
      type org_filter_prefix_ctls
        type(read_character_item) :: org_filter_head_ctl
        type(read_character_item) :: org_filter_coef_head_ctl
        type(read_character_item) :: org_filter_elen_head_ctl
        type(read_character_item) :: org_filter_moms_head_ctl
      end type org_filter_prefix_ctls
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_fnames =  'orginal_filter_files_ctl'
      integer (kind=kint) :: i_org_filter_fnames =  0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_area_ctl =  'filter_area_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_deltax_ctl =       'element_size_ctl'
!
      integer (kind=kint) :: i_filter_area_ctl =    0
      integer (kind=kint) :: i_deltax_ctl =         0
!
!     3rd level for filter_area
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_area = 'filter_ele_grp_ctl'
!
!     3rd level for mass matrix
!
      character(len=kchara), parameter                                  &
     &         :: hd_mass_matrix_type = 'mass_matrix_type_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_esize_solver = 'esize_solver_control'
      integer (kind=kint) :: i_esize_solver_ctl =  0
!
!     4th level for solver_control for element size
!
      character(len=kchara), parameter                                  &
     &         :: hd_method_esize =     'method_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_precond_esize =    'precond_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_itr_esize =        'itr_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_eps_esize =        'eps_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_sigma_esize =      'sigma_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_sigma_diag_esize = 'sigma_diag_ctl'
!
!     flags for filter file headers
!
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_head =      'org_filter_file_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_elen_head = 'org_filter_elength_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_moms_head = 'org_filter_moment_header'
      character(len=kchara), parameter                                  &
     &         :: hd_org_filter_coef_head = 'org_filter_coefs_header'
!
!
      private :: hd_filter_area_ctl, hd_deltax_ctl
      private :: hd_mass_matrix_type, hd_esize_solver, hd_filter_area
      private :: hd_method_esize, hd_precond_esize, hd_itr_esize
      private :: hd_eps_esize, hd_sigma_esize, hd_sigma_diag_esize
      private :: i_esize_solver_ctl, i_deltax_ctl, i_filter_area_ctl
!
      private :: hd_org_filter_fnames, i_org_filter_fnames
      private :: hd_org_filter_head, hd_org_filter_elen_head
      private :: hd_org_filter_moms_head, hd_org_filter_coef_head
!
      private :: read_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_filter_area_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      if(right_begin_flag(hd_filter_area_ctl) .eq. 0) return
      if (i_filter_area_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_area_ctl = find_control_end_flag(hd_filter_area_ctl)
        if(i_filter_area_ctl .gt. 0) exit
!
        call read_control_array_c1(ctl_file_code,                       &
     &      hd_filter_area, fil3_ctl%filter_area_ctl, c_buf1)
      end do
!
      end subroutine read_filter_area_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_element_size_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      if(right_begin_flag(hd_deltax_ctl) .eq. 0) return
      if (i_deltax_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_deltax_ctl = find_control_end_flag(hd_deltax_ctl)
        if(i_deltax_ctl .gt. 0) exit
!
        call read_dx_solver_param_ctl(fil3_ctl)
!
        call read_chara_ctl_type(hd_mass_matrix_type,                   &
     &      fil3_ctl%mass_matrix_type_ctl)
      end do
!
      end subroutine read_element_size_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_dx_solver_param_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      if(right_begin_flag(hd_esize_solver) .eq. 0) return
      if (i_esize_solver_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_esize_solver_ctl = find_control_end_flag(hd_esize_solver)
        if(i_esize_solver_ctl .gt. 0) exit
!
!
        call read_chara_ctl_type                                        &
     &     (hd_method_esize, fil3_ctl%method_esize_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_precond_esize, fil3_ctl%precond_esize_ctl)
!
        call read_real_ctl_type                                         &
     &     (hd_eps_esize, fil3_ctl%eps_esize_ctl)
        call read_real_ctl_type                                         &
     &     (hd_sigma_esize, fil3_ctl%sigma_esize_ctl)
        call read_real_ctl_type(hd_sigma_diag_esize,                    &
     &      fil3_ctl%sigma_diag_esize_ctl)
!
        call read_integer_ctl_type                                      &
     &     (hd_itr_esize, fil3_ctl%itr_esize_ctl)
      end do
!
      end subroutine read_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_dx_solver_param_ctl(fil3_ctl)
!
      type(ctl_data_3d_filter), intent(inout) :: fil3_ctl
!
!
      call dealloc_control_array_chara(fil3_ctl%filter_area_ctl)
!
      fil3_ctl%mass_matrix_type_ctl%iflag = 0
!
      fil3_ctl%method_esize_ctl%iflag =     0
      fil3_ctl%precond_esize_ctl%iflag =    0
      fil3_ctl%itr_esize_ctl%iflag =        0
      fil3_ctl%eps_esize_ctl%iflag =        0
      fil3_ctl%sigma_esize_ctl%iflag =      0
      fil3_ctl%sigma_diag_esize_ctl%iflag = 0
!
      end subroutine dealloc_dx_solver_param_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_org_filter_fnames_ctl(org_fil_files_ctl)
!
      type(org_filter_prefix_ctls), intent(inout) :: org_fil_files_ctl
!
!
      if(right_begin_flag(hd_org_filter_fnames) .eq. 0) return
      if (i_org_filter_fnames .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_org_filter_fnames                                             &
     &      = find_control_end_flag(hd_org_filter_fnames)
        if(i_org_filter_fnames .gt. 0) exit
!
!
        call read_chara_ctl_type(hd_org_filter_head,                    &
     &      org_fil_files_ctl%org_filter_head_ctl)
        call read_chara_ctl_type(hd_org_filter_coef_head,               &
     &      org_fil_files_ctl%org_filter_coef_head_ctl)
        call read_chara_ctl_type(hd_org_filter_elen_head,               &
     &      org_fil_files_ctl%org_filter_elen_head_ctl)
        call read_chara_ctl_type(hd_org_filter_moms_head,               &
     &      org_fil_files_ctl%org_filter_moms_head_ctl)
      end do
!
      end subroutine read_org_filter_fnames_ctl
!
!  ---------------------------------------------------------------------
!
      end module t_ctl_data_3d_filter
