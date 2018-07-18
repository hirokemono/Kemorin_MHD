!
!      module t_ctl_data_gen_3d_filter
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_control_4_gen_filter(fil3_ctl)
!!      subroutine read_control_4_sort_filter(fil3_ctl)
!!      subroutine dealloc_dx_solver_param_ctl(fil3_ctl)
!!        type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!!
!
      module t_ctl_data_gen_3d_filter
!
      use m_precision
      use m_ctl_data_gen_filter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_read_control_arrays
      use t_ctl_data_filter_files
      use t_control_elements
      use skip_comment_f
!
      implicit  none
!
!
      integer(kind = kint), parameter :: filter_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_filter_ctl = "ctl_filter"
      character(len = kchara), parameter                                &
     &                        :: fname_sort_flt_ctl = "ctl_sort_filter"
!
!
      type ctl_data_gen_3d_filter
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
      end type ctl_data_gen_3d_filter
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &         :: hd_filter_control = 'const_filter'
      integer (kind=kint) :: i_filter_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &         :: hd_filter_area_ctl =  'filter_area_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_deltax_ctl =       'element_size_ctl'
!
      integer (kind=kint) :: i_platform =   0
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
      private :: hd_filter_control, i_filter_control
      private :: hd_filter_area_ctl, hd_deltax_ctl
      private :: hd_platform, i_platform
      private :: hd_mass_matrix_type, hd_esize_solver, hd_filter_area
      private :: hd_method_esize, hd_precond_esize, hd_itr_esize
      private :: hd_eps_esize, hd_sigma_esize, hd_sigma_diag_esize
      private :: i_esize_solver_ctl, i_deltax_ctl, i_filter_area_ctl
!
      character(len=kchara), parameter :: hd_filter_fnames              &
     &                        = 'filter_files_def'
      integer (kind=kint) :: i_filter_fnames = 0
!
      private :: hd_filter_fnames, i_filter_fnames
      private :: read_dx_solver_param_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_gen_filter(fil3_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_filter_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data(fil3_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_gen_filter
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_sort_filter(fil3_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!
!
      ctl_file_code = filter_ctl_file_code
!
      open(ctl_file_code, file=fname_sort_flt_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_const_filter_ctl_data(fil3_ctl)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_sort_filter
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_const_filter_ctl_data(fil3_ctl)
!
      use m_ctl_data_gen_filter
      use m_ctl_data_org_filter_name
!
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
!
!
      if(right_begin_flag(hd_filter_control) .eq. 0) return
      if (i_filter_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        i_filter_control = find_control_end_flag(hd_filter_control)
        if(i_filter_control .gt. 0) exit
!
!
        call read_control_platforms                                     &
     &     (hd_platform, i_platform, fil3_ctl%gen_filter_plt)
!
        call read_filter_param_ctl
        call read_filter_fnames_control                                 &
     &     (hd_filter_fnames, i_filter_fnames, fil3_ctl%ffile_3d_ctl)
        call read_org_filter_fnames_ctl
!
        call read_filter_area_ctl(fil3_ctl)
        call read_element_size_ctl(fil3_ctl)
      end do
!
      end subroutine read_const_filter_ctl_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_filter_area_ctl(fil3_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
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
        call read_control_array_c1                                      &
     &     (hd_filter_area, fil3_ctl%filter_area_ctl)
      end do
!
      end subroutine read_filter_area_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_element_size_ctl(fil3_ctl)
!
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
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
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
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
      type(ctl_data_gen_3d_filter), intent(inout) :: fil3_ctl
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
!
      end module t_ctl_data_gen_3d_filter
