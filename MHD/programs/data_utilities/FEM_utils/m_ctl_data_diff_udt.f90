!
!      module m_ctl_data_diff_udt
!
!      Written by H. Matsui on Nov., 2006
!
!     required module for 3rd level
!
!      subroutine read_control_4_diff_udt
!      subroutine read_control_4_ave_udt
!      subroutine read_control_4_corr_udt
!      subroutine read_control_med_grp_patch
!
      module m_ctl_data_diff_udt
!
      use m_precision
      use m_parallel_var_dof
      use m_machine_parameter
      use m_read_control_elements
      use skip_comment_f
!
!
      implicit  none
!
!
      integer(kind = kint), parameter :: diff_ctl_file_code = 11
!
!
      character(len = kchara), parameter                                &
     &                 :: fname_diff_ctl = "ctl_diff_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_ave_ctl =  "ctl_ave_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_prod_ctl = "ctl_prod_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_corr_ctl = "ctl_correlate_udt"
      character(len = kchara), parameter                                &
     &                 :: fname_grp_patch_ctl = "ctl_med_group_patch"
!
!
      character(len = kchara) :: ref_udt_head_ctl = "field/out"
      character(len = kchara) :: tgt_udt_head_ctl = "field/out"
!
      character(len = kchara) :: product_field_ctl =  "velocity"
      character(len = kchara) :: correlate_fld_ctl =  "velocity"
      character(len = kchara) :: correlate_cmp_ctl =  "norm"
!
      character(len = kchara) :: correlate_coord_ctl = "Cartesian"
!
      character(len = kchara) :: group_mesh_head_ctl = "grouping_mesh"
!
!     Top level for difference
      character(len=kchara), parameter ::                               &
     &                   hd_diff_control = 'difference_udts'
      integer (kind=kint) :: i_diff_control = 0
!
!     Top level for average
      character(len=kchara), parameter ::                               &
     &                   hd_ave_control = 'averaging_udts'
!
!     Top level for correlation
      character(len=kchara), parameter ::                               &
     &                  hd_corr_control = 'correlate_udts'
!
!     Top level for meridional patch
      character(len=kchara), parameter ::                               &
     &                  hd_med_grp_patch = 'meridional_group_patch'
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_diff_files = 'data_file_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_diff_model = 'models'
!
      integer (kind=kint) :: i_diff_files =  0
      integer (kind=kint) :: i_diff_model =  0
!
!     3rd level for file header
!
      character(len=kchara), parameter                                  &
     &         :: hd_ref_udt_head_ctl = 'reference_field_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_tgt_udt_head_ctl = 'target_field_header_ctl'
!
      integer (kind=kint) :: i_ref_udt_head_ctl = 0
      integer (kind=kint) :: i_tgt_udt_head_ctl = 0
!
!     3rd level for fields
!
      character(len=kchara), parameter                                  &
     &      :: hd_prod_name =     'product_field_name_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_corr_fld_name = 'correlate_field_name_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_corr_cmp_name = 'correlate_component_name_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_correlate_coord = 'correlate_coordinate_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_group_mesh_head = 'grouping_mesh_head_ctl'
!
      integer (kind=kint) :: i_prod_name =       0
      integer (kind=kint) :: i_corr_fld_name =   0
      integer (kind=kint) :: i_corr_cmp_name =   0
      integer (kind=kint) :: i_correlate_coord = 0
      integer (kind=kint) :: i_group_mesh_head = 0
!
      private :: diff_ctl_file_code
      private :: fname_diff_ctl, fname_ave_ctl
      private :: fname_prod_ctl, fname_corr_ctl, fname_grp_patch_ctl
!
      private :: hd_diff_control, i_diff_control
      private :: hd_ave_control, hd_corr_control
      private :: hd_diff_files, hd_diff_model
      private :: hd_ref_udt_head_ctl, hd_tgt_udt_head_ctl
      private :: hd_correlate_coord
      private :: hd_corr_fld_name, hd_corr_cmp_name
      private :: hd_prod_name, hd_group_mesh_head
      private :: i_diff_files, i_diff_model
!
      private :: read_diff_control_data
      private :: read_diff_files_ctl, read_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_diff_udt
!
!
      ctl_file_code = diff_ctl_file_code
      open(ctl_file_code, file=fname_diff_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_diff_control_data(hd_diff_control)
      close(ctl_file_code)
!
      end subroutine read_control_4_diff_udt
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_ave_udt
!
!
      ctl_file_code = diff_ctl_file_code
      open(ctl_file_code, file=fname_ave_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_diff_control_data(hd_ave_control)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_ave_udt
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_corr_udt
!
!
      ctl_file_code = diff_ctl_file_code
      open(ctl_file_code, file=fname_corr_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_diff_control_data(hd_corr_control)
!
      close(ctl_file_code)
!
      end subroutine read_control_4_corr_udt
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_med_grp_patch
!
!
      ctl_file_code = diff_ctl_file_code
      open(ctl_file_code, file=fname_grp_patch_ctl, status='old')
!
!
      call load_ctl_label_and_line
      call read_diff_control_data(hd_med_grp_patch)
!
      close(ctl_file_code)
!
      end subroutine read_control_med_grp_patch
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_diff_control_data(hd_entry)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
      character(len=kchara), intent(in) :: hd_entry
!
!
      if(right_begin_flag(hd_entry) .eq. 0) return
      if (i_diff_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_entry, i_diff_control)
        if(i_diff_control .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
!
        call read_diff_files_ctl
        call read_diff_model_ctl
      end do
!
      end subroutine read_diff_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_diff_files_ctl
!
!
      if(right_begin_flag(hd_diff_files) .eq. 0) return
      if (i_diff_files.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_diff_files, i_diff_files)
        if(i_diff_files .gt. 0) exit
!
!
        call read_character_ctl_item(hd_ref_udt_head_ctl,               &
     &        i_ref_udt_head_ctl, ref_udt_head_ctl)
        call read_character_ctl_item(hd_tgt_udt_head_ctl,               &
     &        i_tgt_udt_head_ctl, tgt_udt_head_ctl)
      end do
!
      end subroutine read_diff_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_diff_model_ctl
!
      use m_ctl_data_4_fields
      use m_ctl_data_4_time_steps
      use m_ctl_data_ele_layering
      use m_ctl_data_4_fem_int_pts
!
!
      if(right_begin_flag(hd_diff_model) .eq. 0) return
      if (i_diff_model .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_diff_model, i_diff_model)
        if(i_diff_model .gt. 0) exit
!
!
        call read_phys_values
        call read_time_step_ctl
        call read_ele_layers_grp_ctl
        call read_fem_int_points_ctl
!
!
        call read_character_ctl_item(hd_prod_name,                      &
     &            i_prod_name, product_field_ctl)
        call read_character_ctl_item(hd_corr_fld_name,                  &
     &            i_corr_fld_name, correlate_fld_ctl)
        call read_character_ctl_item(hd_corr_cmp_name,                  &
     &            i_corr_cmp_name, correlate_cmp_ctl)
        call read_character_ctl_item(hd_correlate_coord,                &
     &            i_correlate_coord, correlate_coord_ctl)
        call read_character_ctl_item(hd_group_mesh_head,                &
     &            i_group_mesh_head, group_mesh_head_ctl)
      end do
!
      end subroutine read_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_diff_udt
