!
!      module m_ctl_data_product_udt
!
!      Written by H. Matsui on Nov., 2006
!
!      subroutine read_control_4_diff_udt
!      subroutine read_control_4_prod_udt
!
!
      module m_ctl_data_product_udt
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use m_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_time_steps
      use t_control_elements
      use skip_comment_f
!
!
      implicit  none
!
!
      integer(kind = kint), parameter :: prod_ctl_file_code = 11
!
!
      character(len = kchara), parameter                                &
     &                 :: fname_prod_ctl = "ctl_prod_udt"
!
!>      Structure for file names
      type(platform_data_control), save :: pu_plt
!>      Structure for original file names
      type(platform_data_control), save :: org_pu_plt
!>      Structure for time stepping control
      type(time_data_control), save :: t_pu_ctl
!
      type(read_character_item), save :: product_udt_1_head_ctl
      type(read_character_item), save :: product_udt_2_head_ctl
!
      type(read_character_item), save :: product_field_1_ctl
      type(read_character_item), save :: product_field_2_ctl
      type(read_character_item), save :: result_field_ctl
!
      type(read_character_item), save :: product_type_ctl
!     Top level for products
      character(len=kchara), parameter ::                               &
     &                  hd_prod_control = 'products_udts'
      integer (kind=kint) :: i_prod_control = 0
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_prod_files = 'data_file_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_prod_model = 'models'
!
      integer (kind=kint) :: i_prod_files =  0
      integer (kind=kint) :: i_prod_model =  0
!
!     3rd level for file header
!
      character(len=kchara), parameter                                  &
     &      :: hd_product_udt_1 =   'product_udt_1_head_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_product_udt_2 =   'product_udt_2_head_ctl'
!
!     3rd level for fields
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
!
      character(len=kchara), parameter                                  &
     &      :: hd_product_field_1 = 'product_field_1_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_product_field_2 = 'product_field_2_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_result_field =    'result_field_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_product_type =    'product_type_ctl'
!
      private :: prod_ctl_file_code
      private :: fname_prod_ctl
!
      private :: hd_prod_control, i_prod_control
      private :: hd_platform, hd_org_data, hd_time_step
      private :: hd_prod_files, hd_prod_model
      private :: hd_product_udt_1, hd_product_udt_2
      private :: hd_product_type
      private :: hd_product_field_1, hd_product_field_2
      private :: hd_result_field
      private :: i_prod_files, i_prod_model
!
      private :: read_prod_control_data
      private :: read_prod_files_ctl, read_product_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_prod_udt
!
!
      ctl_file_code = prod_ctl_file_code
      open(ctl_file_code, file=fname_prod_ctl, status='old')
!
      call load_ctl_label_and_line
      call read_prod_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_4_prod_udt
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_prod_control_data
!
!
      if(right_begin_flag(hd_prod_control) .eq. 0) return
      if (i_prod_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_prod_control = find_control_end_flag(hd_prod_control)
        if(i_prod_control .gt. 0) exit
!
        call read_control_platforms                                     &
     &     (ctl_file_code, hd_platform, pu_plt, c_buf1)
        call read_control_platforms                                     &
     &     (ctl_file_code, hd_org_data, org_pu_plt, c_buf1)
!
        call read_prod_files_ctl
        call read_product_model_ctl
      end do
!
      end subroutine read_prod_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_prod_files_ctl
!
!
      if(right_begin_flag(hd_prod_files) .eq. 0) return
      if (i_prod_files.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_prod_files = find_control_end_flag(hd_prod_files)
        if(i_prod_files .gt. 0) exit
!
!
        call read_chara_ctl_type(c_buf1, hd_product_udt_1,              &
     &      product_udt_1_head_ctl)
        call read_chara_ctl_type(c_buf1, hd_product_udt_2,              &
     &      product_udt_2_head_ctl)
      end do
!
      end subroutine read_prod_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_product_model_ctl
!
!
      if(right_begin_flag(hd_prod_model) .eq. 0) return
      if (i_prod_model.gt.0) return
      do
        call load_ctl_label_and_line
!
        i_prod_model = find_control_end_flag(hd_prod_model)
        if(i_prod_model .gt. 0) exit
!
!
        call read_control_time_step_data                                &
     &     (ctl_file_code, hd_time_step, t_pu_ctl, c_buf1)
!
        call read_chara_ctl_type(c_buf1, hd_result_field,               &
     &      result_field_ctl)
        call read_chara_ctl_type(c_buf1, hd_product_field_1,            &
     &      product_field_1_ctl)
        call read_chara_ctl_type(c_buf1, hd_product_field_2,            &
     &      product_field_2_ctl)
        call read_chara_ctl_type(c_buf1, hd_product_type,               &
     &      product_type_ctl)
      end do
!
      end subroutine read_product_model_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_product_udt
