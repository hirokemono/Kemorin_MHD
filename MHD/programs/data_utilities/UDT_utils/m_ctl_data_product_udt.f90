!
!      module m_ctl_data_product_udt
!
      module m_ctl_data_product_udt
!
!      Written by H. Matsui on Nov., 2006
!
!     required module for 3rd level
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
      integer(kind = kint), parameter :: prod_ctl_file_code = 11
!
!
      character(len = kchara), parameter                                &
     &                 :: fname_prod_ctl = "ctl_prod_udt"
!
!
      character(len = kchara) :: product_udt_1_head_ctl = "field/out"
      character(len = kchara) :: product_udt_2_head_ctl = "field/out"
!
      character(len = kchara) :: product_field_1_ctl = "velocity"
      character(len = kchara) :: product_field_2_ctl = "magnetic_field"
      character(len = kchara) :: result_field_ctl =  "velocity"
!
      character(len = kchara) :: product_type_ctl = "Cartesian"
!
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
      integer (kind=kint) :: i_product_udt_1 =   0
      integer (kind=kint) :: i_product_udt_2 =   0
!
!     3rd level for fields
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
      integer (kind=kint) :: i_product_field_1 = 0
      integer (kind=kint) :: i_product_field_2 = 0
      integer (kind=kint) :: i_result_field =    0
      integer (kind=kint) :: i_product_type =    0
!
      private :: prod_ctl_file_code
      private :: fname_prod_ctl
!
      private :: i_prod_control, hd_prod_control
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
!      subroutine read_control_4_diff_udt
!      subroutine read_control_4_prod_udt
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
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
!
!
      if(right_begin_flag(hd_prod_control) .eq. 0) return
      if (i_prod_control.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_prod_control, i_prod_control)
        if(i_prod_control .gt. 0) exit
!
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
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
        call find_control_end_flag(hd_prod_files, i_prod_files)
        if(i_prod_files .gt. 0) exit
!
!
        call read_character_ctl_item(hd_product_udt_1,                  &
     &        i_product_udt_1, product_udt_1_head_ctl)
        call read_character_ctl_item(hd_product_udt_2,                  &
     &        i_product_udt_2, product_udt_2_head_ctl)
      end do
!
      end subroutine read_prod_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine read_product_model_ctl
!
      use m_ctl_data_4_time_steps
!
!
      if(right_begin_flag(hd_prod_model) .eq. 0) return
      if (i_prod_model.gt.0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_prod_model, i_prod_model)
        if(i_prod_model .gt. 0) exit
!
!
        call read_time_step_ctl
!
        call read_character_ctl_item(hd_result_field,                   &
     &            i_result_field, result_field_ctl)
        call read_character_ctl_item(hd_product_field_1,                &
     &            i_product_field_1, product_field_1_ctl)
        call read_character_ctl_item(hd_product_field_2,                &
     &            i_product_field_2, product_field_2_ctl)
        call read_character_ctl_item(hd_product_type,                   &
     &            i_product_type, product_type_ctl)
      end do
!
      end subroutine read_product_model_ctl
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_product_udt
