!
!      module t_ctl_data_product_udt
!
!      Written by H. Matsui on Nov., 2006
!
!!      subroutine read_control_4_prod_udt(prod_udt_c)
!!      subroutine reset_prod_control_data(prod_udt_c)
!!        type(product_udt_ctl), intent(inout) :: prod_udt_c
!
!
      module t_ctl_data_product_udt
!
      use m_precision
      use calypso_mpi
      use m_machine_parameter
      use t_read_control_elements
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
      type product_model_ctl
        type(read_character_item) :: product_udt_1_head_ctl
        type(read_character_item) :: product_udt_2_head_ctl
!
!>        Structure for time stepping control
        type(time_data_control) :: t_pu_ctl
!
        type(read_character_item) :: product_field_1_ctl
        type(read_character_item) :: product_field_2_ctl
        type(read_character_item) :: result_field_ctl
!
        type(read_character_item) :: product_type_ctl
!
        integer (kind=kint) :: i_prod_files =  0
        integer (kind=kint) :: i_prod_model =  0
      end type product_model_ctl
!
      type product_udt_ctl
!>        Structure for file names
        type(platform_data_control) :: pu_plt
!>        Structure for original file names
        type(platform_data_control) :: org_pu_plt
!
        type(product_model_ctl) :: prod_ctl
!
        integer (kind=kint) :: i_prod_control = 0
      end type product_udt_ctl
!
!
!     Top level for products
      character(len=kchara), parameter ::                               &
     &                  hd_prod_control = 'products_udts'
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_prod_files = 'data_file_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_prod_model = 'models'
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
      private :: hd_prod_control
      private :: hd_platform, hd_org_data, hd_time_step
      private :: hd_prod_files, hd_prod_model
      private :: hd_product_udt_1, hd_product_udt_2
      private :: hd_product_type
      private :: hd_product_field_1, hd_product_field_2
      private :: hd_result_field
!
      private :: read_prod_control_data, bcast_prod_control_data
      private :: read_prod_files_ctl, read_product_model_ctl
      private :: bcast_prod_files_ctl, bcast_product_model_ctl
      private :: reset_prod_files_ctl, reset_product_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_4_prod_udt(prod_udt_c)
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
!
        open(prod_ctl_file_code, file=fname_prod_ctl, status='old')
        do
          call load_one_line_from_control(prod_ctl_file_code, c_buf1)
          call read_prod_control_data                                   &
     &       (prod_ctl_file_code, hd_prod_control, prod_udt_c, c_buf1)
          if(prod_udt_c%i_prod_control .gt. 0) exit
        end do
        close(prod_ctl_file_code)
      end if
!
      call bcast_prod_control_data(prod_udt_c)
!
      end subroutine read_control_4_prod_udt
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_prod_control_data                                 &
     &         (id_control, hd_block, prod_udt_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(prod_udt_c%i_prod_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, prod_udt_c%pu_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, prod_udt_c%org_pu_plt, c_buf)
!
        call read_prod_files_ctl                                        &
     &     (id_control, hd_prod_files, prod_udt_c%prod_ctl, c_buf)
        call read_product_model_ctl                                     &
     &     (id_control, hd_prod_model, prod_udt_c%prod_ctl, c_buf)
      end do
      prod_udt_c%i_prod_control = 1
!
      end subroutine read_prod_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_prod_control_data(prod_udt_c)
!
      use bcast_4_platform_ctl
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
!
!
      call bcast_prod_files_ctl(prod_udt_c%prod_ctl)
      call bcast_product_model_ctl(prod_udt_c%prod_ctl)
!
      call bcast_ctl_data_4_platform(prod_udt_c%pu_plt)
      call bcast_ctl_data_4_platform(prod_udt_c%org_pu_plt)
!
      call MPI_BCAST(prod_udt_c%i_prod_control, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_prod_control_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_prod_control_data(prod_udt_c)
!
      type(product_udt_ctl), intent(inout) :: prod_udt_c
!
!
      call reset_prod_files_ctl(prod_udt_c%prod_ctl)
      call reset_product_model_ctl(prod_udt_c%prod_ctl)
!
      call reset_control_platforms(prod_udt_c%pu_plt)
      call reset_control_platforms(prod_udt_c%org_pu_plt)
!
      prod_udt_c%i_prod_control = 0
!
      end subroutine reset_prod_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_prod_files_ctl                                    &
     &         (id_control, hd_block, prod_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(product_model_ctl), intent(inout) :: prod_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(prod_ctl%i_prod_files.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_chara_ctl_type(c_buf, hd_product_udt_1,               &
     &      prod_ctl%product_udt_1_head_ctl)
        call read_chara_ctl_type(c_buf, hd_product_udt_2,               &
     &      prod_ctl%product_udt_2_head_ctl)
      end do
      prod_ctl%i_prod_files = 1
!
      end subroutine read_prod_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_prod_files_ctl(prod_ctl)
!
      use bcast_control_arrays
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      call bcast_ctl_type_c1(prod_ctl%product_udt_1_head_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_udt_2_head_ctl)
!
      call MPI_BCAST(prod_ctl%i_prod_files, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_prod_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_prod_files_ctl(prod_ctl)
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      prod_ctl%product_udt_1_head_ctl%iflag = 0
      prod_ctl%product_udt_2_head_ctl%iflag = 0
!
      prod_ctl%i_prod_files = 0
!
      end subroutine reset_prod_files_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_product_model_ctl                                 &
     &         (id_control, hd_block, prod_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(product_model_ctl), intent(inout) :: prod_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(prod_ctl%i_prod_model.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
!
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, prod_ctl%t_pu_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_result_field,                &
     &      prod_ctl%result_field_ctl)
        call read_chara_ctl_type(c_buf, hd_product_field_1,             &
     &      prod_ctl%product_field_1_ctl)
        call read_chara_ctl_type(c_buf, hd_product_field_2,             &
     &      prod_ctl%product_field_2_ctl)
        call read_chara_ctl_type(c_buf, hd_product_type,                &
     &      prod_ctl%product_type_ctl)
      end do
      prod_ctl%i_prod_model = 1
!
      end subroutine read_product_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_product_model_ctl(prod_ctl)
!
      use bcast_4_time_step_ctl
      use bcast_control_arrays
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      call bcast_ctl_data_4_time_step(prod_ctl%t_pu_ctl)
!
      call bcast_ctl_type_c1(prod_ctl%result_field_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_field_1_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_field_2_ctl)
      call bcast_ctl_type_c1(prod_ctl%product_type_ctl)
!
      call MPI_BCAST(prod_ctl%i_prod_model, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_product_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine reset_product_model_ctl(prod_ctl)
!
      type(product_model_ctl), intent(inout) :: prod_ctl
!
!
      prod_ctl%result_field_ctl%iflag =    0
      prod_ctl%product_field_1_ctl%iflag = 0
      prod_ctl%product_field_2_ctl%iflag = 0
      prod_ctl%product_type_ctl%iflag =    0
!
      prod_ctl%i_prod_model = 0
!
      end subroutine reset_product_model_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_product_udt
