!
!      module t_ctl_data_diff_udt
!
!      Written by H. Matsui on Nov., 2006
!
!     required module for 3rd level
!
!!      subroutine read_control_4_diff_udt(hd_block, diff_udt_c)
!!      subroutine dealloc_diff_control_data(diff_udt_c)
!
      module t_ctl_data_diff_udt
!
      use m_precision
      use m_machine_parameter
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_fields
      use t_ctl_data_4_time_steps
      use t_ctl_data_ele_layering
      use t_ctl_data_4_fem_int_pts
      use t_control_elements
      use calypso_mpi
      use skip_comment_f
!
!
      implicit  none
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
      integer(kind = kint), parameter :: diff_ctl_file_code = 11
!
      type diff_model_ctl
        type(read_character_item) :: ref_udt_head_ctl
        type(read_character_item) :: tgt_udt_head_ctl
!
!>        Structure for field information control
        type(field_control) :: fld_d_ctl
!>        Structure for time stepping control
        type(time_data_control) :: t_d_ctl
!>        Structure for element layering
        type(layering_control) :: elayer_d_ctl
!>       integeration points
        type(fem_intergration_control)  :: fint_d_ctl
!
        type(read_character_item) :: product_field_ctl
        type(read_character_item) :: correlate_fld_ctl
        type(read_character_item) :: correlate_cmp_ctl
!
        type(read_character_item) :: correlate_coord_ctl
!
        type(read_character_item) :: group_mesh_head_ctl
!
        integer (kind=kint) :: i_diff_files =  0
        integer (kind=kint) :: i_diff_model =  0
      end type diff_model_ctl
!
      type diff_udt_ctl
!>        Structure for file names
        type(platform_data_control) :: d_plt
!>        Structure for original file names
        type(platform_data_control) :: org_d_plt
!>        Structure for difference
        type(diff_model_ctl) :: diff_ctl
!
        integer (kind=kint) :: i_diff_control = 0
      end type diff_udt_ctl
!
!     2nd level for const_filter
!
      character(len=kchara), parameter                                  &
     &         :: hd_diff_files = 'data_file_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_diff_model = 'models'
!
!     3rd level for file header
!
      character(len=kchara), parameter                                  &
     &         :: hd_ref_udt_head_ctl = 'reference_field_header_ctl'
      character(len=kchara), parameter                                  &
     &         :: hd_tgt_udt_head_ctl = 'target_field_header_ctl'
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
!   labels for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &      :: hd_phys_values =  'phys_values_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      character(len=kchara), parameter :: hd_dynamic_layers             &
     &                        = 'dynamic_model_layer_ctl'
      character(len=kchara), parameter                                  &
     &      :: hd_int_points = 'intg_point_num_ctl'
!
      private :: hd_platform, hd_org_data, hd_phys_values
      private :: hd_dynamic_layers
      private :: hd_int_points
!
      private :: fname_diff_ctl, fname_ave_ctl
      private :: fname_prod_ctl, fname_corr_ctl, fname_grp_patch_ctl
!
      private :: hd_diff_files, hd_diff_model
      private :: hd_ref_udt_head_ctl, hd_tgt_udt_head_ctl
      private :: hd_correlate_coord
      private :: hd_corr_fld_name, hd_corr_cmp_name
      private :: hd_prod_name, hd_group_mesh_head
!
      private :: diff_ctl_file_code
      private :: read_diff_control_data
      private :: bcast_diff_control_data
      private :: read_diff_files_ctl
      private :: dealloc_diff_files_ctl, bcast_diff_files_ctl
      private :: read_diff_model_ctl
      private :: bcast_diff_model_ctl, dealloc_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_4_diff_udt(hd_block, diff_udt_c)
!
      character(len=kchara), intent(in) :: hd_block
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(diff_ctl_file_code, file=fname_diff_ctl, status='old')
!
        do
          call load_one_line_from_control(diff_ctl_file_code, c_buf1)
          call read_diff_control_data(diff_ctl_file_code,               &
     &        hd_block, diff_udt_c, c_buf1)
          if(diff_udt_c%i_diff_control .gt. 0) exit
        end do
        close(diff_ctl_file_code)
      end if
!
      call bcast_diff_control_data(diff_udt_c)
!
      end subroutine read_control_4_diff_udt
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_diff_control_data                                 &
     &         (id_control, hd_block, diff_udt_c, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(diff_udt_c%i_diff_control .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, diff_udt_c%d_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, diff_udt_c%org_d_plt, c_buf)
!
        call read_diff_files_ctl                                        &
     &     (id_control, hd_diff_files, diff_udt_c%diff_ctl, c_buf)
        call read_diff_model_ctl                                        &
     &     (id_control, hd_diff_model, diff_udt_c%diff_ctl, c_buf)
      end do
      diff_udt_c%i_diff_control = 1
!
      end subroutine read_diff_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_control_data(diff_udt_c)
!
      use bcast_4_platform_ctl
!
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
!
!
      call bcast_ctl_data_4_platform(diff_udt_c%d_plt)
      call bcast_ctl_data_4_platform(diff_udt_c%org_d_plt)
!
      call MPI_BCAST(diff_udt_c%i_diff_control, 1,                      &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_diff_control_data
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_diff_control_data(diff_udt_c)
!
      type(diff_udt_ctl), intent(inout)  :: diff_udt_c
!
!
      call bcast_diff_files_ctl(diff_udt_c%diff_ctl)
      call bcast_diff_model_ctl(diff_udt_c%diff_ctl)
!
      call dealloc_diff_files_ctl(diff_udt_c%diff_ctl)
      call dealloc_diff_model_ctl(diff_udt_c%diff_ctl)
!
      call reset_control_platforms(diff_udt_c%d_plt)
      call reset_control_platforms(diff_udt_c%org_d_plt)
!
      diff_udt_c%i_diff_control = 0
!
      end subroutine dealloc_diff_control_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_diff_files_ctl                                    &
     &         (id_control, hd_block, diff_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(diff_ctl%i_diff_files.gt.0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ref_udt_head_ctl, diff_ctl%ref_udt_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_tgt_udt_head_ctl, diff_ctl%tgt_udt_head_ctl)
      end do
      diff_ctl%i_diff_files = 1
!
      end subroutine read_diff_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_files_ctl(diff_ctl)
!
      use bcast_control_arrays
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      call bcast_ctl_type_c1(diff_ctl%ref_udt_head_ctl)
      call bcast_ctl_type_c1(diff_ctl%tgt_udt_head_ctl)
!
      call MPI_BCAST(diff_ctl%i_diff_files, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_diff_files_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_diff_files_ctl(diff_ctl)
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      diff_ctl%ref_udt_head_ctl%iflag = 0
      diff_ctl%tgt_udt_head_ctl%iflag = 0
!
      diff_ctl%i_diff_files = 0
!
      end subroutine dealloc_diff_files_ctl
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_diff_model_ctl                                    &
     &         (id_control, hd_block, diff_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(diff_ctl%i_diff_model .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_phys_data_control                                     &
     &     (id_control, hd_phys_values, diff_ctl%fld_d_ctl, c_buf)
        call read_control_time_step_data                                &
     &     (id_control, hd_time_step, diff_ctl%t_d_ctl, c_buf)
        call read_ele_layers_control(id_control, hd_dynamic_layers,     &
     &      diff_ctl%elayer_d_ctl, c_buf)
        call read_control_fem_int_points                                &
     &     (id_control, hd_int_points, diff_ctl%fint_d_ctl, c_buf)
!
        call read_chara_ctl_type(c_buf, hd_prod_name,                   &
     &      diff_ctl%product_field_ctl)
        call read_chara_ctl_type(c_buf, hd_corr_fld_name,               &
     &      diff_ctl%correlate_fld_ctl)
        call read_chara_ctl_type(c_buf, hd_corr_cmp_name,               &
     &      diff_ctl%correlate_cmp_ctl)
        call read_chara_ctl_type(c_buf, hd_correlate_coord,             &
     &      diff_ctl%correlate_coord_ctl)
        call read_chara_ctl_type(c_buf, hd_group_mesh_head,             &
     &      diff_ctl%group_mesh_head_ctl)
      end do
      diff_ctl%i_diff_model = 1
!
      end subroutine read_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_diff_model_ctl(diff_ctl)
!
      use bcast_control_arrays
      use bcast_4_field_ctl
      use bcast_4_time_step_ctl
      use bcast_4_filter_files_ctl
      use bcast_4_fem_int_pts_ctl
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      call bcast_phys_data_ctl(diff_ctl%fld_d_ctl)
      call bcast_ctl_data_4_time_step(diff_ctl%t_d_ctl)
      call bcast_ele_layers_control(diff_ctl%elayer_d_ctl)
      call bcast_control_fem_int_points(diff_ctl%fint_d_ctl)
!
      call bcast_ctl_type_c1(diff_ctl%product_field_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_fld_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_cmp_ctl)
      call bcast_ctl_type_c1(diff_ctl%correlate_coord_ctl)
      call bcast_ctl_type_c1(diff_ctl%group_mesh_head_ctl)
!
      call MPI_BCAST(diff_ctl%i_diff_model, 1,                          &
     &               CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
!
      end subroutine bcast_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_diff_model_ctl(diff_ctl)
!
      type(diff_model_ctl), intent(inout)  :: diff_ctl
!
!
      call dealloc_phys_control(diff_ctl%fld_d_ctl)
      call dealloc_ctl_data_ele_layering(diff_ctl%elayer_d_ctl)
!
      diff_ctl%product_field_ctl%iflag = 0
      diff_ctl%correlate_fld_ctl%iflag = 0
      diff_ctl%correlate_cmp_ctl%iflag = 0
      diff_ctl%correlate_coord_ctl%iflag = 0
      diff_ctl%group_mesh_head_ctl%iflag = 0
!
      diff_ctl%i_diff_model = 0
!
      end subroutine dealloc_diff_model_ctl
!
!   --------------------------------------------------------------------
!
      end module t_ctl_data_diff_udt
