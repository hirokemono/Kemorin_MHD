!>@file   t_ctl_data_volume_grouping.f90
!!@brief  module t_ctl_data_volume_grouping
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine read_control_new_partition
!!      subroutine dealloc_control_new_partition(part_tctl)
!!        type(new_patition_test_control), intent(inout) :: part_tctl
!!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin mesh_test
!!    begin data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh/in'
!!    end data_files_def
!!
!!    begin new_data_files_def
!!      num_subdomain_ctl    96
!!      mesh_file_prefix         'mesh/in'
!!    end new_data_files_def
!!
!!    begin FEM_mesh_ctl
!!      FEM_surface_output_switch      'NO'
!!      FEM_viewer_mesh_output_switch  'NO'
!!    end FEM_mesh_ctl
!!
!!    begin new_partitioning_ctl
!!      partitioning_method_ctl
!!      array dir_domain_ctl
!!        dir_domain_ctl  x     3
!!        dir_domain_ctl  y     4
!!        dir_domain_ctl  z     8
!!      end array dir_domain_ctl
!!      group_ratio_to_domain_ctl    100
!!
!!      sleeve_level_ctl
!!      element_overlap_ctl
!!    end new_partitioning_ctl
!!  end  mesh_test
!!    -------------------------------------------------------------------
!!@endverbatim
!
      module t_ctl_data_volume_grouping
!
      use m_precision
      use m_machine_parameter
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
!
      use t_read_control_elements
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use skip_comment_f
!
      implicit  none
!
      integer(kind = kint), parameter :: part_ctl_file_code = 11
      character(len = kchara), parameter                                &
     &                        :: fname_new_part_ctl = "ctl_new_part"
!
      type new_patition_control
!>        Flag for new patitioning method
        type(read_character_item) :: new_part_method_ctl
!
!>        Structure for number of subdomains
!!@n        ndomain_section_ctl%c_tbl:  Direction of sectioning
!!@n        ndomain_section_ctl%ivect:  Number of domains
        type(ctl_array_ci) :: ndomain_section_ctl
!>        Ratio of reference division to subdomain
        type(read_integer_item) :: ratio_of_grouping_ctl
!
!>        Flag for element overlapping
        type(read_character_item) :: element_overlap_ctl
!>        Number of sleeve level
        type(read_integer_item) :: sleeve_level_ctl
!
        integer(kind = kint), private :: i_new_patition_ctl = 0
      end type new_patition_control
!
      type new_patition_test_control
!>        Structure for file controls
        type(platform_data_control) :: plt
!>        Structure for new file controls
        type(platform_data_control) :: new_plt
!
!>        Structure of mesh IO controls and sleeve informations
        type(FEM_mesh_control) :: Fmesh_ctl
!
!>        Structure for new partitioning controls
        type(new_patition_control) :: new_part_ctl
!
        integer(kind = kint), private :: i_mesh_test_ctl = 0
      end type new_patition_test_control
!
!     Top level
!
      character(len=kchara), parameter, private                         &
     &         :: hd_mesh_test_ctl = 'mesh_test'
!
      character(len=kchara), parameter, private                         &
     &                    :: hd_platform =      'data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_platform =  'new_data_files_def'
      character(len=kchara), parameter, private                         &
     &                    :: hd_FEM_mesh =      'FEM_mesh_ctl'
      character(len=kchara), parameter, private                         &
     &                    :: hd_new_partition = 'new_partitioning_ctl'
!
      character(len=kchara), parameter, private                         &
     &                 :: hd_part_method =  'partitioning_method_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_sleeve_level = 'sleeve_level_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_ele_overlap =  'element_overlap_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_num_es =       'dir_domain_ctl'
      character(len=kchara), parameter, private                         &
     &                 :: hd_ratio_divide = 'group_ratio_to_domain_ctl'
!
      private :: read_new_parition_data_ctl
      private :: bcast_control_new_partition
      private :: read_ctl_data_new_partition
      private :: dealloc_ctl_data_new_decomp, bcast_ctl_data_new_decomp
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_control_new_partition(part_tctl)
!
      use calypso_mpi
      use skip_comment_f
      use bcast_4_platform_ctl
      use t_read_control_elements
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
      type(buffer_for_control) :: c_buf1
!
!
      if(my_rank .eq. 0) then
        open(part_ctl_file_code, file=fname_new_part_ctl,status='old')
!
        do
          call load_one_line_from_control(part_ctl_file_code, c_buf1)
          call read_new_parition_data_ctl                               &
     &     (part_ctl_file_code, hd_mesh_test_ctl, part_tctl, c_buf1)
          if(part_tctl%i_mesh_test_ctl .gt. 0) exit
        end do
        close(part_ctl_file_code)
      end if
!
      call bcast_control_new_partition(part_tctl)
!
      end subroutine read_control_new_partition
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_control_new_partition(part_tctl)
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      call reset_control_platforms(part_tctl%plt)
      call reset_control_platforms(part_tctl%new_plt)
      call reset_FEM_mesh_control(part_tctl%Fmesh_ctl)
      call dealloc_ctl_data_new_decomp(part_tctl%new_part_ctl)
!
      part_tctl%i_mesh_test_ctl = 0
!
      end subroutine dealloc_control_new_partition
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_new_parition_data_ctl                             &
     &         (id_control, hd_block, part_tctl, c_buf)
!
      use t_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(new_patition_test_control), intent(inout) :: part_tctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_tctl%i_mesh_test_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, part_tctl%plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_new_platform, part_tctl%new_plt, c_buf)
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, part_tctl%Fmesh_ctl, c_buf)
        call read_ctl_data_new_partition(id_control, hd_new_partition,  &
     &      part_tctl%new_part_ctl, c_buf)
!
      end do
      part_tctl%i_mesh_test_ctl = 1
!
      end subroutine read_new_parition_data_ctl
!
!   --------------------------------------------------------------------
!
      subroutine bcast_control_new_partition(part_tctl)
!
      use calypso_mpi
      use calypso_mpi_int
      use bcast_4_platform_ctl
!
      type(new_patition_test_control), intent(inout) :: part_tctl
!
!
      call bcast_ctl_data_4_platform(part_tctl%plt)
      call bcast_ctl_data_4_platform(part_tctl%new_plt)
      call bcast_FEM_mesh_control(part_tctl%Fmesh_ctl)
      call bcast_ctl_data_new_decomp(part_tctl%new_part_ctl)
!
      part_tctl%i_mesh_test_ctl = 0
!
      end subroutine bcast_control_new_partition
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_ctl_data_new_partition                            &
     &         (id_control, hd_block, new_part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(new_patition_control), intent(inout) :: new_part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(new_part_ctl%i_new_patition_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_i(id_control,                         &
     &      hd_num_es, new_part_ctl%ndomain_section_ctl, c_buf)
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sleeve_level, new_part_ctl%sleeve_level_ctl)
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_ratio_divide, new_part_ctl%ratio_of_grouping_ctl)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_part_method, new_part_ctl%new_part_method_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_ele_overlap, new_part_ctl%element_overlap_ctl)
      end do
      new_part_ctl%i_new_patition_ctl = 1
!
      end subroutine read_ctl_data_new_partition
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_new_decomp(new_part_ctl)
!
      type(new_patition_control), intent(inout) :: new_part_ctl
!
!
      call dealloc_control_array_c_i(new_part_ctl%ndomain_section_ctl)
!
      new_part_ctl%new_part_method_ctl%iflag = 0
      new_part_ctl%element_overlap_ctl%iflag = 0
      new_part_ctl%sleeve_level_ctl%iflag = 0
!
      new_part_ctl%ratio_of_grouping_ctl%iflag = 0
!
      new_part_ctl%i_new_patition_ctl = 0
!
      end subroutine dealloc_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      subroutine bcast_ctl_data_new_decomp(new_part_ctl)
!
      use calypso_mpi_int
      use bcast_control_arrays
!
      type(new_patition_control), intent(inout) :: new_part_ctl
!
!
      call bcast_ctl_array_ci(new_part_ctl%ndomain_section_ctl)
      call bcast_ctl_type_c1(new_part_ctl%new_part_method_ctl)
      call bcast_ctl_type_c1(new_part_ctl%element_overlap_ctl)
      call bcast_ctl_type_i1(new_part_ctl%sleeve_level_ctl)
      call bcast_ctl_type_i1(new_part_ctl%ratio_of_grouping_ctl)
!
      call calypso_mpi_bcast_one_int                                    &
     &   (new_part_ctl%i_new_patition_ctl, 0)
!
      end subroutine bcast_ctl_data_new_decomp
!
! -----------------------------------------------------------------------
!
      end module t_ctl_data_volume_grouping
