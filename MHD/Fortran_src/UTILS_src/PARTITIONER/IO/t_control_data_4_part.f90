!>@file   t_control_data_4_part.f90
!!@brief  module t_control_data_4_part
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Control data for partitioner
!!
!!@verbatim
!!      subroutine dealloc_ctl_data_4_part(part_ctl)
!!      subroutine read_control_data_4_part(part_ctl)
!!@endverbatim
!
      module t_control_data_4_part
!
      use m_precision
!
      use t_read_control_elements
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_control_array_integer
      use t_control_array_character
      use t_control_array_charaint
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 11
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_part'
!
!
      type control_data_4_partitioner
!>        Structure for file names
        type(platform_data_control) :: part_plt
!>        Structure for original file names
        type(platform_data_control) :: single_plt
!
!>        Structure for FEM mesh controls
        type(FEM_mesh_control) :: part_Fmesh
!>        Patitioning method
        type(read_character_item) :: part_method_ctl
!>        Number of sleeve level
        type(read_integer_item) :: sleeve_level_old
!
!>        Flag for new patitioning method
        type(read_character_item) :: new_part_method_ctl
!>        Flag for selective ghost cells generation overlapping
        type(read_character_item) :: selective_ghost_ctl
!
!>        Structure for list of bisection
!!@n        RCB_dir_ctl%c_tbl: Direction of bisectioning
!!@n        RCB_dir_ctl%ivec: Number of decomposition
        type(ctl_array_ci) :: RCB_dir_ctl
!
!>        Structure for number of subdomains
!!@n        ndomain_section_ctl%c_tbl:  Direction of sectioning
!!@n        ndomain_section_ctl%ivect:  Number of domains
        type(ctl_array_ci) :: ndomain_section_ctl
!
!>        Structure for element group list for layering
!!@n        ele_grp_layering_ctl%c_tbl:  list of element group
        type(ctl_array_chara) :: ele_grp_layering_ctl
!
!>        File name for sphere file data
        type(read_character_item) :: sphere_file_name_ctl
!>        File name for MeTiS imput
        type(read_character_item) :: metis_input_file_ctl
!>        File name for MeTiS domain file
        type(read_character_item) :: metis_domain_file_ctl
!
!>        File name for domain grouping file name
        type(read_character_item) :: domain_group_file_ctl
!
!>        File name for finer mesh file prefix
        type(read_character_item) :: finer_mesh_head_ctl
!>        File format for finer mesh file prefix
        type(read_character_item) :: finer_mesh_fmt_ctl
!
!>        File name for interpolation table for finer mesh file
        type(read_character_item) :: itp_tbl_head_ctl
!>        File format for finer mesh file
        type(read_character_item) :: itp_tbl_format_ctl
!
!>        Structure for element group list for ordering
!!@n        ele_grp_ordering_ctl%c_tbl:  list of element group
        type(ctl_array_chara) :: ele_grp_ordering_ctl
!
!
        integer (kind=kint) :: i_part_ctl = 0
!
        integer (kind=kint) :: i_decomp_ctl =       0
        integer (kind=kint) :: i_part_ghost_ctl =   0
        integer (kind=kint) :: i_ele_ordering_ctl = 0
      end type control_data_4_partitioner
!
!   Top level
!
      character(len=kchara), parameter :: hd_part_ctl                   &
     &                      = 'partitioner_control'
!
!   2nd level for partitioner_control
!
      character(len=kchara), parameter                                  &
     &                    :: hd_platform = 'data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_org_data = 'org_data_files_def'
      character(len=kchara), parameter                                  &
     &                    :: hd_FEM_mesh = 'FEM_mesh_ctl'
!
      character(len=kchara), parameter :: hd_org_f_ctl                  &
     &                      = 'original_file_ctl'
      character(len=kchara), parameter :: hd_ele_ordering_ctl           &
     &                      = 'ordering_by_ele_grp'
      character(len=kchara), parameter :: hd_decomp_ctl                 &
     &                      = 'decompose_ctl'
      character(len=kchara), parameter :: hd_part_ghost_ctl             &
     &                      = 'part_ghost_ctl'
!
!   3rd level for new partition and ghost cells
!
      character(len=kchara), parameter :: hd_new_partition              &
     &                      = 'new_partition_ctl'
      character(len=kchara), parameter :: hd_selective_ghost            &
     &                      = 'selective_ghost_cell'
!
!   3rd level for ordering_ctl
!
      character(len=kchara), parameter :: hd_nele_grp_ordering          &
     &                      = 'ordering_ele_grp_ctl'
!
!   3rd level for decompose_ctl
!
      character(len=kchara), parameter :: hd_part_method                &
     &                      = 'partitioning_method_ctl'
      character(len=kchara), parameter :: hd_sleeve_level               &
     &                      = 'sleeve_level_ctl'
!
!     RCB
      character(len=kchara), parameter :: hd_num_rcb = 'RCB_dir_ctl'
!
!     ES
      character(len=kchara), parameter :: hd_num_es = 'dir_domain_ctl'
!
!     layered_ES_ctl
      character(len=kchara), parameter :: hd_num_r_layerd               &
     &                      = 'radial_layering_grp_ctl'
!
!     cubed sphere
      character(len=kchara), parameter :: hd_sph_sf_file                &
     &                      = 'sphere_file_name_ctl'
!
!     metis input
      character(len=kchara), parameter :: hd_metis_in_file              &
     &                      = 'metis_input_file_ctl'
!
!     metis resules
      character(len=kchara), parameter :: hd_metis_dom_file             &
     &                      = 'metis_domain_file_ctl'
!
!     grouping data file
      character(len=kchara), parameter :: hd_domain_tbl_file            &
     &                      = 'domain_group_file_ctl'
      character(len=kchara), parameter :: hd_fine_mesh_file             &
     &                      = 'finer_mesh_head_ctl'
      character(len=kchara), parameter :: hd_fine_fmt_file              &
     &                      = 'finer_mesh_fmt_ctl'
      character(len=kchara), parameter :: hd_fine_itp_file              &
     &                      = 'interpolate_tbl_head'
      character(len=kchara), parameter                                  &
     &         :: hd_fmt_itp_tbl = 'interpolate_table_format_ctl'
!
!
      private :: control_file_name
      private :: hd_part_ctl
      private :: hd_org_f_ctl, hd_platform, hd_org_data, hd_FEM_mesh
      private :: hd_ele_ordering_ctl, hd_decomp_ctl
      private :: hd_nele_grp_ordering
      private :: hd_part_method, hd_sleeve_level
      private :: hd_num_rcb, hd_num_es, hd_num_r_layerd, hd_sph_sf_file
      private :: hd_metis_in_file, hd_metis_dom_file
      private :: hd_domain_tbl_file, hd_fine_mesh_file
      private :: hd_fine_itp_file, hd_fmt_itp_tbl, hd_fine_fmt_file
!
      private :: read_part_control_data, read_ctl_data_4_part_ghost
      private :: read_ctl_data_4_decomp, read_ctl_data_4_ele_ordeirng
!
      private :: dealloc_ctl_data_4_decomp, reset_ctl_data_4_part_ghost
      private :: dealloc_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_4_part(part_ctl)
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
      call reset_FEM_mesh_control(part_ctl%part_Fmesh)
      call dealloc_ctl_data_4_decomp(part_ctl)
      call reset_ctl_data_4_part_ghost(part_ctl)
      call dealloc_ctl_data_4_ele_ordeirng(part_ctl)
!
      end subroutine dealloc_ctl_data_4_part
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_part(part_ctl)
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
      type(buffer_for_control)  :: c_buf1
!
!
      open(control_file_code, file = control_file_name)
!
      do
        call load_one_line_from_control(control_file_code, c_buf1)
        call read_part_control_data(control_file_code, hd_part_ctl,  &
     &      part_ctl, c_buf1)
        if(part_ctl%i_part_ctl .gt. 0) exit
      end do
!
      close(control_file_code)
!
      end subroutine read_control_data_4_part
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_part_control_data                                &
     &         (id_control, hd_block, part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_ctl%i_part_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_platforms                                     &
     &     (id_control, hd_platform, part_ctl%part_plt, c_buf)
        call read_control_platforms                                     &
     &     (id_control, hd_org_data, part_ctl%single_plt, c_buf)
!
        call read_FEM_mesh_control                                      &
     &     (id_control, hd_FEM_mesh, part_ctl%part_Fmesh, c_buf)
!
        call read_ctl_data_4_decomp                                     &
     &     (id_control, hd_decomp_ctl, part_ctl, c_buf)
        call read_ctl_data_4_ele_ordeirng                               &
     &     (id_control, hd_ele_ordering_ctl, part_ctl, c_buf)
        call read_ctl_data_4_part_ghost                                 &
     &     (id_control, hd_part_ghost_ctl, part_ctl, c_buf)
      end do
      part_ctl%i_part_ctl = 1
!
      end subroutine read_part_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_decomp                                 &
     &         (id_control, hd_block, part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_ctl%i_decomp_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c_i(id_control,                         &
     &      hd_num_rcb, part_ctl%RCB_dir_ctl, c_buf)
        call read_control_array_c_i(id_control,                         &
     &      hd_num_es, part_ctl%ndomain_section_ctl, c_buf)
        call read_control_array_c1(id_control,                          &
     &      hd_num_r_layerd, part_ctl%ele_grp_layering_ctl, c_buf)
!
!
        call read_integer_ctl_type                                      &
     &     (c_buf, hd_sleeve_level, part_ctl%sleeve_level_old)
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_part_method, part_ctl%part_method_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_sph_sf_file, part_ctl%sphere_file_name_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_metis_in_file, part_ctl%metis_input_file_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_metis_dom_file, part_ctl%metis_domain_file_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_fine_mesh_file, part_ctl%finer_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_fine_fmt_file, part_ctl%finer_mesh_fmt_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_fine_itp_file, part_ctl%itp_tbl_head_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_domain_tbl_file, part_ctl%domain_group_file_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_fmt_itp_tbl, part_ctl%itp_tbl_format_ctl)
      end do
      part_ctl%i_decomp_ctl = 1
!
      end subroutine read_ctl_data_4_decomp
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ctl_data_4_decomp(part_ctl)
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
!
      call dealloc_control_array_c_i(part_ctl%RCB_dir_ctl)
      call dealloc_control_array_c_i(part_ctl%ndomain_section_ctl)
      call dealloc_control_array_chara(part_ctl%ele_grp_layering_ctl)
!
      part_ctl%part_method_ctl%iflag = 0
      part_ctl%sleeve_level_old%iflag = 0
!
      part_ctl%sphere_file_name_ctl%iflag = 0
      part_ctl%metis_input_file_ctl%iflag = 0
      part_ctl%metis_domain_file_ctl%iflag = 0
!
      part_ctl%domain_group_file_ctl%iflag = 0
!
      part_ctl%finer_mesh_head_ctl%iflag = 0
      part_ctl%finer_mesh_fmt_ctl%iflag = 0
!
      part_ctl%itp_tbl_head_ctl%iflag = 0
      part_ctl%itp_tbl_format_ctl%iflag = 0
!
      part_ctl%i_decomp_ctl = 0
!
      end subroutine dealloc_ctl_data_4_decomp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_part_ghost                             &
     &         (id_control, hd_block, part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_ctl%i_part_ghost_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_new_partition, part_ctl%new_part_method_ctl)
        call read_chara_ctl_type                                        &
     &     (c_buf, hd_selective_ghost, part_ctl%selective_ghost_ctl)
      end do
      part_ctl%i_part_ghost_ctl = 1
!
      end subroutine read_ctl_data_4_part_ghost
!
! -----------------------------------------------------------------------
!
      subroutine reset_ctl_data_4_part_ghost(part_ctl)
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
!
      part_ctl%new_part_method_ctl%iflag = 0
      part_ctl%selective_ghost_ctl%iflag = 0
!
      part_ctl%i_part_ghost_ctl = 0
!
      end subroutine reset_ctl_data_4_part_ghost
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_ele_ordeirng                           &
     &         (id_control, hd_block, part_ctl, c_buf)
!
      integer(kind = kint), intent(in) :: id_control
      character(len=kchara), intent(in) :: hd_block
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
      type(buffer_for_control), intent(inout)  :: c_buf
!
!
      if(check_begin_flag(c_buf, hd_block) .eqv. .FALSE.) return
      if(part_ctl%i_ele_ordering_ctl .gt. 0) return
      do
        call load_one_line_from_control(id_control, c_buf)
        if(check_end_flag(c_buf, hd_block)) exit
!
        call read_control_array_c1(id_control, hd_nele_grp_ordering,    &
     &      part_ctl%ele_grp_ordering_ctl, c_buf)
      end do
      part_ctl%i_ele_ordering_ctl = 1
!
      end subroutine read_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------!
      subroutine dealloc_ctl_data_4_ele_ordeirng(part_ctl)
!
      type(control_data_4_partitioner), intent(inout) :: part_ctl
!
!
      call dealloc_control_array_chara(part_ctl%ele_grp_ordering_ctl)
!
      part_ctl%i_ele_ordering_ctl = 0
!
      end subroutine dealloc_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      end module t_control_data_4_part

