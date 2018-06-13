!>@file   m_control_data_4_part.f90
!!@brief  module m_control_data_4_part
!!
!!@author H. Matsui
!!@date Programmed in Sep., 2007
!
!>@brief  Control data for partitioner
!!
!!@verbatim
!!      subroutine dealloc_num_bisection_ctl
!!      subroutine dealloc_num_subdomains_ctl
!!      subroutine dealloc_ele_grp_ordering_ctl
!!      subroutine dealloc_ele_grp_layer_ctl
!!      subroutine read_control_data_4_part
!!@endverbatim
!
      module m_control_data_4_part
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
      use t_ctl_data_4_platforms
      use t_ctl_data_4_FEM_mesh
      use t_read_control_arrays
      use t_control_elements
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 11
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_part'
!
      integer (kind=kint), parameter :: my_rank = 0
!
!
!>      Structure for file names
      type(platform_data_control), save :: part_plt
!>      Structure for original file names
      type(platform_data_control), save :: single_plt
!
!>      Structure for FEM mesh controls
      type(FEM_mesh_control), save :: part_Fmesh
!>      Patitioning method
      type(read_character_item), save :: part_method_ctl
!>      Flag for element overlapping
      type(read_character_item), save :: element_overlap_ctl
!>      Number of sleeve level
      type(read_integer_item), save :: sleeve_level_old
!
!>      Structure for list of bisection
!!@n      ele_grp_ordering_ctl%c_tbl: Direction of bisectioning
      type(ctl_array_chara), save :: RCB_dir_ctl
!
!>      Structure for number of subdomains
!!@n      ndomain_section_ctl%c_tbl:  Direction of sectioning
!!@n      ndomain_section_ctl%ivect:  Number of domains
      type(ctl_array_ci), save :: ndomain_section_ctl
!
!>      Structure for element group list for layering
!!@n      ele_grp_ordering_ctl%c_tbl:  list of element group
      type(ctl_array_chara), save :: ele_grp_layering_ctl
!
!>      File name for sphere file data
      type(read_character_item), save :: sphere_file_name_ctl
!>      File name for MeTiS imput
      type(read_character_item), save :: metis_input_file_ctl
!>      File name for MeTiS domain file
      type(read_character_item), save :: metis_domain_file_ctl
!
!>      File name for domain grouping file name
      type(read_character_item), save :: domain_group_file_ctl
!
!>      File name for finer mesh file prefix
      type(read_character_item), save :: finer_mesh_head_ctl
!>      File format for finer mesh file prefix
      type(read_character_item), save :: finer_mesh_fmt_ctl
!
!>      File name for interpolation table for finer mesh file
      type(read_character_item), save :: itp_tbl_head_ctl
!>      File format for finer mesh file
      type(read_character_item), save :: itp_tbl_format_ctl
!
!>      Structure for element group list for ordering
!!@n      ele_grp_ordering_ctl%c_tbl:  list of element group
      type(ctl_array_chara), save :: ele_grp_ordering_ctl
!
!   Top level
!
      character(len=kchara), parameter :: hd_part_ctl                   &
     &                      = 'partitioner_control'
      integer (kind=kint) :: i_part_ctl = 0
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
!
      integer(kind=kint) :: i_platform =   0
      integer(kind=kint) :: i_org_data =   0
      integer(kind=kint) :: i_FEM_mesh =   0
!
      integer (kind=kint) :: i_org_f_ctl =        0
      integer (kind=kint) :: i_ele_ordering_ctl = 0
      integer (kind=kint) :: i_decomp_ctl =       0
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
      character(len=kchara), parameter :: hd_ele_overlap                &
     &                      = 'element_overlap_ctl'
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
     &         :: hd_fmt_itp_tbl =    'interpolate_table_format_ctl'

      integer (kind=kint) :: i_fmt_itp_tbl =      0
!
!
      private :: control_file_name
      private :: my_rank
      private :: hd_part_ctl, i_part_ctl
      private :: hd_org_f_ctl
      private :: hd_platform, i_platform
      private :: hd_org_data, i_org_data
      private :: hd_FEM_mesh, i_FEM_mesh
      private :: hd_ele_ordering_ctl, hd_decomp_ctl
      private :: i_ele_ordering_ctl, i_decomp_ctl
      private :: hd_nele_grp_ordering
      private :: hd_part_method, hd_sleeve_level, hd_ele_overlap
      private :: hd_num_rcb, hd_num_es, hd_num_r_layerd, hd_sph_sf_file
      private :: hd_metis_in_file, hd_metis_dom_file
      private :: hd_domain_tbl_file, hd_fine_mesh_file
      private :: hd_fine_itp_file, hd_fmt_itp_tbl, hd_fine_fmt_file
!
      private :: read_part_control_data
      private :: read_ctl_data_4_decomp, read_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_bisection_ctl
!
      call dealloc_control_array_chara(RCB_dir_ctl)
!
      end subroutine dealloc_num_bisection_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_subdomains_ctl
!
      call dealloc_control_array_c_i(ndomain_section_ctl)
!
      end subroutine dealloc_num_subdomains_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_ordering_ctl
!
      call dealloc_control_array_chara(ele_grp_ordering_ctl)
!
      end subroutine dealloc_ele_grp_ordering_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_layer_ctl
!
      call dealloc_control_array_chara(ele_grp_layering_ctl)
!
      end subroutine dealloc_ele_grp_layer_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_part
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_part_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_4_part
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_part_control_data
!
!
      if(right_begin_flag(hd_part_ctl) .eq. 0) return
      if (i_part_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_part_ctl, i_part_ctl)
        if(i_part_ctl .gt. 0) exit
!
!
        call read_control_platforms(hd_platform, i_platform, part_plt)
        call read_control_platforms                                     &
     &     (hd_org_data, i_org_data, single_plt)
!
        call read_FEM_mesh_control(hd_FEM_mesh, i_FEM_mesh, part_Fmesh)
!
        call read_ctl_data_4_decomp
        call read_ctl_data_4_ele_ordeirng
      end do
!
      end subroutine read_part_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------!
      subroutine read_ctl_data_4_decomp
!
!
      if(right_begin_flag(hd_decomp_ctl) .eq. 0) return
      if (i_decomp_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_decomp_ctl, i_decomp_ctl)
        if(i_decomp_ctl .gt. 0) exit
!
!
        call read_control_array_c1(hd_num_rcb, RCB_dir_ctl)
        call read_control_array_c_i(hd_num_es, ndomain_section_ctl)
        call read_control_array_c1                                      &
     &     (hd_num_r_layerd, ele_grp_layering_ctl)
!
!
        call read_integer_ctl_type(hd_sleeve_level, sleeve_level_old)
!
        call read_chara_ctl_type(hd_part_method, part_method_ctl)
        call read_chara_ctl_type(hd_ele_overlap, element_overlap_ctl)
        call read_chara_ctl_type(hd_sph_sf_file, sphere_file_name_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_metis_in_file, metis_input_file_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_metis_dom_file, metis_domain_file_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_fine_mesh_file, finer_mesh_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_fine_fmt_file, finer_mesh_fmt_ctl)
        call read_chara_ctl_type(hd_fine_itp_file, itp_tbl_head_ctl)
        call read_chara_ctl_type                                        &
     &     (hd_domain_tbl_file, domain_group_file_ctl)
        call read_chara_ctl_type(hd_fmt_itp_tbl, itp_tbl_format_ctl)
      end do
!
      end subroutine read_ctl_data_4_decomp
!
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_ele_ordeirng
!
!
      if(right_begin_flag(hd_ele_ordering_ctl) .eq. 0) return
      if (i_ele_ordering_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_ele_ordering_ctl,                 &
     &     i_ele_ordering_ctl)
        if(i_ele_ordering_ctl .gt. 0) exit
!
        call read_control_array_c1                                      &
     &     (hd_nele_grp_ordering, ele_grp_ordering_ctl)
      end do
!
      end subroutine read_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------!
      end module m_control_data_4_part

