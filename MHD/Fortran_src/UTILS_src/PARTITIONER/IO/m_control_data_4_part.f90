!
!      module m_control_data_4_part
!
!      Written by Kemorin on Sep. 2007
!
!      subroutine dealloc_ele_grp_ordering_ctl
!      subroutine dealloc_ele_grp_layer_ctl
!       subroutine read_control_data_4_part
!
      module m_control_data_4_part
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
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
      character (len = kchara) :: part_method_ctl
      character (len = kchara) :: element_overlap_ctl
      integer(kind = kint) :: sleeve_level_ctl = 0
!
      integer(kind = kint) :: num_RCB_level_ctl = 0
      character(len=kchara), allocatable :: RCB_dir_ctl(:)
!
      integer(kind = kint) :: n_section_ctl = 0
      integer(kind = kint) :: num_section_ctl(3) = 0
      character(len=kchara) :: es_dir_ctl(3)
!
      integer(kind = kint) :: num_radial_layering_ctl = 0
      character(len=kchara), allocatable :: ele_grp_layer_ctl(:)
!
      character(len=kchara) :: sphere_file_name_ctl
      character(len=kchara) :: metis_input_file_ctl
      character(len=kchara) :: metis_domain_file_ctl
      character(len=kchara) :: domain_group_file_ctl
      character(len=kchara) :: finer_mesh_head_ctl
      character(len=kchara) :: itp_tbl_head_ctl
      character(len=kchara) :: itp_tbl_format_ctl
!
      integer(kind = kint) :: nele_grp_ordering_ctl = 0
      character(len=kchara), allocatable :: ele_grp_ordering_ctl(:)
!
!   Top level
!
      character(len=kchara), parameter :: hd_part_ctl                   &
     &                      = 'partitioner_control'
      integer (kind=kint) :: i_part_ctl = 0
!
!   2nd level for partitioner_control
!
      character(len=kchara), parameter :: hd_org_f_ctl                  &
     &                      = 'original_file_ctl'
      character(len=kchara), parameter :: hd_ele_ordering_ctl           &
     &                      = 'ordering_by_ele_grp'
      character(len=kchara), parameter :: hd_decomp_ctl                 &
     &                      = 'decompose_ctl'
!
      integer (kind=kint) :: i_org_f_ctl =        0
      integer (kind=kint) :: i_ele_ordering_ctl = 0
      integer (kind=kint) :: i_decomp_ctl =       0
!
!   3rd level for ordering_ctl
!
      character(len=kchara), parameter :: hd_nele_grp_ordering          &
     &                      = 'ordering_ele_grp_ctl'
      integer (kind=kint) :: i_nele_grp_ordering =  0
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
      integer (kind=kint) :: i_part_method =  0
      integer (kind=kint) :: i_sleeve_level = 0
      integer (kind=kint) :: i_ele_overlap =  0
!
!     RCB
      character(len=kchara), parameter :: hd_num_rcb = 'RCB_dir_ctl'
      integer (kind=kint) :: i_num_rcb =  0
!
!     ES
      character(len=kchara), parameter :: hd_num_es = 'dir_domain_ctl'
      integer (kind=kint) :: i_num_es =  0
!
!     layered_ES_ctl
      character(len=kchara), parameter :: hd_num_r_layerd               &
     &                      = 'radial_layering_grp_ctl'
      integer (kind=kint) :: i_num_r_layerd =  0
!
!     cubed sphere
      character(len=kchara), parameter :: hd_sph_sf_file                &
     &                      = 'sphere_file_name_ctl'
      integer (kind=kint) :: i_sph_sf_file =  0
!
!     metis input
      character(len=kchara), parameter :: hd_metis_in_file              &
     &                      = 'metis_input_file_ctl'
      integer (kind=kint) :: i_metis_in_file =  0
!
!     metis resules
      character(len=kchara), parameter :: hd_metis_dom_file             &
     &                      = 'metis_domain_file_ctl'
      integer (kind=kint) :: i_metis_dom_file =  0
!
!     grouping data file
      character(len=kchara), parameter :: hd_domain_tbl_file            &
     &                      = 'domain_group_file_ctl'
      character(len=kchara), parameter :: hd_fine_mesh_file             &
     &                      = 'finer_mesh_head_ctl'
      character(len=kchara), parameter :: hd_fine_itp_file              &
     &                      = 'interpolate_tbl_head'
      character(len=kchara), parameter                                  &
     &         :: hd_fmt_itp_tbl =    'interpolate_table_format_ctl'

      integer (kind=kint) :: i_domain_tbl_file =  0
      integer (kind=kint) :: i_fine_mesh_file =   0
      integer (kind=kint) :: i_fine_itp_file =    0
      integer (kind=kint) :: i_fmt_itp_tbl =      0
!
!
      private :: control_file_name
      private :: my_rank
      private :: hd_part_ctl, i_part_ctl
      private :: hd_org_f_ctl
      private :: hd_ele_ordering_ctl, hd_decomp_ctl
      private :: i_ele_ordering_ctl, i_decomp_ctl
      private :: hd_nele_grp_ordering
      private :: hd_part_method, hd_sleeve_level, hd_ele_overlap
      private :: hd_num_rcb, hd_num_es, hd_num_r_layerd, hd_sph_sf_file
      private :: hd_metis_in_file, hd_metis_dom_file
      private :: hd_domain_tbl_file, hd_fine_mesh_file
      private :: hd_fine_itp_file, hd_fmt_itp_tbl
!
      private :: alloc_ele_grp_ordering_ctl
      private :: alloc_ele_grp_layer_ctl
      private :: read_part_control_data
      private :: read_ctl_data_4_decomp, read_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ele_grp_ordering_ctl
!
      allocate(ele_grp_ordering_ctl(nele_grp_ordering_ctl) )
!
      end subroutine alloc_ele_grp_ordering_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ele_grp_layer_ctl
!
     allocate( ele_grp_layer_ctl(num_radial_layering_ctl) )
!
      end subroutine alloc_ele_grp_layer_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_ordering_ctl
!
      deallocate(ele_grp_ordering_ctl)
!
      end subroutine dealloc_ele_grp_ordering_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ele_grp_layer_ctl
!
      deallocate( ele_grp_layer_ctl )
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
      use m_ctl_data_4_platforms
      use m_ctl_data_4_org_data
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
        call read_ctl_data_4_platform
        call read_ctl_data_4_org_data
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
        call find_control_array_flag(hd_num_rcb, num_RCB_level_ctl)
        if(num_RCB_level_ctl.gt.0 .and. i_num_rcb.eq.0) then
          allocate( RCB_dir_ctl(num_RCB_level_ctl) )
          call read_control_array_chara_list(hd_num_rcb,                &
     &        num_RCB_level_ctl, i_num_rcb, RCB_dir_ctl)
        end if
!
        call find_control_array_flag(hd_num_r_layerd,                   &
     &      num_radial_layering_ctl)
        if(num_radial_layering_ctl.gt.0 .and. i_num_r_layerd.eq.0) then
          call alloc_ele_grp_layer_ctl
          call read_control_array_chara_list(hd_num_r_layerd,           &
     &        num_radial_layering_ctl, i_num_r_layerd,                  &
     &        ele_grp_layer_ctl)
        end if
!
        call find_control_array_flag(hd_num_es, n_section_ctl)
        if(n_section_ctl.gt.0 .and. i_num_es.eq.0) then
          if(n_section_ctl .ne. 3)                                      &
     &         stop 'number of subdomain should be 3 directions'
          call read_control_array_int_v_list(hd_num_es,                 &
     &        n_section_ctl, i_num_es, es_dir_ctl, num_section_ctl)
        end if
!
!
        call read_integer_ctl_item(hd_sleeve_level,                     &
     &            i_sleeve_level, sleeve_level_ctl)
!
        call read_character_ctl_item(hd_part_method,                    &
     &            i_part_method, part_method_ctl)
        call read_character_ctl_item(hd_ele_overlap,                    &
     &            i_ele_overlap, element_overlap_ctl)
        call read_character_ctl_item(hd_sph_sf_file,                    &
     &            i_sph_sf_file, sphere_file_name_ctl)
        call read_character_ctl_item(hd_metis_in_file,                  &
     &            i_metis_in_file, metis_input_file_ctl)
        call read_character_ctl_item(hd_metis_dom_file,                 &
     &            i_metis_dom_file, metis_domain_file_ctl)
        call read_character_ctl_item(hd_fine_mesh_file,                 &
     &            i_fine_mesh_file, finer_mesh_head_ctl)
        call read_character_ctl_item(hd_fine_itp_file,                  &
     &            i_fine_itp_file, itp_tbl_head_ctl)
        call read_character_ctl_item(hd_domain_tbl_file,                &
     &            i_domain_tbl_file, domain_group_file_ctl)
        call read_character_ctl_item(hd_fmt_itp_tbl,                    &
     &            i_fmt_itp_tbl, itp_tbl_format_ctl)
      end do
!
      end subroutine read_ctl_data_4_decomp
!
! -----------------------------------------------------------------------!
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
        call find_control_array_flag(hd_nele_grp_ordering,              &
     &      nele_grp_ordering_ctl)
        if(nele_grp_ordering_ctl.gt.0                                   &
     &      .and. i_nele_grp_ordering.eq.0) then
          call alloc_ele_grp_ordering_ctl
          call read_control_array_chara_list(hd_nele_grp_ordering,      &
     &        nele_grp_ordering_ctl, i_nele_grp_ordering,               &
     &        ele_grp_ordering_ctl)
        end if
      end do
!
      end subroutine read_ctl_data_4_ele_ordeirng
!
! -----------------------------------------------------------------------!
      end module m_control_data_4_part

