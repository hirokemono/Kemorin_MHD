!
!      module m_control_data_4_refine
!
!      Written by Kemorin on Oct., 2007
!
!      subroutine dealloc_refined_data_ctl
!      subroutine dealloc_refined_code_ctl
!      subroutine read_control_data_4_refiner
!      subroutine read_ctl_data_4_refine_mesh
!
      module m_control_data_4_refine
!
      use m_precision
!
      use m_read_control_elements
      use skip_comment_f
!
      implicit    none
!
      integer (kind = kint), parameter :: control_file_code = 13
      character (len = kchara), parameter                               &
     &         :: control_file_name = 'ctl_refine'
!
      character (len = kchara) :: orginal_mesh_head_ctl
      character (len = kchara) :: refine_mesh_head_ctl
!
      character (len = kchara) :: coarse_2_fine_head_ctl
      character (len = kchara) :: fine_2_course_head_ctl
      character (len = kchara) :: refine_info_head_ctl
      character (len = kchara) :: old_refine_info_head_ctl
!
      character (len = kchara) :: interpolate_type_ctl
!
      integer(kind = kint) :: num_refine_type_ctl = 0
      character (len = kchara), allocatable :: refined_ele_grp_ctl(:)
      character (len = kchara), allocatable :: refined_ele_type_ctl(:)
!
      integer(kind = kint) :: num_refine_code_ctl = 0
      character (len = kchara), allocatable :: refine_i_ele_grp_ctl(:)
      integer (kind=kint), allocatable :: iflag_refine_type_ctl(:)
!
!
!   Top level
!
      character(len=kchara), parameter :: hd_refine_ctl                 &
     &                      = 'refine_control'
      integer (kind=kint) :: i_refine_ctl = 0
!
!   2nd level for partitioner_control
!
      character(len=kchara), parameter :: hd_single_refine_files        &
     &                      = 'single_itp_table_ctl'
      character(len=kchara), parameter :: hd_refine_param               &
     &                      = 'refine_parameter_ctl'
      integer (kind=kint) :: i_single_refine_files =  0
      integer (kind=kint) :: i_refine_param = 0
!
!   3rd level for partitioner_control
!
      character(len=kchara), parameter :: hd_org_f_ctl                  &
     &                      = 'orginal_mesh_head_ctl'
      character(len=kchara), parameter :: hd_refined_f_ctl              &
     &                      = 'refined_mesh_head_ctl'
      character(len=kchara), parameter :: hd_course_to_fine_ctl         &
     &                      = 'cource_to_fine_table_ctl'
      character(len=kchara), parameter :: hd_fine_to_course_ctl         &
     &                      = 'fine_to_cource_table_ctl'
      character(len=kchara), parameter :: hd_refine_info_ctl            &
     &                      = 'refine_info_head_ctl'
      character(len=kchara), parameter :: hd_old_refine_info_ctl        &
     &                      = 'old_refine_info_head_ctl'
!
      integer (kind=kint) :: i_org_f_ctl =           0
      integer (kind=kint) :: i_refined_f_ctl =       0
      integer (kind=kint) :: i_course_to_fine_ctl =  0
      integer (kind=kint) :: i_fine_to_course_ctl =  0
      integer (kind=kint) :: i_refine_info_ctl =     0
      integer (kind=kint) :: i_old_refine_info_ctl = 0
!
!   3rd level for refine_parameter_ctl
!
      character(len=kchara), parameter :: hd_itp_type                   &
     &                      = 'interpolate_type_ctl'
      character(len=kchara), parameter :: hd_num_ref_type               &
     &                      = 'refine_data_ctl'
      character(len=kchara), parameter :: hd_num_ref_code               &
     &                      = 'refine_code_ctl'
!
      integer (kind=kint) :: i_itp_type = 0
      integer (kind=kint) :: i_num_ref_type = 0
      integer (kind=kint) :: i_num_ref_code = 0
!
!
      private :: control_file_name
      private :: hd_refine_ctl, i_refine_ctl
      private :: hd_single_refine_files, i_single_refine_files
      private :: hd_refine_param
      private :: i_refine_param
      private :: hd_org_f_ctl, hd_refined_f_ctl
      private :: hd_course_to_fine_ctl, hd_fine_to_course_ctl
      private :: hd_refine_info_ctl, hd_old_refine_info_ctl
!
      private :: alloc_refined_data_ctl, alloc_refined_code_ctl
      private :: read_refine_control_data
      private :: read_ctl_data_4_refine_type
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_refined_data_ctl
!
!
      allocate( refined_ele_grp_ctl(num_refine_type_ctl) )
      allocate( refined_ele_type_ctl(num_refine_type_ctl) )
!
      end subroutine alloc_refined_data_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_refined_code_ctl
!
!
      allocate( refine_i_ele_grp_ctl(num_refine_code_ctl) )
      allocate( iflag_refine_type_ctl(num_refine_code_ctl) )
!
      end subroutine alloc_refined_code_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_refined_data_ctl
!
      deallocate(refined_ele_grp_ctl, refined_ele_type_ctl)
!
      end subroutine dealloc_refined_data_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_refined_code_ctl
!
      deallocate(refine_i_ele_grp_ctl, iflag_refine_type_ctl)
!
      end subroutine dealloc_refined_code_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_data_4_refiner
!
!
      ctl_file_code = control_file_code
!
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_refine_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_4_refiner
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------!
       subroutine read_refine_control_data
!
!
      if(right_begin_flag(hd_refine_ctl) .eq. 0) return
      if (i_refine_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_refine_ctl, i_refine_ctl)
        if(i_refine_ctl .gt. 0) exit
!
!
        call read_ctl_data_4_refine_mesh
        call read_ctl_data_4_refine_type
      end do
!
      end subroutine read_refine_control_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_ctl_data_4_refine_mesh
!
!
      if(right_begin_flag(hd_single_refine_files) .eq. 0) return
      if (i_single_refine_files .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_single_refine_files,              &
     &      i_single_refine_files)
        if(i_single_refine_files .gt. 0) exit
!
!
        call read_character_ctl_item(hd_org_f_ctl,                      &
     &           i_org_f_ctl, orginal_mesh_head_ctl)
        call read_character_ctl_item(hd_refined_f_ctl,                  &
     &           i_refined_f_ctl, refine_mesh_head_ctl)
!
        call read_character_ctl_item(hd_course_to_fine_ctl,             &
     &           i_course_to_fine_ctl, coarse_2_fine_head_ctl)
        call read_character_ctl_item(hd_fine_to_course_ctl,             &
     &           i_fine_to_course_ctl, fine_2_course_head_ctl)
!
        call read_character_ctl_item(hd_refine_info_ctl,                &
     &           i_refine_info_ctl, refine_info_head_ctl)
        call read_character_ctl_item(hd_old_refine_info_ctl,            &
     &           i_old_refine_info_ctl, old_refine_info_head_ctl)
      end do
!
      end subroutine read_ctl_data_4_refine_mesh
!
! -----------------------------------------------------------------------!
      subroutine read_ctl_data_4_refine_type
!
!
      if(right_begin_flag(hd_refine_param) .eq. 0) return
      if (i_refine_param .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_refine_param, i_refine_param)
        if(i_refine_param .gt. 0) exit
!
!
        call find_control_array_flag(hd_num_ref_type,                   &
     &      num_refine_type_ctl)
        if(num_refine_type_ctl.gt.0 .and. i_num_ref_type.eq.0) then
          call alloc_refined_data_ctl
          call read_control_array_chara2_list(hd_num_ref_type,          &
     &        num_refine_type_ctl, i_num_ref_type,                      &
     &        refined_ele_grp_ctl, refined_ele_type_ctl)
        end if
!
        call find_control_array_flag(hd_num_ref_code,                   &
     &      num_refine_code_ctl)
        if(num_refine_code_ctl.gt.0 .and. i_num_ref_code.eq.0) then
          call alloc_refined_code_ctl
          call read_control_array_int_v_list(hd_num_ref_code,           &
     &        num_refine_code_ctl, i_num_ref_code,                      &
     &        refine_i_ele_grp_ctl, iflag_refine_type_ctl)
        end if
!
!
        call read_character_ctl_item(hd_itp_type,                       &
     &            i_itp_type, interpolate_type_ctl)
      end do
!
      end subroutine read_ctl_data_4_refine_type
!
! -----------------------------------------------------------------------!
      end module m_control_data_4_refine
