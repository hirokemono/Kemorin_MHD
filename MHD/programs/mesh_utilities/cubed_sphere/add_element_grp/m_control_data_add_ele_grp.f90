!m_control_data_add_ele_grp.f90
!      module m_control_data_add_ele_grp
!
      module m_control_data_add_ele_grp
!
!      Written by H. Matsui on Mar., 2008
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
     &         :: control_file_name = 'ctl_add_ele_grp'
!
      character (len = kchara) :: orginal_mesh_head_ctl
      character (len = kchara) :: modified_mesh_head_ctl
      character (len = kchara) :: layered_mesh_head_ctl
!
      character (len = kchara) :: sph_grp_direction_ctl
      integer (kind = kint) :: num_r_ele_grouping_ctl = 0
      integer (kind = kint) :: num_t_ele_grouping_ctl = 0
      integer (kind = kint) :: num_s_ele_grouping_ctl = 0
      integer (kind = kint) :: num_z_ele_grouping_ctl = 0
      character (len = kchara), allocatable :: r_ele_grping_name_ctl(:)
      character (len = kchara), allocatable :: t_ele_grping_name_ctl(:)
      character (len = kchara), allocatable :: s_ele_grping_name_ctl(:)
      character (len = kchara), allocatable :: z_ele_grping_name_ctl(:)
      real(kind = kreal), allocatable :: min_r_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: max_r_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: min_t_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: max_t_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: min_s_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: max_s_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: min_z_ele_grping_ctl(:)
      real(kind = kreal), allocatable :: max_z_ele_grping_ctl(:)
!
!   Top level
!
      character(len=kchara), parameter :: hd_add_ele_grp_ctl            &
     &                      = 'add_element_groups'
      integer (kind=kint) :: i_add_ele_grp_ctl = 0
!
!   2nd level for add_element_groups
!
      character(len=kchara), parameter :: hd_files_ctl                  &
     &                      = 'file_name_ctl'
      character(len=kchara), parameter :: hd_add_ele_grp_para           &
     &                      = '2d_grouping_ctl'
      integer (kind=kint) :: i_files_ctl =        0
      integer (kind=kint) :: i_add_ele_grp_para = 0
!
!   3rd level for file_names
!
      character(len=kchara), parameter :: hd_org_mesh_ctl               &
     &                      = 'orginal_mesh_head_ctl'
      character(len=kchara), parameter :: hd_new_mesh_ctl               &
     &                      = 'new_mesh_head_ctl'
      character(len=kchara), parameter :: hd_layered_mesh_head          &
     &                      = 'grouping_mesh_head_ctl'
!
      integer (kind=kint) :: i_org_mesh_ctl = 0
      integer (kind=kint) :: i_new_mesh_ctl = 0
      integer (kind=kint) :: i_layered_mesh_head = 0
!
!   3rd level for element_group_ctl
!
      character(len=kchara), parameter :: hd_2nd_grp_direction          &
     &                      = '2nd_grp_direction_ctl'
      character(len=kchara), parameter :: hd_num_r_ele_grping           &
     &                      = 'radial_range_ctl'
      character(len=kchara), parameter :: hd_num_t_ele_grping           &
     &                      = 'theta_range_ctl'
      character(len=kchara), parameter :: hd_num_s_ele_grping           &
     &                      = 's_range_ctl'
      character(len=kchara), parameter :: hd_num_z_ele_grping           &
     &                      = 'z_range_ctl'
!
      integer (kind=kint) :: i_2nd_grp_direction = 0
      integer (kind=kint) :: i_num_r_ele_grping =  0
      integer (kind=kint) :: i_num_t_ele_grping =  0
      integer (kind=kint) :: i_num_s_ele_grping =  0
      integer (kind=kint) :: i_num_z_ele_grping =  0
!
      private :: hd_add_ele_grp_ctl, i_add_ele_grp_ctl
      private :: hd_files_ctl, i_files_ctl
      private :: hd_add_ele_grp_para, i_add_ele_grp_para
      private :: hd_org_mesh_ctl, hd_new_mesh_ctl, hd_layered_mesh_head
      private :: hd_2nd_grp_direction
      private :: hd_num_r_ele_grping, hd_num_t_ele_grping
      private :: hd_num_s_ele_grping, hd_num_z_ele_grping
!
      private :: read_control_4_add_egrp_data
      private :: read_ctl_data_4_add_egrp_mesh
      private :: read_ctl_data_4_add_2d_egrp
      private :: alloc_r_ele_grp_list_ctl, alloc_t_ele_grp_list_ctl
      private :: alloc_s_ele_grp_list_ctl, alloc_z_ele_grp_list_ctl
!
!       subroutine read_control_add_elegrp
!
!      subroutine dealloc_r_ele_grp_list_ctl
!      subroutine dealloc_t_ele_grp_list_ctl
!      subroutine dealloc_s_ele_grp_list_ctl
!      subroutine dealloc_z_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_r_ele_grp_list_ctl
!
!
      allocate(r_ele_grping_name_ctl(num_r_ele_grouping_ctl))
      allocate(min_r_ele_grping_ctl(num_r_ele_grouping_ctl))
      allocate(max_r_ele_grping_ctl(num_r_ele_grouping_ctl))
      if (num_r_ele_grouping_ctl .gt. 0) then
        min_r_ele_grping_ctl = 0.0d0
        max_r_ele_grping_ctl = 0.0d0
      end if
!
      end subroutine alloc_r_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_t_ele_grp_list_ctl
!
!
      allocate(t_ele_grping_name_ctl(num_t_ele_grouping_ctl))
      allocate(min_t_ele_grping_ctl(num_t_ele_grouping_ctl))
      allocate(max_t_ele_grping_ctl(num_t_ele_grouping_ctl))
      if (num_t_ele_grouping_ctl .gt. 0) then
        min_t_ele_grping_ctl = 0.0d0
        max_t_ele_grping_ctl = 0.0d0
      end if
!
      end subroutine alloc_t_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_s_ele_grp_list_ctl
!
!
      allocate(s_ele_grping_name_ctl(num_s_ele_grouping_ctl))
      allocate(min_s_ele_grping_ctl(num_s_ele_grouping_ctl))
      allocate(max_s_ele_grping_ctl(num_s_ele_grouping_ctl))
      if (num_s_ele_grouping_ctl .gt. 0) then
        min_s_ele_grping_ctl = 0.0d0
        max_s_ele_grping_ctl = 0.0d0
      end if
!
      end subroutine alloc_s_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_z_ele_grp_list_ctl
!
!
      allocate(z_ele_grping_name_ctl(num_z_ele_grouping_ctl))
      allocate(min_z_ele_grping_ctl(num_z_ele_grouping_ctl))
      allocate(max_z_ele_grping_ctl(num_z_ele_grouping_ctl))
      if (num_z_ele_grouping_ctl .gt. 0) then
        min_z_ele_grping_ctl = 0.0d0
        max_z_ele_grping_ctl = 0.0d0
      end if
!
      end subroutine alloc_z_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_r_ele_grp_list_ctl
!
!
      deallocate(r_ele_grping_name_ctl)
      deallocate(min_r_ele_grping_ctl, max_r_ele_grping_ctl)
!
      end subroutine dealloc_r_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_t_ele_grp_list_ctl
!
!
      deallocate(t_ele_grping_name_ctl)
      deallocate(min_t_ele_grping_ctl, max_t_ele_grping_ctl)
!
      end subroutine dealloc_t_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_s_ele_grp_list_ctl
!
!
      deallocate(s_ele_grping_name_ctl)
      deallocate(min_s_ele_grping_ctl, max_s_ele_grping_ctl)
!
      end subroutine dealloc_s_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_z_ele_grp_list_ctl
!
!
      deallocate(z_ele_grping_name_ctl)
      deallocate(min_z_ele_grping_ctl, max_z_ele_grping_ctl)
!
      end subroutine dealloc_z_ele_grp_list_ctl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine read_control_add_elegrp
!
!
      ctl_file_code = control_file_code
      open (ctl_file_code, file = control_file_name)
!
      call load_ctl_label_and_line
      call read_control_4_add_egrp_data
!
      close(ctl_file_code)
!
      end subroutine read_control_add_elegrp
!
! -----------------------------------------------------------------------!
       subroutine read_control_4_add_egrp_data
!
!
      if(right_begin_flag(hd_add_ele_grp_ctl) .eq. 0) return
      if (i_add_ele_grp_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_add_ele_grp_ctl,                  &
     &      i_add_ele_grp_ctl)
        if(i_add_ele_grp_ctl .gt. 0) exit
!
        call read_ctl_data_4_add_egrp_mesh
        call read_ctl_data_4_add_2d_egrp
      end do
!
      end subroutine read_control_4_add_egrp_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine read_ctl_data_4_add_egrp_mesh
!
!
      if(right_begin_flag(hd_files_ctl) .eq. 0) return
      if (i_files_ctl .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_files_ctl, i_files_ctl)
        if(i_files_ctl .gt. 0) exit
!
!
        call read_character_ctl_item(hd_org_mesh_ctl,                   &
     &           i_org_mesh_ctl, orginal_mesh_head_ctl)
        call read_character_ctl_item(hd_new_mesh_ctl,                   &
     &           i_new_mesh_ctl, modified_mesh_head_ctl)
        call read_character_ctl_item(hd_layered_mesh_head,              &
     &           i_layered_mesh_head, layered_mesh_head_ctl)
      end do
!
      end subroutine read_ctl_data_4_add_egrp_mesh
!
! -----------------------------------------------------------------------!
      subroutine read_ctl_data_4_add_2d_egrp
!
!
      if(right_begin_flag(hd_add_ele_grp_para) .eq. 0) return
      if (i_add_ele_grp_para .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_add_ele_grp_para,                 &
     &      i_add_ele_grp_para)
        if(i_add_ele_grp_para .gt. 0) exit
!
        call find_control_array_flag(hd_num_r_ele_grping,               &
     &      num_r_ele_grouping_ctl)
        if(num_r_ele_grouping_ctl.gt.0                                  &
     &      .and. i_num_r_ele_grping.eq.0) then
          call alloc_r_ele_grp_list_ctl
          call read_control_array_c_r2_list(hd_num_r_ele_grping,        &
     &        num_r_ele_grouping_ctl, i_num_r_ele_grping,               &
     &        r_ele_grping_name_ctl, min_r_ele_grping_ctl,              &
     &        max_r_ele_grping_ctl)
        end if
!
        call find_control_array_flag(hd_num_t_ele_grping,               &
     &      num_t_ele_grouping_ctl)
        if(num_t_ele_grouping_ctl.gt.0                                  &
     &      .and. i_num_t_ele_grping.eq.0) then
          call alloc_t_ele_grp_list_ctl
          call read_control_array_c_r2_list(hd_num_t_ele_grping,        &
     &        num_t_ele_grouping_ctl, i_num_t_ele_grping,               &
     &        t_ele_grping_name_ctl, min_t_ele_grping_ctl,              &
     &        max_t_ele_grping_ctl)
        end if
!
        call find_control_array_flag(hd_num_s_ele_grping,               &
     &      num_s_ele_grouping_ctl)
        if(num_s_ele_grouping_ctl.gt.0                                  &
     &      .and. i_num_s_ele_grping.eq.0) then
          call alloc_s_ele_grp_list_ctl
          call read_control_array_c_r2_list(hd_num_s_ele_grping,        &
     &        num_s_ele_grouping_ctl, i_num_s_ele_grping,               &
     &        s_ele_grping_name_ctl, min_s_ele_grping_ctl,              &
     &        max_s_ele_grping_ctl)
        end if
!
        call find_control_array_flag(hd_num_z_ele_grping,               &
     &      num_z_ele_grouping_ctl)
        if(num_z_ele_grouping_ctl.gt.0                                  &
     &      .and. i_num_z_ele_grping.eq.0) then
          call alloc_z_ele_grp_list_ctl
          call read_control_array_c_r2_list(hd_num_z_ele_grping,        &
     &        num_z_ele_grouping_ctl, i_num_z_ele_grping,               &
     &        z_ele_grping_name_ctl, min_z_ele_grping_ctl,              &
     &        max_z_ele_grping_ctl)
        end if
!
!
        call read_character_ctl_item(hd_2nd_grp_direction,              &
     &          i_2nd_grp_direction, sph_grp_direction_ctl)
      end do
!
      end subroutine read_ctl_data_4_add_2d_egrp
!
! -----------------------------------------------------------------------!
      end module m_control_data_add_ele_grp

