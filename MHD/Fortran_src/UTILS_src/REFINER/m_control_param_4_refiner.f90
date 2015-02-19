!m_control_param_4_refiner.f90
!      module m_control_param_4_refiner
!
      module m_control_param_4_refiner
!
!      Written by Kemorin on Oct., 2007
!
      use m_precision
!
      implicit    none
!
!
      integer (kind = kint) :: iflag_interpolate_type
!
      integer(kind = kint) :: num_refine_type = 0
      character (len = kchara), allocatable :: refined_ele_grp(:)
      character (len = kchara), allocatable :: refined_ele_type(:)
      integer (kind=kint), allocatable :: id_refined_ele_grp(:)
      integer (kind=kint), allocatable :: iflag_refine_type(:)
      integer (kind=kint) :: iflag_redefine_tri
!
      integer(kind = kint) :: iflag_small_tri_refine = 1
!
      character(len=kchara) :: original_mesh_head
      character(len=kchara) :: refined_mesh_head = 'in'
!
      character(len=kchara) :: course_2_fine_head = 'course_2_fine'
      character(len=kchara) :: fine_2_course_head = 'fine_2_course'
!
      character(len = kchara) :: refine_info_head = 'refine_info'
      character(len = kchara) :: old_refine_info_head = 'refine_info.0'
      integer(kind = kint) :: iflag_read_old_refine_file = 0
!
      private :: allocate_refine_param
!
!      subroutine allocate_refine_param
!      subroutine deallocate_refine_param
!      subroutine deallocate_refine_param_chara
!
!      subroutine set_control_4_refiner
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_refine_param
!
      allocate(refined_ele_grp(num_refine_type) )
      allocate(refined_ele_type(num_refine_type) )
      allocate(id_refined_ele_grp(num_refine_type) )
      allocate(iflag_refine_type(num_refine_type) )
!
      id_refined_ele_grp = 0
      iflag_refine_type = 0
!
      end subroutine allocate_refine_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_refine_param
!
      deallocate(id_refined_ele_grp)
      deallocate(iflag_refine_type)
!
      end subroutine deallocate_refine_param
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_refine_param_chara
!
      deallocate(refined_ele_grp)
      deallocate(refined_ele_type)
!
      end subroutine deallocate_refine_param_chara
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_4_refiner
!
      use m_control_data_4_refine
      use m_ctl_data_4_platforms
      use m_ctl_data_4_2nd_data
      use skip_comment_f
!
!
      if (i_mesh_header .gt. 0) then
        original_mesh_head = mesh_file_prefix
        write(*,*) 'original_mesh_head: ', trim(original_mesh_head)
      else
        write(*,*) 'set original mesh header'
        stop
      end if
!
      if (i_new_mesh_head .gt. 0) then
        refined_mesh_head = new_mesh_prefix
        write(*,*) 'refined_mesh_head: ', trim(refined_mesh_head)
      else
        write(*,*) 'set refined mesh header'
        stop
      end if
!
!
      if (i_course_to_fine_ctl .gt. 0) then
        course_2_fine_head = coarse_2_fine_head_ctl
      end if
!
      if (i_fine_to_course_ctl .gt. 0) then
        fine_2_course_head = fine_2_course_head_ctl
      end if
!
      if (i_refine_info_ctl .gt. 0) then
        refine_info_head = refine_info_head_ctl
      end if
!
      iflag_read_old_refine_file = i_old_refine_info_ctl
      if (iflag_read_old_refine_file .gt. 0) then
        old_refine_info_head = old_refine_info_head_ctl
      end if
!
!
      iflag_interpolate_type = 0
      if (i_itp_type .gt. 0) then
        if (   cmp_no_case(interpolate_type_ctl, 'project_sphere')      &
     &    .or. cmp_no_case(interpolate_type_ctl, 'project_to_sphere')   &
     &     ) then
          iflag_interpolate_type = 2
!        else if (cmp_no_case(interpolate_type_ctl, 'rtp')              &
!     &      .or. cmp_no_case(interpolate_type_ctl, 'spherical')) then
!          iflag_interpolate_type = 1
        end if
      end if
!
!
      if (i_num_ref_type .gt. 0) then
        num_refine_type = num_refine_type_ctl
      else if (i_num_ref_code .gt. 0) then
        num_refine_type = num_refine_code_ctl
      else
        write(*,*) 'set refine type and area'
        stop
      end if
!
      if (num_refine_type .gt. 0) then
        call allocate_refine_param
!
        if (i_num_ref_type .gt. 0) then
!
          iflag_redefine_tri = 1
          refined_ele_grp(1:num_refine_type)                            &
     &         = refined_ele_grp_ctl(1:num_refine_type)
          refined_ele_type(1:num_refine_type)                           &
     &         = refined_ele_type_ctl(1:num_refine_type)
          deallocate(refined_ele_grp_ctl)
          deallocate(refined_ele_type_ctl)
!
        else if (i_num_ref_code .gt. 0) then
!
          iflag_redefine_tri = 0
          refined_ele_grp(1:num_refine_type)                            &
     &         = refine_i_ele_grp_ctl(1:num_refine_type)
          iflag_refine_type(1:num_refine_type)                          &
     &         = iflag_refine_type_ctl(1:num_refine_type)
          deallocate(refine_i_ele_grp_ctl)
          deallocate(iflag_refine_type_ctl)
!
        end if
!
      else
        write(*,*) 'set refine type and area'
        stop
      end if
!
      end subroutine set_control_4_refiner
!
! -----------------------------------------------------------------------
!
      end module m_control_param_4_refiner
