!t_control_param_4_refiner.f90
!      module t_control_param_4_refiner
!
!      Written by Kemorin on Oct., 2007
!
!!      subroutine dealloc_refine_param(refine_p)
!!      subroutine dealloc_refine_param_chara(refine_p)
!!
!!      subroutine set_control_4_refiner
!!        type(control_data_4_refine), intent(in) :: refine_ctl
!
      module t_control_param_4_refiner
!
      use m_precision
      use t_file_IO_parameter
!
      implicit    none
!
!
      character(len = kchara), parameter :: cflag_div_rtp1 =  'rtp'
      character(len = kchara), parameter                                &
     &              :: cflag_div_rtp2 = 'spherical'
      character(len = kchara), parameter                                &
     &              :: cflag_proj_sph1 = 'project_sphere'
      character(len = kchara), parameter                                &
     &              :: cflag_proj_sph2 = 'project_to_sphere'
!
      integer(kind = kint), parameter :: iflag_div_xyz =  0
      integer(kind = kint), parameter :: iflag_div_rtp =  1
      integer(kind = kint), parameter :: iflag_proj_sph = 2
!
      type ctl_param_4_refiner
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
        type(field_IO_params) ::  original_mesh_file
        type(field_IO_params) ::  refined_mesh_file
!
        character(len=kchara) :: course_2_fine_head = 'course_2_fine'
        character(len=kchara) :: fine_2_course_head = 'fine_2_course'
!
        character(len = kchara) :: refine_info_head = 'refine_info'
        character(len = kchara) :: old_refine_info_head = 'refine_info.0'
        integer(kind = kint) :: iflag_read_old_refine_file = 0
      end type ctl_param_4_refiner
!
      private :: alloc_refine_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_refine_param(refine_p)
!
      type(ctl_param_4_refiner), intent(inout) :: refine_p
!
!
      allocate(refine_p%refined_ele_grp(refine_p%num_refine_type) )
      allocate(refine_p%refined_ele_type(refine_p%num_refine_type) )
      allocate(refine_p%id_refined_ele_grp(refine_p%num_refine_type) )
      allocate(refine_p%iflag_refine_type(refine_p%num_refine_type) )
!
      refine_p%id_refined_ele_grp = 0
      refine_p%iflag_refine_type = 0
!
      end subroutine alloc_refine_param
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_refine_param(refine_p)
!
      type(ctl_param_4_refiner), intent(inout) :: refine_p
!
!
      deallocate(refine_p%id_refined_ele_grp)
      deallocate(refine_p%iflag_refine_type)
!
      end subroutine dealloc_refine_param
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_refine_param_chara(refine_p)
!
      type(ctl_param_4_refiner), intent(inout) :: refine_p
!
!
      deallocate(refine_p%refined_ele_grp)
      deallocate(refine_p%refined_ele_type)
!
      end subroutine dealloc_refine_param_chara
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_control_4_refiner(refine_ctl, refine_p)
!
      use t_control_data_4_refine
      use m_default_file_prefix
      use skip_comment_f
      use set_control_platform_data
!
      type(control_data_4_refine), intent(in) :: refine_ctl
      type(ctl_param_4_refiner), intent(inout) :: refine_p
!
      character(len = kchara) :: tmpchara
      integer(kind = kint) :: i
!
!
      call set_control_mesh_def                                         &
     &   (refine_ctl%source_plt, refine_p%original_mesh_file)
      call set_control_mesh_file_def(def_new_mesh_head,                 &
     &    refine_ctl%refined_plt, refine_p%refined_mesh_file)
!
!
      if (refine_ctl%coarse_2_fine_head_ctl%iflag .gt. 0) then
        refine_p%course_2_fine_head                                     &
     &      = refine_ctl%coarse_2_fine_head_ctl%charavalue
      end if
!
      if (refine_ctl%fine_2_course_head_ctl%iflag .gt. 0) then
        refine_p%fine_2_course_head                                     &
     &      = refine_ctl%fine_2_course_head_ctl%charavalue
      end if
!
      if (refine_ctl%refine_info_head_ctl%iflag .gt. 0) then
        refine_p%refine_info_head                                       &
     &      = refine_ctl%refine_info_head_ctl%charavalue
      end if
!
      refine_p%iflag_read_old_refine_file                               &
     &        = refine_ctl%old_refine_info_head_ctl%iflag
      if (refine_p%iflag_read_old_refine_file .gt. 0) then
        refine_p%old_refine_info_head                                   &
     &         = refine_ctl%old_refine_info_head_ctl%charavalue
      end if
!
!
      refine_p%iflag_interpolate_type = iflag_div_xyz
      if (refine_ctl%interpolate_type_ctl%iflag .gt. 0) then
        tmpchara = refine_ctl%interpolate_type_ctl%charavalue
        if (   cmp_no_case(tmpchara, cflag_proj_sph1)                   &
     &    .or. cmp_no_case(tmpchara, cflag_proj_sph2)) then
          refine_p%iflag_interpolate_type = iflag_proj_sph
!        else if (cmp_no_case(tmpchara, cflag_div_rtp1)                 &
!     &      .or. cmp_no_case(tmpchara, cflag_div_rtp2)) then
!          refine_p%iflag_interpolate_type = iflag_div_rtp
        end if
      end if
!
!
      if (refine_ctl%refined_ele_grp_ctl%icou .gt. 0) then
        refine_p%num_refine_type = refine_ctl%refined_ele_grp_ctl%num
      else if(refine_ctl%refine_i_ele_grp_ctl%icou .gt. 0) then
        refine_p%num_refine_type = refine_ctl%refine_i_ele_grp_ctl%num
      else
        write(*,*) 'set refine type and area'
        stop
      end if
!
      if(refine_p%num_refine_type .gt. 0) then
        call alloc_refine_param(refine_p)
!
        if (refine_ctl%refined_ele_grp_ctl%icou .gt. 0) then
!
          refine_p%iflag_redefine_tri = 1
          do i = 1, refine_p%num_refine_type
            refine_p%refined_ele_grp(i)                                 &
     &         = refine_ctl%refined_ele_grp_ctl%c1_tbl(i)
            refine_p%refined_ele_type(i)                                &
     &         = refine_ctl%refined_ele_grp_ctl%c2_tbl(i)
          end do
!
        else if(refine_ctl%refine_i_ele_grp_ctl%icou .gt. 0) then
!
          refine_p%iflag_redefine_tri = 0
          do i = 1, refine_p%num_refine_type
            refine_p%refined_ele_grp(i)                                 &
     &        = refine_ctl%refine_i_ele_grp_ctl%c_tbl(i)
            refine_p%iflag_refine_type(i)                               &
     &        = refine_ctl%refine_i_ele_grp_ctl%ivec(i)
          end do
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
      end module t_control_param_4_refiner
