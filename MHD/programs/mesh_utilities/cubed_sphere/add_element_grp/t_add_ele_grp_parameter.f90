!t_add_ele_grp_parameter.f90
!      module t_add_ele_grp_parameter
!
!      Written by H. Matsui on Oct., 2007
!
!!      subroutine alloc_add_r_ele_grping(num, add_egrp_param)
!!      subroutine alloc_add_t_ele_grping(num, add_egrp_param)
!!      subroutine alloc_add_s_ele_grping(num, add_egrp_param)
!!      subroutine alloc_add_z_ele_grping(num, add_egrp_param)
!!        integer(kind = kint), intent(in) :: num
!!        type(add_ele_grp_param), intent(inout) :: add_egrp_param
!!
!!      subroutine dealloc_add_r_ele_grping(add_egrp_param)
!!      subroutine dealloc_add_t_ele_grping(add_egrp_param)
!!      subroutine dealloc_add_s_ele_grping(add_egrp_param)
!!      subroutine dealloc_add_z_ele_grping(add_egrp_param)
!
      module t_add_ele_grp_parameter
!
      use m_precision
      use t_file_IO_parameter
!
      implicit    none
!
      type add_ele_grp_param
!>        Structure for field data IO paramters
        type(field_IO_params) ::  original_mesh_file
!>        Structure for field data IO paramters
        type(field_IO_params) ::  modified_mesh_file
!
        integer (kind=kint) :: iflag_grping_direction = 0
!
        integer (kind=kint) :: num_r_ele_grp
        character (len=kchara), allocatable :: r_ele_grp_name(:)
        real(kind = kreal), allocatable :: minmax_r_ele_grping(:,:)
!
        integer (kind=kint) :: num_t_ele_grp
        character (len=kchara), allocatable :: t_ele_grp_name(:)
        real(kind = kreal), allocatable :: minmax_t_ele_grping(:,:)
!
        integer (kind=kint) :: num_s_ele_grp
        character (len=kchara), allocatable :: s_ele_grp_name(:)
        real(kind = kreal), allocatable :: minmax_s_ele_grping(:,:)
!
        integer (kind=kint) :: num_z_ele_grp
        character (len=kchara), allocatable :: z_ele_grp_name(:)
        real(kind = kreal), allocatable :: minmax_z_ele_grping(:,:)
      end type add_ele_grp_param
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_add_r_ele_grping(num, add_egrp_param)
!
      integer(kind = kint), intent(in) :: num
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
!
      add_egrp_param%num_r_ele_grp = num
      allocate(add_egrp_param%r_ele_grp_name(num))
      allocate(add_egrp_param%minmax_r_ele_grping(num,2))

      if(add_egrp_param%num_r_ele_grp .le. 0) return
      add_egrp_param%minmax_r_ele_grping = 0.0d0
!
      end subroutine alloc_add_r_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine alloc_add_t_ele_grping(num, add_egrp_param)
!
      integer(kind = kint), intent(in) :: num
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
!
      add_egrp_param%num_t_ele_grp = num
      allocate(add_egrp_param%t_ele_grp_name(num) )
      allocate(add_egrp_param%minmax_t_ele_grping(num,2))

      if(add_egrp_param%num_t_ele_grp .gt. 0) return
      add_egrp_param%minmax_t_ele_grping = 0.0d0
!
      end subroutine alloc_add_t_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine alloc_add_s_ele_grping(num, add_egrp_param)
!
      integer(kind = kint), intent(in) :: num
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
!
      add_egrp_param%num_s_ele_grp = num
      allocate(add_egrp_param%s_ele_grp_name(num))
      allocate(add_egrp_param%minmax_s_ele_grping(num,2))

      if(add_egrp_param%num_s_ele_grp .le. 0) return
      add_egrp_param%minmax_s_ele_grping = 0.0d0
!
      end subroutine alloc_add_s_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine alloc_add_z_ele_grping(num, add_egrp_param)
!
      integer(kind = kint), intent(in) :: num
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
!
      add_egrp_param%num_z_ele_grp = num
      allocate(add_egrp_param%z_ele_grp_name(num))
      allocate(add_egrp_param%minmax_z_ele_grping(num,2))
!
      if(add_egrp_param%num_z_ele_grp .le. 0) return
      add_egrp_param%minmax_z_ele_grping = 0.0d0
!
      end subroutine alloc_add_z_ele_grping
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_add_r_ele_grping(add_egrp_param)
!
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      deallocate(add_egrp_param%r_ele_grp_name)
      deallocate(add_egrp_param%minmax_r_ele_grping)
!
      end subroutine dealloc_add_r_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_add_t_ele_grping(add_egrp_param)
!
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      deallocate(add_egrp_param%t_ele_grp_name)
      deallocate(add_egrp_param%minmax_t_ele_grping)
!
      end subroutine dealloc_add_t_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_add_s_ele_grping(add_egrp_param)
!
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      deallocate(add_egrp_param%s_ele_grp_name)
      deallocate(add_egrp_param%minmax_s_ele_grping)
!
      end subroutine dealloc_add_s_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_add_z_ele_grping(add_egrp_param)
!
      type(add_ele_grp_param), intent(inout) :: add_egrp_param
!
      deallocate(add_egrp_param%z_ele_grp_name)
      deallocate(add_egrp_param%minmax_z_ele_grping)
!
      end subroutine dealloc_add_z_ele_grping
!
! -----------------------------------------------------------------------
!
      end module t_add_ele_grp_parameter
