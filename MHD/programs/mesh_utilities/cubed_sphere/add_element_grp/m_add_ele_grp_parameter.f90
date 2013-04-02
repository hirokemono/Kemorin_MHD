!m_add_ele_grp_parameter.f90
!      module m_add_ele_grp_parameter
!
      module m_add_ele_grp_parameter
!
!      Written by H. Matsui on Oct., 2007
!
      use m_precision
!
      implicit    none
!
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
!
      character(len=kchara) :: layerd_mesh_head
!
      character(len=kchara) :: original_mesh_head
      character(len=kchara) :: modified_mesh_head
!
!      subroutine allocate_add_r_ele_grping
!      subroutine allocate_add_t_ele_grping
!      subroutine allocate_add_s_ele_grping
!      subroutine allocate_add_z_ele_grping
!
!      subroutine deallocate_add_r_ele_grping
!      subroutine deallocate_add_t_ele_grping
!      subroutine deallocate_add_s_ele_grping
!      subroutine deallocate_add_z_ele_grping
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_add_r_ele_grping
!
      allocate( r_ele_grp_name(num_r_ele_grp) )
      allocate( minmax_r_ele_grping(num_r_ele_grp,2) )
      if(num_r_ele_grp .gt. 0) minmax_r_ele_grping = 0.0d0
!
      end subroutine allocate_add_r_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine allocate_add_t_ele_grping
!
      allocate( t_ele_grp_name(num_t_ele_grp) )
      allocate( minmax_t_ele_grping(num_t_ele_grp,2) )
      if(num_t_ele_grp .gt. 0) minmax_t_ele_grping = 0.0d0
!
      end subroutine allocate_add_t_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine allocate_add_s_ele_grping
!
      allocate( s_ele_grp_name(num_s_ele_grp) )
      allocate( minmax_s_ele_grping(num_s_ele_grp,2) )
      if(num_s_ele_grp .gt. 0) minmax_s_ele_grping = 0.0d0
!
      end subroutine allocate_add_s_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine allocate_add_z_ele_grping
!
      allocate( z_ele_grp_name(num_z_ele_grp) )
      allocate( minmax_z_ele_grping(num_z_ele_grp,2) )
      if(num_z_ele_grp .gt. 0) minmax_z_ele_grping = 0.0d0
!
      end subroutine allocate_add_z_ele_grping
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_add_r_ele_grping
!
      deallocate( r_ele_grp_name, minmax_r_ele_grping )
!
      end subroutine deallocate_add_r_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_add_t_ele_grping
!
      deallocate( t_ele_grp_name, minmax_t_ele_grping )
!
      end subroutine deallocate_add_t_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_add_s_ele_grping
!
      deallocate( s_ele_grp_name, minmax_s_ele_grping )
!
      end subroutine deallocate_add_s_ele_grping
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_add_z_ele_grping
!
      deallocate( z_ele_grp_name, minmax_z_ele_grping )
!
      end subroutine deallocate_add_z_ele_grping
!
! -----------------------------------------------------------------------
!
      end module m_add_ele_grp_parameter
