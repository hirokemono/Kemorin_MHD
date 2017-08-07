!
!      module t_work_4_MHD_layering
!
!      Written by H. Matsui on Feb., 2008
!
!!      subroutine alloc_lists_4_layer(numele, WK_layer)
!!      subroutine alloc_mat_node_flag(numnod, WK_layer_n)
!!      subroutine dealloc_lists_4_layer(WK_layer)
!!      subroutine dealloc_mat_node_flag(WK_layer_n)
!!        type(work_4_make_layering), intent(inout) :: WK_layer
!!        type(nod_work_4_make_layering), intent(inout) :: WK_layer_n
!
      module t_work_4_MHD_layering
!
      use m_precision
      use m_machine_parameter
!
      implicit  none
!
!
      type work_4_make_layering
        integer(kind=kint), allocatable :: mat_flag_mhd(:)
! 
!>        element index for new ordering
        integer(kind=kint), allocatable  :: old2newele_layer(:)
!>        element index for new ordering
        integer(kind=kint), allocatable  :: new2oldele_layer(:)
      end type work_4_make_layering
!
!
      type nod_work_4_make_layering
        integer (kind=kint), allocatable :: mat_node_flag(:)
      end type nod_work_4_make_layering
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_lists_4_layer(numele, WK_layer)
!
      integer(kind = kint), intent(in) :: numele
      type(work_4_make_layering), intent(inout) :: WK_layer
!
!
      allocate( WK_layer%new2oldele_layer(numele) )
      allocate( WK_layer%old2newele_layer(numele) )
      allocate( WK_layer%mat_flag_mhd(numele) )
!
      WK_layer%new2oldele_layer = 0
      WK_layer%old2newele_layer = 0
      WK_layer%mat_flag_mhd = -10
!
       end subroutine alloc_lists_4_layer
!
! ----------------------------------------------------------------------
!
      subroutine alloc_mat_node_flag(numnod, WK_layer_n)
!
      integer(kind = kint), intent(in) :: numnod
      type(nod_work_4_make_layering), intent(inout) :: WK_layer_n
!
      allocate(WK_layer_n%mat_node_flag(numnod))
      if(numnod .gt. 0) WK_layer_n%mat_node_flag = 0
!
      end subroutine alloc_mat_node_flag
!
! ---------------------------------------------------------------------
! ---------------------------------------------------------------------
!
      subroutine dealloc_lists_4_layer(WK_layer)
!
      type(work_4_make_layering), intent(inout) :: WK_layer
!
!
      deallocate( WK_layer%new2oldele_layer )
      deallocate( WK_layer%old2newele_layer )
      deallocate( WK_layer%mat_flag_mhd )
!
      end subroutine dealloc_lists_4_layer
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_mat_node_flag(WK_layer_n)
!
      type(nod_work_4_make_layering), intent(inout) :: WK_layer_n
!
      deallocate(WK_layer_n%mat_node_flag)
!
      end subroutine dealloc_mat_node_flag
!
! ---------------------------------------------------------------------
!
      end module t_work_4_MHD_layering
