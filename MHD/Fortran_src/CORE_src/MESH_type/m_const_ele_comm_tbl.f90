!
!      module m_const_ele_comm_tbl
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine alloc_ie_gl_import(ecomm_wk, nnod_4_ele, ntot_import)
!      subroutine dealloc_ie_gl_import(ecomm_wk)
!      subroutine alloc_const_ele_comm_tbl(nprocs)
!      subroutine dealloc_const_ele_comm_tbl(nprocs)
!
      module m_const_ele_comm_tbl
!
      use m_precision
!
      use t_comm_table
!
      implicit  none
!
!
      type work_4_const_export
        integer(kind = kint), allocatable :: ie_gl_import(:,:)
      end type work_4_const_export
!
      type(communication_table), pointer :: ele_comm_tmp(:)
      type(work_4_const_export), pointer :: ele_comm_work(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine alloc_ie_gl_import(ecomm_wk, nnod_4_ele, ntot_import)
!
      integer(kind = kint), intent(in) :: nnod_4_ele, ntot_import
      type(work_4_const_export), intent(inout) :: ecomm_wk
!
!
      allocate(ecomm_wk%ie_gl_import(nnod_4_ele,ntot_import))
      if(ntot_import .gt. 0) ecomm_wk%ie_gl_import = 0
!
      end subroutine alloc_ie_gl_import
!
!------------------------------------------------------------------
!
      subroutine dealloc_ie_gl_import(ecomm_wk)
!
      type(work_4_const_export), intent(inout) :: ecomm_wk
!
!
      deallocate(ecomm_wk%ie_gl_import)
!
      end subroutine dealloc_ie_gl_import
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine alloc_const_ele_comm_tbl(nprocs)
!
      integer(kind = kint), intent(in) :: nprocs
!
!
      allocate(ele_comm_work(nprocs))
      allocate(ele_comm_tmp(nprocs))
!
      end subroutine alloc_const_ele_comm_tbl
!
!------------------------------------------------------------------
!
      subroutine dealloc_const_ele_comm_tbl
!
!
      deallocate(ele_comm_work, ele_comm_tmp)
!
      end subroutine dealloc_const_ele_comm_tbl
!
!------------------------------------------------------------------
!
      end module m_const_ele_comm_tbl
