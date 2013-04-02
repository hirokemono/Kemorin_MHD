!
!      module m_crs_matrix_4_filter
!
      module m_crs_matrix_4_filter
!
!     Written by H. Matsui on Aug., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: nmax_crs
      integer(kind = kint) :: imax_l, imax_u
!
      integer(kind = kint) :: n_crs
      integer(kind = kint) :: n_inter_crs
!
      integer(kind = kint) :: npl_crs, npu_crs
!
      integer(kind = kint), allocatable :: istack_l_crs(:)
      integer(kind = kint), allocatable :: istack_u_crs(:)
!
      integer(kind = kint), allocatable :: item_l_crs(:)
      integer(kind = kint), allocatable :: item_u_crs(:)
!
      real(kind = kreal), allocatable :: diag_mat(:)
      real(kind = kreal), allocatable :: al_mat(:)
      real(kind = kreal), allocatable :: au_mat(:)
!
!      subroutine allocate_array_4_crs_stack
!      subroutine allocate_array_4_crs_item
!      subroutine deallocate_array_4_crs_item
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_array_4_crs_stack
!
      allocate( istack_l_crs(0:nmax_crs) )
      allocate( istack_u_crs(0:nmax_crs) )
      istack_l_crs = 0
      istack_u_crs = 0
!
      end subroutine allocate_array_4_crs_stack
!
!-----------------------------------------------------------------------
!
      subroutine allocate_array_4_crs_item
!
      allocate( item_l_crs(imax_l) )
      allocate( item_u_crs(imax_u) )
      allocate( al_mat(imax_l) )
      allocate( au_mat(imax_u) )
      allocate( diag_mat(nmax_crs) )
      item_l_crs = 0
      item_u_crs = 0
!
      diag_mat = 0.0d0
      al_mat = 0.0d0
      au_mat = 0.0d0
!
      end subroutine allocate_array_4_crs_item
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_array_4_crs_item
!
      deallocate( istack_l_crs )
      deallocate( istack_u_crs )
!
      deallocate( item_l_crs )
      deallocate( item_u_crs )
      deallocate( al_mat )
      deallocate( au_mat )
      deallocate( diag_mat )
!
      end subroutine deallocate_array_4_crs_item
!
!-----------------------------------------------------------------------
!
      end module m_crs_matrix_4_filter

