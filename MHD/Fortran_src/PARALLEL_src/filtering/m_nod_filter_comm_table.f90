!m_nod_filter_comm_table.f90
!      module m_nod_filter_comm_table
!
!     Written by H. Matsui on Apr., 2008
!
!
!      subroutine allocate_neib_filter_id
!
!      subroutine allocate_filter_import_num
!      subroutine allocate_filter_export_num
!      subroutine allocate_filter_import_item
!      subroutine allocate_filter_export_item
!
!      subroutine deallocate_neib_filter_id
!
!      subroutine deallocate_filter_import_item
!      subroutine deallocate_filter_export_item
!
!      subroutine allocate_globalnod_filter
!      subroutine deallocate_globalnod_filter
!
!      subroutine allocate_nod_data_4_filter
!      subroutine deallocate_nod_data_4_filter
!
!      subroutine allocate_int_data_4_filter
!      subroutine deallocate_int_data_4_filter
!
      module m_nod_filter_comm_table
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint) :: inter_nod_3dfilter
      integer(kind = kint) :: nnod_filtering
      integer(kind = kint), allocatable :: id_globalnod_filtering(:)
      real(kind = kreal), allocatable :: xx_filtering(:,:)
!
      real(kind = kreal), allocatable :: x_vec_filtering(:)
      integer(kind = kint), allocatable :: ix_vec_filtering(:)
!
      integer(kind = kint) :: num_neib_filter
!     number of neighboring pe
      integer(kind = kint), allocatable :: id_neib_filter(:)
!     neighboring pe id
!
!
      integer(kind = kint) :: ntot_import_filter
!    total number of import data 
      integer(kind = kint), allocatable :: num_import_filter(:)
      integer(kind = kint), allocatable :: istack_import_filter(:)
!     import data count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_import_filter(:)
!     id for import data                     (i-th)
!
      integer(kind = kint) :: ntot_export_filter
!    total number of export data 
      integer(kind = kint), allocatable :: num_export_filter(:)
      integer(kind = kint), allocatable :: istack_export_filter(:)
!     export data count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_export_filter(:)
!     id for export data                     (i-th)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_filter_id
!
      allocate(id_neib_filter(num_neib_filter))
      id_neib_filter = -1
!
      end subroutine allocate_neib_filter_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_filter_import_num
!
      allocate(num_import_filter(num_neib_filter))
      allocate(istack_import_filter(0:num_neib_filter))
!
      if (num_neib_filter .gt. 0) num_import_filter = 0
      istack_import_filter = 0
!
      end subroutine allocate_filter_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_filter_export_num
!
      allocate(num_export_filter(num_neib_filter))
      allocate(istack_export_filter(0:num_neib_filter))
!
      if (num_neib_filter .gt. 0) num_export_filter = 0
      istack_export_filter = 0
!
      end subroutine allocate_filter_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_filter_import_item
!
      allocate(item_import_filter(ntot_import_filter))
      if (ntot_import_filter .gt. 0) item_import_filter = 0
!
      end subroutine allocate_filter_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_filter_export_item
!
      allocate(item_export_filter(ntot_export_filter))
      if (num_neib_filter .gt. 0) item_export_filter = 0
!
      end subroutine allocate_filter_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_filter_id
!
      deallocate(id_neib_filter)
!
      end subroutine deallocate_neib_filter_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_filter_import_item
!
      deallocate(num_import_filter)
      deallocate(istack_import_filter)
      deallocate(item_import_filter)
!
      end subroutine deallocate_filter_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_filter_export_item
!
      deallocate(num_export_filter)
      deallocate(istack_export_filter)
      deallocate(item_export_filter)
!
      end subroutine deallocate_filter_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_globalnod_filter
!
      allocate( id_globalnod_filtering(nnod_filtering) )
      allocate( xx_filtering(nnod_filtering,3) )
      id_globalnod_filtering = 0
      xx_filtering = 0.0d0
!
      end subroutine allocate_globalnod_filter
!
!------------------------------------------------------------------
!
      subroutine deallocate_globalnod_filter
!
      deallocate( id_globalnod_filtering )
      deallocate( xx_filtering )
!
      end subroutine deallocate_globalnod_filter
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_nod_data_4_filter
!
      allocate( x_vec_filtering(6*nnod_filtering) )
      x_vec_filtering = 0.0d0
!
      end subroutine allocate_nod_data_4_filter
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_data_4_filter
!
      deallocate( x_vec_filtering )
!
      end subroutine deallocate_nod_data_4_filter
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_int_data_4_filter
!
      allocate( ix_vec_filtering(nnod_filtering) )
      ix_vec_filtering = 0
!
      end subroutine allocate_int_data_4_filter
!
!------------------------------------------------------------------
!
      subroutine deallocate_int_data_4_filter
!
      deallocate( ix_vec_filtering )
!
      end subroutine deallocate_int_data_4_filter
!
!------------------------------------------------------------------
!
      end module m_nod_filter_comm_table
