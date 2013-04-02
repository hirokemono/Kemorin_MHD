!m_nod_w_filter_comm_table.f90
!      module m_nod_w_filter_comm_table
!
!     Written by H. Matsui on Nov., 2008
!
!      subroutine allocate_neib_w_fil_id
!
!      subroutine allocate_w_fil_import_num
!      subroutine allocate_w_fil_export_num
!      subroutine allocate_w_fil_import_item
!      subroutine allocate_w_fil_export_item
!
!      subroutine deallocate_neib_w_fil_id
!
!      subroutine deallocate_w_fil_import_item
!      subroutine deallocate_w_fil_export_item
!
!      subroutine allocate_globalnod_w_fil
!      subroutine deallocate_globalnod_w_fil
!
!      subroutine allocate_nod_data_w_fil
!      subroutine deallocate_nod_data_w_fil
!
!      subroutine allocate_int_data_w_fil
!      subroutine deallocate_int_data_w_fil
!
      module m_nod_w_filter_comm_table
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint) :: inter_nod_w_filter
      integer(kind = kint) :: nnod_w_filtering
      integer(kind = kint), allocatable :: id_globalnod_w_fil(:)
      real(kind = kreal), allocatable :: xx_w_filter(:,:)
!
      real(kind = kreal), allocatable :: x_vec_w_fil(:)
      integer(kind = kint), allocatable :: ix_vec_w_fil(:)
!
      integer(kind = kint) :: num_neib_w_fil
!     number of neighboring pe
      integer(kind = kint), allocatable :: id_neib_w_fil(:)
!     neighboring pe id
!
!
      integer(kind = kint) :: ntot_import_w_fil
!    total number of import data 
      integer(kind = kint), allocatable :: num_import_w_fil(:)
      integer(kind = kint), allocatable :: istack_import_w_fil(:)
!     import data count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_import_w_fil(:)
!     id for import data                     (i-th)
!
      integer(kind = kint) :: ntot_export_w_fil
!    total number of export data 
      integer(kind = kint), allocatable :: num_export_w_fil(:)
      integer(kind = kint), allocatable :: istack_export_w_fil(:)
!     export data count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable :: item_export_w_fil(:)
!     id for export data                     (i-th)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_w_fil_id
!
      allocate(id_neib_w_fil(num_neib_w_fil))
      id_neib_w_fil = -1
!
      end subroutine allocate_neib_w_fil_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_w_fil_import_num
!
      allocate(num_import_w_fil(num_neib_w_fil))
      allocate(istack_import_w_fil(0:num_neib_w_fil))
!
      if (num_neib_w_fil .gt. 0) num_import_w_fil = 0
      istack_import_w_fil = 0
!
      end subroutine allocate_w_fil_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_w_fil_export_num
!
      allocate(num_export_w_fil(num_neib_w_fil))
      allocate(istack_export_w_fil(0:num_neib_w_fil))
!
      if (num_neib_w_fil .gt. 0) num_export_w_fil = 0
      istack_export_w_fil = 0
!
      end subroutine allocate_w_fil_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_w_fil_import_item
!
      allocate(item_import_w_fil(ntot_import_w_fil))
      if (ntot_import_w_fil .gt. 0) item_import_w_fil = 0
!
      end subroutine allocate_w_fil_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_w_fil_export_item
!
      allocate(item_export_w_fil(ntot_export_w_fil))
      if (num_neib_w_fil .gt. 0) item_export_w_fil = 0
!
      end subroutine allocate_w_fil_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_w_fil_id
!
      deallocate(id_neib_w_fil)
!
      end subroutine deallocate_neib_w_fil_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_w_fil_import_item
!
      deallocate(num_import_w_fil)
      deallocate(istack_import_w_fil)
      deallocate(item_import_w_fil)
!
      end subroutine deallocate_w_fil_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_w_fil_export_item
!
      deallocate(num_export_w_fil)
      deallocate(istack_export_w_fil)
      deallocate(item_export_w_fil)
!
      end subroutine deallocate_w_fil_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_globalnod_w_fil
!
      allocate( id_globalnod_w_fil(nnod_w_filtering) )
      allocate( xx_w_filter(nnod_w_filtering,3) )
      id_globalnod_w_fil = 0
      xx_w_filter = 0.0d0
!
      end subroutine allocate_globalnod_w_fil
!
!------------------------------------------------------------------
!
      subroutine deallocate_globalnod_w_fil
!
      deallocate( id_globalnod_w_fil )
      deallocate( xx_w_filter )
!
      end subroutine deallocate_globalnod_w_fil
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_nod_data_w_fil
!
      allocate( x_vec_w_fil(6*nnod_w_filtering) )
      x_vec_w_fil = 0.0d0
!
      end subroutine allocate_nod_data_w_fil
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_data_w_fil
!
      deallocate( x_vec_w_fil )
!
      end subroutine deallocate_nod_data_w_fil
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_int_data_w_fil
!
      allocate( ix_vec_w_fil(nnod_w_filtering) )
      ix_vec_w_fil = 0
!
      end subroutine allocate_int_data_w_fil
!
!------------------------------------------------------------------
!
      subroutine deallocate_int_data_w_fil
!
      deallocate( ix_vec_w_fil )
!
      end subroutine deallocate_int_data_w_fil
!
!------------------------------------------------------------------
!
      end module m_nod_w_filter_comm_table
