!
!     module   m_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
!       subroutine allocate_marix_list_fl
!       subroutine allocate_marix_list_cd
!       subroutine allocate_marix_list_ins
!
!       subroutine deallocate_marix_list_fl
!       subroutine deallocate_marix_list_cd
!       subroutine deallocate_marix_list_ins
!
!       subroutine deallocate_marix_list_fl_l
!       subroutine deallocate_marix_list_cd_l
!       subroutine deallocate_marix_list_ins_l
!
      module   m_sorted_node_MHD

      use m_precision
!
      implicit  none
!
      integer (kind=kint), allocatable :: idx_4_fl_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_cd_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_ins_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_cd_mat_full(:,:)
!
      integer (kind=kint), allocatable :: idx_4_fll_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_cdl_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_insl_mat(:,:)
      integer (kind=kint), allocatable :: idx_4_cdl_mat_full(:,:)
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine allocate_marix_list_fl
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_sorted_node
!
!
      allocate (idx_4_fl_mat (num_sort_smp,nnod_4_ele))
      allocate (idx_4_fll_mat (num_sort_smp,num_t_linear))
!
      idx_4_fl_mat = 0
      idx_4_fll_mat = 0
!
      end subroutine allocate_marix_list_fl
!
! ----------------------------------------------
!
      subroutine allocate_marix_list_cd
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_sorted_node
!
      allocate (idx_4_cd_mat (num_sort_smp,nnod_4_ele)) 
      allocate (idx_4_cd_mat_full (num_sort_smp,nnod_4_ele))
      allocate (idx_4_cdl_mat (num_sort_smp,num_t_linear)) 
      allocate (idx_4_cdl_mat_full (num_sort_smp,num_t_linear))
!
      idx_4_cd_mat      = 0
      idx_4_cd_mat_full = 0
      idx_4_cdl_mat      = 0
      idx_4_cdl_mat_full = 0
!
      end subroutine allocate_marix_list_cd
!
! ----------------------------------------------
!
      subroutine allocate_marix_list_ins
!
      use m_geometry_constants
      use m_geometry_parameter
      use m_sorted_node
!
      allocate (idx_4_ins_mat (num_sort_smp,nnod_4_ele))
      allocate (idx_4_insl_mat (num_sort_smp,num_t_linear))
      idx_4_ins_mat = 0
      idx_4_insl_mat = 0
!
      end subroutine allocate_marix_list_ins
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       subroutine deallocate_marix_list_fl
!
       deallocate (idx_4_fl_mat)
!
       end subroutine deallocate_marix_list_fl
!
! ----------------------------------------------
!
       subroutine deallocate_marix_list_cd
!
       deallocate (idx_4_cd_mat)
       deallocate (idx_4_cd_mat_full)
!
       end subroutine deallocate_marix_list_cd
!
! ----------------------------------------------
!
       subroutine deallocate_marix_list_ins
!
       deallocate (idx_4_ins_mat)
!
       end subroutine deallocate_marix_list_ins
!
! ----------------------------------------------
! ----------------------------------------------
!
       subroutine deallocate_marix_list_fl_l
!
       deallocate (idx_4_fll_mat)
!
       end subroutine deallocate_marix_list_fl_l
!
! ----------------------------------------------
!
       subroutine deallocate_marix_list_cd_l
!
       deallocate (idx_4_cdl_mat)
       deallocate (idx_4_cdl_mat_full)
!
       end subroutine deallocate_marix_list_cd_l
!
! ----------------------------------------------
!
       subroutine deallocate_marix_list_ins_l
!
       deallocate (idx_4_insl_mat)
!
       end subroutine deallocate_marix_list_ins_l
!
! ----------------------------------------------
!-----------------------------------------------------------------------
!
      end module m_sorted_node_MHD
