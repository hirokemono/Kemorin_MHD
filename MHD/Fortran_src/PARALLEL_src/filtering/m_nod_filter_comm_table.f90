!m_nod_filter_comm_table.f90
!      module m_nod_filter_comm_table
!
!     Written by H. Matsui on Apr., 2008
!
!
!!      subroutine copy_node_data_to_filter(node)
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
      use t_comm_table
!
      implicit  none
!
!
      integer(kind = kint) :: inter_nod_3dfilter
      integer(kind = kint) :: nnod_filtering
      integer(kind = kint_gl), allocatable :: id_globalnod_filtering(:)
      real(kind = kreal), allocatable :: xx_filtering(:,:)
!
      real(kind = kreal), allocatable :: x_vec_filtering(:)
      integer(kind = kint), allocatable :: ix_vec_filtering(:)
!
!> data structure for filter communication table
      type(communication_table), save :: flt_comm
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_node_data_to_filter(node)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
!
!
      nnod_filtering =     node%numnod
      inter_nod_3dfilter = node%internal_node
      call allocate_globalnod_filter
!
!$omp parallel workshare
      id_globalnod_filtering(1:nnod_filtering)                          &
     &                      = node%inod_global(1:nnod_filtering)
      xx_filtering(1:nnod_filtering,1) = node%xx(1:nnod_filtering,1)
      xx_filtering(1:nnod_filtering,2) = node%xx(1:nnod_filtering,2)
      xx_filtering(1:nnod_filtering,3) = node%xx(1:nnod_filtering,3)
!$omp end parallel workshare
!
      end subroutine copy_node_data_to_filter
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
