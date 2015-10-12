!
!     module matrix_initialization
!
!      subroutine iccg_matrix_init                                      &
!     &         (node, ele, rhs_tbl, mat_tbl, num_mat, aiccg)
!        type(node_data), intent(in) ::    node
!        type(element_data), intent(in) :: ele
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(table_mat_const), intent(in) ::        mat_tbl
!
!      subroutine s_crs_matrix_init(np_smp, nnod_4_ele,                 &
!     &          inod_ele_max, num_sort_smp, nod_stack_smp, idx_4_mat,  &
!     &          num_mat, diag_start, diag_end, aiccg)
!
      module matrix_initialization
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine iccg_matrix_init                                       &
     &         (node, ele, rhs_tbl, mat_tbl, num_mat, aiccg)
!
      use m_constants
      use m_machine_parameter
      use t_geometry_data
      use t_table_FEM_const
!
      type(node_data), intent(in) ::    node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) ::        mat_tbl
!
      integer(kind = kint), intent(in) :: num_mat
      real(kind = kreal), intent(inout) :: aiccg(0:num_mat)
!
!
      call s_crs_matrix_init(np_smp, ele%nnod_4_ele,                    &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, mat_tbl%idx_4_mat,                     &
     &    num_mat, ione, node%numnod, aiccg)
!
      end subroutine iccg_matrix_init
!
!-----------------------------------------------------------------------
!
      subroutine s_crs_matrix_init(np_smp, nnod_4_ele,                  &
     &          inod_ele_max, num_sort_smp, nod_stack_smp, idx_4_mat,   &
     &          num_mat, diag_start, diag_end, aiccg)
!
!
      integer(kind = kint), intent(in) :: np_smp, nnod_4_ele
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &                  :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in)                                  &
     &                  :: idx_4_mat(num_sort_smp,nnod_4_ele)
      integer(kind = kint), intent(in) :: num_mat
      integer(kind = kint), intent(in) :: diag_start, diag_end
!
      real(kind = kreal), intent(inout) :: aiccg(0:num_mat)
!
      integer(kind = kint) :: k2, iproc, inum, inn
      integer(kind = kint) :: istart, iend, in
      integer(kind = kint) :: mat_num
!
!
!$omp workshare
      aiccg(diag_start:diag_end) = 1.0d0
!$omp end workshare
!
!$omp parallel do private(k2,inum,istart,iend,inn,in,mat_num)
      do iproc = 1, np_smp
        do k2 = 1, nnod_4_ele
!
          do inum = 1, inod_ele_max
            inn = inum + inod_ele_max*(iproc-1)
            istart = nod_stack_smp(inn-1)+1
            iend = nod_stack_smp(inn)
!
            do in = istart, iend
              mat_num = idx_4_mat(in,k2)
              aiccg(mat_num) = 0.0d0
            end do
          end do
        end do
!
      end do
!$omp end parallel do
!
      end subroutine s_crs_matrix_init
!
!-----------------------------------------------------------------------
!
      end module matrix_initialization
