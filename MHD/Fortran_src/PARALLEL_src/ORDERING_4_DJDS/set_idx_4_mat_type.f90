!>@file   set_idx_4_mat_type.f90
!!@brief  module set_idx_4_mat_type
!!
!!@author H. Matsui
!!@date Programmed in March, 2009
!!@date Modified in Nov., 2013
!
!>     Find index of matrix in FEM assemble loop using structure
!!
!!@verbatim
!!      subroutine set_whole_index_list_4_djds(node, ele,               &
!!     &          rhs_tbl, djds_tbl, whole_mat)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        type(table_mat_const), intent(inout) :: whole_mat
!!      subroutine set_DJDS_off_diag_type(NP, N, djds_tbl,              &
!!     &          nod1, nod2, mat_num)
!!        integer (kind = kint), intent(in) :: N, NP
!!        integer (kind = kint), intent(in) :: nod1, nod2
!!        type(DJDS_ordering_table), intent(in) :: djds_tbl
!!        integer (kind = kint), intent(inout) :: mat_num
!!@endverbatim
!
      module set_idx_4_mat_type
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_solver_djds
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_whole_index_list_4_djds(node, ele,                 &
     &          rhs_tbl, djds_tbl, whole_mat)
!
      use t_table_FEM_const
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      type(table_mat_const), intent(inout) :: whole_mat
!
      integer(kind = kint) :: mat_num, k2
      integer(kind = kint) :: iproc, iele, inum, iconn
      integer(kind = kint) :: inn, ist, ied, in
!
!
!$omp parallel private(k2)
      do k2 = 1, whole_mat%nnod_1ele
!
!$omp do private(iproc,inum,inn,ist,ied,in,iele,iconn,mat_num)
        do iproc = 1, np_smp
          do inum = 1, rhs_tbl%inod_ele_max
!
            inn = inum + rhs_tbl%inod_ele_max*(iproc-1)
            ist = rhs_tbl%nod_stack_smp(inn-1)+1
            ied = rhs_tbl%nod_stack_smp(inn)
!
            do in = ist, ied
              iele =  rhs_tbl%iele_sort_smp(in)
              iconn = rhs_tbl%iconn_sort_smp(in)
!
              call set_DJDS_off_diag_type                               &
     &           (node%numnod, node%internal_node, djds_tbl,            &
     &            ele%ie(iele,iconn), ele%ie(iele,k2), mat_num)
              whole_mat%idx_4_mat(in,k2) = mat_num
            end do
          end do
        end do
!$omp end do nowait
!
      end do
!$omp end parallel
!
      end subroutine set_whole_index_list_4_djds
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_DJDS_off_diag_type(NP, N, djds_tbl,                &
     &          nod1, nod2, mat_num)
!
      use t_geometry_data
      use set_DJDS_off_diagonal
!
      integer (kind = kint), intent(in) :: N, NP
      integer (kind = kint), intent(in) :: nod1, nod2
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal(N, NP, np_smp,                       &
     &     djds_tbl%NLmax, djds_tbl%NUmax,                              &
     &     djds_tbl%itotal_l, djds_tbl%itotal_u,                        &
     &     djds_tbl%npLX1, djds_tbl%npUX1, djds_tbl%NHYP,               &
     &     djds_tbl%STACKmc, djds_tbl%NLmaxHYP,                         &
     &     djds_tbl%NUmaxHYP, djds_tbl%OLDtoNEW,                        &
     &     djds_tbl%OLDtoNEW_DJDS_L, djds_tbl%OLDtoNEW_DJDS_U,          &
     &     djds_tbl%indexDJDS_L, djds_tbl%indexDJDS_U,                  &
     &     djds_tbl%itemDJDS_L, djds_tbl%itemDJDS_U,                    &
     &     djds_tbl%PEon, djds_tbl%COLORon, nod1, nod2, mat_num)
!
      end subroutine set_DJDS_off_diag_type
!
!-----------------------------------------------------------------------
!
      end module set_idx_4_mat_type
