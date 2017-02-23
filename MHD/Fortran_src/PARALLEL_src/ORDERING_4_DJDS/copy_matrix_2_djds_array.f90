!
!      module copy_matrix_2_djds_array
!
!      Written by H. Matsui
!
!!      subroutine transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs,  &
!!     &          mat_crs, CG_param, DJDS_param, djds_tbl, djds_mat)
!      subroutine copy_matrix_2_djds_NN(tbl_crs, mat_crs, djds_tbl,     &
!     &          NP, N, NB, num_mat_comp, aiccg)
!
!      subroutine copy_RH_vect_2_crs_nn((mat1_crs, NP, NB, B, X)
!      subroutine copy_solution_2_crs_nn(NP, NB, X, mat_crs)
!
      module copy_matrix_2_djds_array
!
      use m_precision
!
      use m_machine_parameter
      use t_iccg_parameter
      use t_crs_matrix
!
      implicit none
!
      character(len=kchara) :: SOLVER_TYPE_djds
!
      private :: copy_paramters_4_djds, copy_matrix_2_djds_NN
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_paramters_4_djds                                  &
     &         (tbl_crs, mat_crs, CG_param, djds_tbl)
!
       use t_solver_djds
       use t_crs_matrix
!
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mat_crs
      type(CG_poarameter), intent(inout) :: CG_param
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
!
!
      call copy_to_iccg_parameter(CG_param, mat_crs)
      SOLVER_TYPE_djds = mat_crs%SOLVER_crs
!
      djds_tbl%itotal_l = tbl_crs%ntot_l
      djds_tbl%itotal_u = tbl_crs%ntot_u
!
      end subroutine copy_paramters_4_djds
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine transfer_crs_2_djds_matrix(node, nod_comm, tbl_crs,    &
     &          mat_crs, CG_param, DJDS_param, djds_tbl, djds_mat)
!
      use calypso_mpi
      use t_geometry_data
      use t_iccg_parameter
      use t_solver_djds
      use t_vector_for_solver
!
      use set_size_4_smp_types
      use reordering_djds_smp_type
      use DJDS_new_comm_table
!
      type(node_data), intent(inout) :: node
      type(communication_table), intent(in) :: nod_comm
      type(CRS_matrix), intent(in) :: mat_crs
      type(DJDS_poarameter), intent(in) :: DJDS_param
!
      type(CG_poarameter), intent(inout) :: CG_param
      type(CRS_matrix_connect), intent(inout) :: tbl_crs
      type(DJDS_ordering_table), intent(inout) :: djds_tbl
      type(DJDS_MATRIX), intent(inout) :: djds_mat
!
      type(mpi_4_solver) :: solver_C
!
!
      call copy_communicator_4_solver(solver_C)
      call copy_paramters_4_djds(tbl_crs, mat_crs, CG_param, djds_tbl)
!
      call count_node_4_smp_mesh_type(node)
!
!C +-----------------+
!C | DJDS reordering |
!C +-----------------+
!C===
!C
       if (iflag_debug.eq.1) write(*,*) 's_reordering_djds_smp'
      call s_reordering_djds_smp(np_smp, node%numnod,                   &
     &    node%internal_node, node%istack_internal_smp,                 &
     &    solver_C, tbl_crs, DJDS_param, djds_tbl)
!C
!C +--------------------------------------+
!C | set new communication table 4 solver |
!C +--------------------------------------+
!C===
!C
      call set_new_comm_table_type(node%numnod, nod_comm, djds_tbl)
!
!C +-------------+
!C | copy matrix |
!C +-------------+
!
      djds_mat%NB = mat_crs%NB_crs
      call alloc_type_djdsNN_mat(node%numnod, node%internal_node,       &
     &   djds_mat%NB, djds_tbl, djds_mat)
!
       if (iflag_debug.eq.1) write(*,*) 'copy_matrix_2_djds_NN'
      call copy_matrix_2_djds_NN(tbl_crs, mat_crs, djds_tbl,            &
     &    node%numnod, node%internal_node, djds_mat%NB,                 &
     &    djds_mat%num_non0, djds_mat%aiccg)
!
      call dealloc_crs_connect(tbl_crs)
!
      end subroutine transfer_crs_2_djds_matrix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_matrix_2_djds_NN(tbl_crs, mat_crs, djds_tbl,      &
     &          NP, N, NB, num_mat_comp, aiccg)
!
      use t_solver_djds
      use set_idx_4_mat_type
!
      integer(kind = kint), intent(in) :: N, NP, NB
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mat_crs
      type(DJDS_ordering_table), intent(in) :: djds_tbl
!
      integer(kind = kint), intent(in) :: num_mat_comp
      real(kind=kreal), intent(inout) :: aiccg(1-NB*NB:num_mat_comp)
!
      integer(kind = kint) :: inod1, inod2, mat_num, im, j1, j2, k
      integer(kind = kint) :: ist, ied, j12
!
!
      do inod1 = 1, NP
        call set_DJDS_off_diag_type                                     &
     &     (NP, N, djds_tbl, inod1, inod1, mat_num)
        do j1 = 1, NB
          do j2 = 1, NB
            j12 = j1 + (j2-1)*NB + (inod1-1)*NB*NB
            im = NB*NB*(mat_num-1) + NB*(j1-1) + j2
            aiccg(im) = mat_crs%D_crs(j12)
          end do
        end do
      end do
!
      do inod1 = 1, NP
        ist = tbl_crs%istack_l(inod1-1)+1
        ied = tbl_crs%istack_l(inod1)
        do k = ist, ied
          inod2 = tbl_crs%item_l(k)
          call set_DJDS_off_diag_type                                   &
     &       (NP, N, djds_tbl, inod1, inod2, mat_num)
          do j1 = 1, NB
            do j2 = 1, NB
              j12 = j1 + (j2-1)*NB + (k-1)*NB*NB
              im = NB*NB*(mat_num-1) + NB*(j1-1) + j2
              aiccg(im) = mat_crs%AL_crs(j12)
            end do
          end do
        end do
      end do
!
      do inod1 = 1, NP
        ist = tbl_crs%istack_u(inod1-1)+1
        ied = tbl_crs%istack_u(inod1)
        do k = ist, ied
          inod2 = tbl_crs%item_u(k)
          call set_DJDS_off_diag_type                                   &
     &       (NP, N, djds_tbl, inod1, inod2, mat_num)
          do j1 = 1, NB
            do j2 = 1, NB
              j12 = j1 + (j2-1)*NB + (k-1)*NB*NB
              im = NB*NB*(mat_num-1) + NB*(j1-1) + j2
              aiccg(im) = mat_crs%AU_crs(j12)
            end do
          end do
        end do
      end do
!
      end subroutine copy_matrix_2_djds_NN
!
!  ---------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_RH_vect_2_crs_nn(mat_crs, NP, NB, B, X)
!
      type(CRS_matrix), intent(in) :: mat_crs
      integer(kind = kint), intent(in) :: NB, NP
      real(kind=kreal), intent(inout) :: X(NB*NP), B(NB*NP)
!
!
!$omp parallel workshare
      B(1:NB*NP) = mat_crs%B_crs(1:NB*NP)
      X(1:NB*NP) = mat_crs%X_crs(1:NB*NP)
!$omp end parallel workshare
!
      end subroutine copy_RH_vect_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_crs_nn(NP, NB, X, mat_crs)
!
      integer(kind = kint), intent(in) :: NB, NP
      real(kind=kreal), intent(in) :: X(NB*NP)
      type(CRS_matrix), intent(inout) :: mat_crs
!
!
!$omp parallel workshare
      mat_crs%X_crs(1:NB*NP) = X(1:NB*NP)
!$omp end parallel workshare
!
      end subroutine copy_solution_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      end module copy_matrix_2_djds_array
