!
!      module copy_matrix_2_djds_array
!
!      Written by H. Matsui
!
!      subroutine copy_matrix_2_djds_NN(N, NP)
!      subroutine s_set_DJDS_off_diag (N, NP, nod1, nod2, mat_num)
!
!      subroutine copy_RH_vect_2_crs_nn(NP)
!      subroutine copy_solution_2_crs_nn(NP)
!
      module copy_matrix_2_djds_array
!
      use m_precision
!
      use m_machine_parameter
      use m_crs_connect
      use m_crs_matrix
!
      implicit none
!
      private :: s_set_DJDS_off_diag
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_matrix_2_djds_NN(N, NP)
!
      use m_matrix_data_4_djds
!
      integer(kind = kint), intent(in) :: N, NP
!
      integer(kind = kint) :: inod1, inod2, mat_num, im, j1, j2, k
!
!
      do inod1 = 1, NP
        call s_set_DJDS_off_diag(N, NP, inod1, inod1, mat_num)
        do j1 = 1, NB_djds
          do j2 = 1, NB_djds
            im = NB_djds*NB_djds*(mat_num-1) + NB_djds*(j1-1) + j2
            aiccg(im) = D_crs(j1,j2,inod1)
          end do
        end do
      end do
!
      do inod1 = 1, NP
        do k = istack_crs_l(inod1-1)+1, istack_crs_l(inod1)
          inod2 = item_crs_l(k)
          call s_set_DJDS_off_diag(N, NP, inod1, inod2, mat_num)
          do j1 = 1, NB_djds
            do j2 = 1, NB_djds
              im = NB_djds*NB_djds*(mat_num-1) + NB_djds*(j1-1) + j2
              aiccg(im) = AL_crs(j1,j2,k)
            end do
          end do
        end do
      end do
!
      do inod1 = 1, NP
        do k = istack_crs_u(inod1-1)+1, istack_crs_u(inod1)
          inod2 = item_crs_u(k)
          call s_set_DJDS_off_diag(N, NP, inod1, inod2, mat_num)
          do j1 = 1, NB_djds
            do j2 = 1, NB_djds
              im = NB_djds*NB_djds*(mat_num-1) + NB_djds*(j1-1) + j2
              aiccg(im) = AU_crs(j1,j2,k)
            end do
          end do
        end do
      end do
!
      end subroutine copy_matrix_2_djds_NN
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine s_set_DJDS_off_diag(N, NP, nod1, nod2, mat_num)
!
      use m_solver_djds
      use set_DJDS_off_diagonal
!
      integer(kind = kint), intent(in) :: N, NP
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (N, NP, np_smp,                      &
     &    NLmax, NUmax, itotal_l, itotal_u,                             &
     &    npLX1, npUX1, NHYP, STACKmc, NLmaxHYP, NUmaxHYP,              &
     &    OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                   &
     &    indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,             &
     &    PEon, COLORon, nod1, nod2, mat_num)
!
      end subroutine s_set_DJDS_off_diag
!
!-----------------------------------------------------------------------
!
      subroutine copy_RH_vect_2_crs_nn(NP)
!
      use m_matrix_data_4_djds
!
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, NB_djds*NP
        b_djds(i) = B_crs(i)
        x_djds(i) = X_crs(i)
      end do
!$omp end parallel do
!
      end subroutine copy_RH_vect_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_crs_nn(NP)
!
      use m_matrix_data_4_djds
!
      integer(kind = kint), intent(in) :: NP
!
      integer(kind = kint) :: i
!
!$omp parallel do
      do i = 1, NB_djds*NP
        X_crs(i) = x_djds(i)
      end do
!$omp end parallel do
!
      end subroutine copy_solution_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      end module copy_matrix_2_djds_array
