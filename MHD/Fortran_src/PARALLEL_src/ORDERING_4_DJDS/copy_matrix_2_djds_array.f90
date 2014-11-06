!
!      module copy_matrix_2_djds_array
!
!      Written by H. Matsui
!
!      subroutine copy_matrix_2_djds_NN
!      subroutine s_set_DJDS_off_diag (nod1, nod2, mat_num)
!
      module copy_matrix_2_djds_array
!
      use m_precision
!
      use m_machine_parameter
      use m_geometry_parameter
      use m_crs_connect
      use m_crs_matrix
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_matrix_2_djds_NN
!
      use m_matrix_data_4_djds
!
      integer(kind = kint) :: inod1, inod2, mat_num, im, j1, j2, k
!
!
      do inod1 = 1, numnod
        call s_set_DJDS_off_diag(inod1, inod1, mat_num)
        do j1 = 1, NB_djds
          do j2 = 1, NB_djds
            im = NB_djds*NB_djds*(mat_num-1) + NB_djds*(j1-1) + j2
            aiccg(im) = D_crs(j1,j2,inod1)
          end do
        end do
      end do
!
      do inod1 = 1, numnod
        do k = istack_crs_l(inod1-1)+1, istack_crs_l(inod1)
          inod2 = item_crs_l(k)
          call s_set_DJDS_off_diag(inod1, inod2, mat_num)
          do j1 = 1, NB_djds
            do j2 = 1, NB_djds
              im = NB_djds*NB_djds*(mat_num-1) + NB_djds*(j1-1) + j2
              aiccg(im) = AL_crs(j1,j2,k)
            end do
          end do
        end do
      end do
!
      do inod1 = 1, numnod
        do k = istack_crs_u(inod1-1)+1, istack_crs_u(inod1)
          inod2 = item_crs_u(k)
          call s_set_DJDS_off_diag(inod1, inod2, mat_num)
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
      subroutine s_set_DJDS_off_diag (nod1, nod2, mat_num)
!
      use m_solver_djds
      use set_DJDS_off_diagonal
!
      integer (kind = kint), intent(in) :: nod1, nod2
      integer (kind = kint), intent(inout) :: mat_num
!
!
      call s_set_DJDS_off_diagonal (internal_node, numnod, np_smp,      &
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
      subroutine copy_RH_vect_2_crs_nn
!
      use m_matrix_data_4_djds
!
      integer(kind = kint) :: i
!
      do i = 1, NB_djds*numnod
        b_djds(i) = B_crs(i)
        x_djds(i) = X_crs(i)
      end do
!
      end subroutine copy_RH_vect_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      subroutine copy_solution_2_crs_nn
!
      use m_matrix_data_4_djds
!
      integer(kind = kint) :: i
!
      do i = 1, NB_djds*numnod
        X_crs(i) = x_djds(i)
      end do
!
      end subroutine copy_solution_2_crs_nn
!
!  ---------------------------------------------------------------------
!
      end module copy_matrix_2_djds_array
