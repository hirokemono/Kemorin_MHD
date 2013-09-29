!
!      module fem_const_filter_matrix
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine allocate_sk_filter
!      subroutine allocate_mat_num_weight
!      subroutine deallocate_sk_filter
!      subroutine deallocate_mat_num_weight
!
!      subroutine set_idx_list_4_filter_mat(nele_grp, iele_grp,         &
!     &          nnod_mat_tbl, inod_mat_tbl)
!      subroutine fem_sk_filter_moments(nele_grp, iele_grp,             &
!     &          inod, ix, k_order)
!      subroutine sum_sk_2_filter_mat(nele_grp, k_order)
!
      module fem_const_filter_matrix
!
      use m_precision
!
      use calypso_mpi
!
      implicit none
!
      integer(kind = kint) :: nmax_num_ele_1nod
      integer(kind = kint), allocatable :: mat_num_filter(:,:)
      integer(kind = kint), allocatable :: mat_num_weight(:,:)
      real(kind = kreal), allocatable :: sk_filter(:,:)
      real(kind = kreal), allocatable :: xx_int(:,:)
!
      real(kind = kreal) :: rms_weight
      real(kind = kreal) :: min_rms_weight
      real(kind = kreal) :: max_det_mat
      integer(kind = kint) :: iflag_final
!
      private :: mat_num_filter, sk_filter
      private :: mat_num_weight

!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_sk_filter
!
      use m_geometry_parameter
      use m_filter_coefs
      use m_matrix_4_filter
!
      allocate( mat_num_filter(nmax_num_ele_1nod,nnod_4_ele) )
      allocate( mat_num_weight(nmax_num_ele_1nod,nnod_4_ele) )
      allocate( sk_filter(nmax_num_ele_1nod,nnod_4_ele) )
      mat_num_filter = 0
      mat_num_weight = 0
      sk_filter = 0.0d0
!
      end subroutine allocate_sk_filter
!
!-----------------------------------------------------------------------
!
      subroutine allocate_mat_num_weight
!
      use m_geometry_parameter
!
      allocate( xx_int(numnod,3) )
      xx_int =      0.0d0
!
      end subroutine allocate_mat_num_weight
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_sk_filter
!
      deallocate( mat_num_weight )
      deallocate( mat_num_filter )
      deallocate( sk_filter )
!
      end subroutine deallocate_sk_filter
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_mat_num_weight
!
      deallocate( xx_int )
!
      end subroutine deallocate_mat_num_weight
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_idx_list_4_filter_mat(nele_grp, iele_grp,          &
     &          nnod_mat_tbl, inod_mat_tbl, nnod_filter_mat)
!
      use m_geometry_parameter
      use m_geometry_data
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: nnod_mat_tbl, nnod_filter_mat
      integer(kind = kint), intent(in) :: inod_mat_tbl(nnod_mat_tbl)
!
      integer(kind = kint) :: inum, iele, k1, inod1
      integer(kind = kint) :: jnum, jnod
!
!
      mat_num_filter = 0
      mat_num_weight = 0
!
!      write(*,*) 'mat_num_filter, mat_num_weight',  &
!     &           size(mat_num_filter,1), size(mat_num_filter,2), &
!     &           size(mat_num_weight,1), size(mat_num_weight,2)
!      write(*,*) 'nele_grp, nnod_4_ele', my_rank, nele_grp, nnod_4_ele
      do inum = 1, nele_grp
        iele = iele_grp(inum)
        do k1 = 1, nnod_4_ele
          inod1 = ie(iele,k1)
!
          do jnum = 1, nnod_filter_mat
            jnod = inod_mat_tbl(jnum)
            if (jnod .eq. inod1) then
              mat_num_filter(inum,k1) = jnum
              mat_num_weight(inum,k1) = jnum
              exit
            end if
          end do
!
          if ( mat_num_weight(inum,k1) .eq. 0 ) then
            do jnum = nnod_filter_mat+1, nnod_mat_tbl
              jnod = inod_mat_tbl(jnum)
              if (jnod .eq. inod1) then
                mat_num_weight(inum,k1) = jnum
                exit
              end if
            end do
          end if
!
        end do
      end do
!
      end subroutine set_idx_list_4_filter_mat
!
!-----------------------------------------------------------------------
!
      subroutine fem_sk_filter_moments(nele_grp, iele_grp,              &
     &          inod, ix, k_order)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_reference_moments
      use m_jacobians
      use m_fem_gauss_int_coefs
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: inod, ix, k_order
!
      integer(kind = kint) :: inum, iele, k1
!
!
      sk_filter = 0.0d0
!
      do inum = 1, nele_grp
        iele = iele_grp(inum)
!
        do k1 = 1, nnod_4_ele
          sk_filter(inum,k1)                                            &
     &     =  ( xx_int(inum,1) - xx(inod,1) )**iorder_mom_3d(k_order,1) &
     &      * ( xx_int(inum,2) - xx(inod,2) )**iorder_mom_3d(k_order,2) &
     &      * ( xx_int(inum,3) - xx(inod,3) )**iorder_mom_3d(k_order,3) &
     &      * aw(k1,ix) * xjac(iele,ix) * owe3d(ix)
        end do
      end do
!
      end subroutine fem_sk_filter_moments
!
!-----------------------------------------------------------------------
!
      subroutine fem_sk_filter_weights(n_int, nele_grp, iele_grp)
!
      use m_geometry_parameter
      use m_geometry_data
      use m_reference_moments
      use m_jacobians
      use m_fem_gauss_int_coefs
      use m_filter_coefs
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: iele_grp(nele_grp)
      integer(kind = kint), intent(in) :: n_int
!
      integer(kind = kint) :: inum, iele, ii, ix, k1, k2, jnum
!
!
!
      sk_filter = 0.0d0
!
      do k2 = 1, nnod_4_ele
!
        do k1 = 1, nnod_4_ele
          do ii = 1, n_int*n_int*n_int
            ix = int_start3(n_int) + ii
              do inum = 1, nele_grp
                iele = iele_grp(inum)
                jnum = mat_num_weight(inum,k1)
!
                sk_filter(inum,k2) = sk_filter(inum,k2)                 &
     &                              + filter_1nod(jnum)                 &
     &                               * aw(k1,ix) * aw(k2,ix)            &
     &                               * xjac(iele,ix) * owe3d(ix)
            end do
          end do
        end do
      end do
!
      end subroutine fem_sk_filter_weights
!
!-----------------------------------------------------------------------
!
      subroutine sum_sk_2_filter_mat(nele_grp, k_order)
!
      use m_geometry_parameter
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: nele_grp
      integer(kind = kint), intent(in) :: k_order
!
      integer(kind = kint) :: inum, k1, jnum
!
!
      do inum = 1, nele_grp
        do k1 = 1, nnod_4_ele
          jnum = k_order + mat_num_filter(inum,k1)*max_mat_size
          mat_work(jnum) = mat_work(jnum) + sk_filter(inum,k1)
        end do
      end do
!
      end subroutine sum_sk_2_filter_mat
!
!-----------------------------------------------------------------------
!
      subroutine copy_2_filter_matrix(num_fixed_point)
!
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: num_fixed_point
      integer(kind = kint) :: k_order, inum, jnum
!
!
      do k_order = 1, (max_mat_size-num_fixed_point)
        do inum = 1, max_mat_size
          jnum = k_order + inum*max_mat_size
          a_mat(k_order+num_fixed_point,inum) = mat_work(jnum)
        end do
      end do
!
      end subroutine copy_2_filter_matrix
!
!-----------------------------------------------------------------------
!
      subroutine substitute_fixed_moments(num_fixed_point)
!
      use m_matrix_4_filter
!
      integer(kind = kint), intent(in) :: num_fixed_point
      integer(kind = kint) :: l_order, k_order, knum
!
!
      do k_order = 1, (max_mat_size-num_fixed_point)
        do l_order = 1, num_fixed_point
          knum = k_order+num_fixed_point
          vec_mat(knum) = vec_mat(knum)                                 &
     &                   - a_mat(knum,l_order) * vec_mat(l_order)
          a_mat(knum,l_order) = 0.0d0
        end do
      end do
!
      end subroutine substitute_fixed_moments
!
!-----------------------------------------------------------------------
!
      subroutine sum_sk_2_filter_weight(nele_grp)
!
      use m_geometry_parameter
      use m_filter_coefs
!
      integer(kind = kint), intent(in) :: nele_grp
!
      integer(kind = kint) :: inum, k2, jnum
!
!
      weight_1nod = 0.0d0
      do inum = 1, nele_grp
        do k2 = 1, nnod_4_ele
          jnum = mat_num_weight(inum,k2)
          weight_1nod(jnum) = weight_1nod(jnum) + sk_filter(inum,k2)
        end do
      end do
!
      end subroutine sum_sk_2_filter_weight
!
!-----------------------------------------------------------------------
!
      end module fem_const_filter_matrix
