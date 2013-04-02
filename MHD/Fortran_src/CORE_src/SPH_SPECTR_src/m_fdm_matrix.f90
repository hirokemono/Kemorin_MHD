!
!      module m_fdm_matrix
!
!     Written by H. Matsui on Jan, 2010
!
!
!      dfdr =    mat_fdm_2(2,1) * d_nod(k  )
!              + mat_fdm_2(2,2) * d_nod(k+1)
!              + mat_fdm_2(2,3) * d_nod(k-1)
!      d2fdr2 =  mat_fdm_2(3,1) * d_nod(k  )
!              + mat_fdm_2(3,2) * d_nod(k+1)
!              + mat_fdm_2(3,3) * d_nod(k-1)
!
!    derivatives on node by element field
!      d_nod(k) =   mat_fdm_2e(1,1) * d_ele(k+1)
!                 + mat_fdm_2e(1,2) * d_ele(k  )
!      dfdr =       mat_fdm_2e(2,1) * d_ele(k+1)
!                 + mat_fdm_2e(2,2) * d_ele(k  )
!
!      subroutine allocate_fdm_matrices(nri)
!      subroutine deallocate_fdm_matrices
!
!      subroutine check_fdm_2_mat(nri, r)
!      subroutine check_fdm_2e_mat(nri, r)
!
      module m_fdm_matrix
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: mat_fdm_2(:,:,:)
      real(kind = kreal), allocatable :: mat_fdm_2e(:,:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_fdm_matrices(nri)
!
      integer(kind = kint), intent(in) :: nri
!
!
      allocate( mat_fdm_2(3,3,nri) )
      allocate( mat_fdm_2e(2,2,nri) )
!
      mat_fdm_2 = 0.0d0
      mat_fdm_2e = 0.0d0
!
      end subroutine allocate_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_fdm_matrices
!
!
      deallocate( mat_fdm_2, mat_fdm_2e )
!
      end subroutine deallocate_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2_mat(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, df_0, df_p, df_n'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), mat_fdm_2(2,1:3,kr)
      end do
      write(50,*) 'kr, r, d2f_0, d2f_p, d2f_n'
      do kr = 1, nri
        write(50,'(i5,1p4e20.12)') kr, r(kr), mat_fdm_2(3,1:3,kr)
      end do
!
      end subroutine check_fdm_2_mat
!
! -----------------------------------------------------------------------
!
      subroutine check_fdm_2e_mat(nri, r)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, cnod_p, cnod_n'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_2e(1,1:2,kr)
      end do
      write(50,*) 'kr, r, dfe_p, d2fe_n'
      do kr = 1, nri
        write(50,'(i5,1p3e20.12)') kr, r(kr), mat_fdm_2e(2,1:2,kr)
      end do
!
      end subroutine check_fdm_2e_mat
!
! -----------------------------------------------------------------------
!
      end module m_fdm_matrix
