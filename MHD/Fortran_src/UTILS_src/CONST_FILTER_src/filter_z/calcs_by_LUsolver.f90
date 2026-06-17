!>@file   calcs_by_LUsolver.f90
!!        module calcs_by_LUsolver
!!
!!@author H. Matsui
!!@date Programmed in ???
!
!>@brief Solve vertical differenciate by LU decompoistion
!!
!!@verbatim
!!      subroutine solve_z_commute_LU(numnod, mat_crs)
!!        integer(kind = kint), intent(in) :: numnod
!!        type(CRS_matrix), intent(inout) :: mat_crs
!!      subroutine solve_delta_z_etc_LU(numnod, mk_mat, rhs_dz, X_lu)
!!        integer(kind = kint), intent(in) :: numnod
!!        real(kind = kreal), intent(in) :: mk_mat(numnod,numnod)
!!        real(kind = kreal), intent(in) :: rhs_dz(numnod)
!!        real(kind = kreal), intent(inout) :: X_lu(numnod)
!!@endverbatim
!
      module calcs_by_LUsolver
!
      use m_precision
!
      use m_matrix_4_LU
      use m_ludcmp
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine solve_z_commute_LU(numnod, mat_crs)
!
      use m_commute_filter_z
      use t_crs_matrix
!
      integer(kind = kint), intent(in) :: numnod
      type(CRS_matrix), intent(inout) :: mat_crs
!
      integer(kind = kint) :: inod, i, j, ji
!
       ncomp_lu = ncomp_mat
       call allocate_matrix_4_LU
!
       do inod = 1, numnod
!
         do i = 1, ncomp_mat
           do j = 1, ncomp_mat
             ji = j + (i-1)*ncomp_mat + (inod-1)*ncomp_mat*ncomp_mat
             a_nod(j,i) = mat_crs%D_crs(ji)
           end do
           b_nod(i) = mat_crs%B_crs( ncomp_lu*(inod-1)+i )
         end do
!
!c decompose A = LU
         call ludcmp(a_nod,ncomp_lu,ncomp_lu,indx,d_nod)
!c solve Ax=LUx=b
         call lubksb(a_nod,ncomp_lu,ncomp_lu,indx,b_nod)
!
         do i = 1, ncomp_mat
           mat_crs%X_crs( ncomp_mat*(inod-1)+i ) = b_nod(i)
           d_nod = d_nod*a_nod(i,i)
         end do
!
         write(*,*) 'det A', inod, d_nod
!
       end do
!
      end subroutine solve_z_commute_LU
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine solve_delta_z_etc_LU(numnod, mk_mat, rhs_dz, X_lu)
!
      integer(kind = kint), intent(in) :: numnod
      real(kind = kreal), intent(in) :: mk_mat(numnod,numnod)
      real(kind = kreal), intent(in) :: rhs_dz(numnod)
!
      real(kind = kreal), intent(inout) :: X_lu(numnod)
!
      integer(kind = kint) :: i, j
!
         ncomp_lu = numnod
!
         call allocate_matrix_4_LU
!
         do i = 1, numnod
           do j = 1, numnod
             a_nod(j,i) = mk_mat(i,j)
           end do
           b_nod(i) = rhs_dz(i)
         end do
!
!c decompose A = LU
         call ludcmp(a_nod, ncomp_lu, ncomp_lu, indx, d_nod)
!c solve Ax=LUx=b
         call lubksb(a_nod, ncomp_lu, ncomp_lu, indx, b_nod)
!
         do i = 1, numnod
           X_lu(i) = b_nod(i)
           d_nod = d_nod*a_nod(i,i)
         end do
!
         write(*,*) 'det A', d_nod
!
         call deallocate_matrix_4_LU
!
      end subroutine solve_delta_z_etc_LU
!
!  ---------------------------------------------------------------------
!
      end module calcs_by_LUsolver
