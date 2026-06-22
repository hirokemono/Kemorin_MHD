!>@file   calcs_by_LUsolver.f90
!!        module calcs_by_LUsolver
!!
!!@author H. Matsui
!!@date Programmed in ???
!
!>@brief Solve vertical differenciate by LU decompoistion
!!
!!@verbatim
!!      subroutine solve_z_commute_LU(numnod, ncomp_mat, mat_crs)
!!        integer(kind = kint), intent(in) :: numnod, ncomp_mat
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
      use t_matrix_4_LU
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
      subroutine solve_z_commute_LU(numnod, ncomp_mat, mat_crs)
!
      use t_crs_matrix
!
      integer(kind = kint), intent(in) :: numnod, ncomp_mat
      type(CRS_matrix), intent(inout) :: mat_crs
!
      type(matrix_4_LU) :: LU_mat
      integer(kind = kint) :: inod, ist, jst
!
      call alloc_matrix_4_LU(ncomp_mat, LU_mat)
!
      do inod = 1, numnod
        jst = (inod-1) * ncomp_mat
        ist = jst * ncomp_mat
        call solve_by_LU_decomp(ncomp_mat, mat_crs%A_crs(ist+1),        &
     &      mat_crs%B_crs(jst+1), mat_crs%X_crs(jst+1), LU_mat)
      end do
!
      call dealloc_matrix_4_LU(LU_mat)
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
      type(matrix_4_LU) :: LU_mat
!
      call alloc_matrix_4_LU(numnod, LU_mat)
      call solve_by_LU_decomp(numnod, mk_mat, rhs_dz, X_lu, LU_mat)
      call dealloc_matrix_4_LU(LU_mat)
!
      end subroutine solve_delta_z_etc_LU
!
!  ---------------------------------------------------------------------
!
      end module calcs_by_LUsolver
