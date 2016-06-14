!>@file   m_fdm_coefs.f90
!!@brief  module m_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    2nd order derivatives on node by nodal field
!!      dfdr =    r_2nd%fdm(1)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(1)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(1)%dmat( 1) * d_nod(k+1)
!!      d2fdr2 =  r_2nd%fdm(2)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(2)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(2)%dmat( 1) * d_nod(k+1)
!!
!!       r_2nd%fdm(1)%dmat = d1nod_mat_fdm_2
!!       r_2nd%fdm(2)%dmat = d2nod_mat_fdm_2
!
!! ----------------------------------------------------------------------!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    r_2nd%wk_mat(2,1,k) * d_nod(k  )
!!              + r_2nd%wk_mat(2,2,k) * d_nod(k+1)
!!              + r_2nd%wk_mat(2,3,k) * d_nod(k-1)
!!      d2fdr2 =  r_2nd%wk_mat(3,1,k) * d_nod(k  )
!!              + r_2nd%wk_mat(3,2,k) * d_nod(k+1)
!!              + r_2nd%wk_mat(3,3,k) * d_nod(k-1)
!!
!!      r_2nd%wk_mat = mat_fdm_2
!! ----------------------------------------------------------------------
!!
!!      subroutine allocate_fdm_coefs(nri)
!!      subroutine allocate_fdm_matrices(nri)
!!
!!      subroutine deallocate_fdm_coefs
!!      subroutine deallocate_fdm_matrices
!!
!!      subroutine copy_fdm_nod_coefs_from_mat(nri)
!!
!!      subroutine check_fdm_2_coefs(nri, r)
!!      subroutine check_fdm_4_coefs(nri, r)
!!
!!      subroutine check_fdm_2_mat(nri, r)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module m_fdm_coefs
!
      use m_precision
      use m_constants
      use t_fdm_coefs
!
      implicit none
!
!
!>        Structure of FDM matrices
      type(fdm_matrices), save :: r_2nd
!
      end module m_fdm_coefs
