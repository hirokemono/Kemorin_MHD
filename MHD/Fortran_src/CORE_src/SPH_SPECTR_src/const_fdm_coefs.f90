!>@file   const_fdm_coefs.f90
!!@brief  module const_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!!      subroutine const_2nd_fdm_matrices(sph_params, sph_rj, r_2nd)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(inout) :: r_2nd
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    2nd order derivatives on node by nodal field
!!      dfdr =    r_2nd%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!              + r_2nd%fdm(1)%dmat(k, 0) * d_nod(k  )
!!              + r_2nd%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!      d2fdr2 =  r_2nd%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!              + r_2nd%fdm(2)%dmat(k, 0) * d_nod(k  )
!!              + r_2nd%fdm(2)%dmat(k, 1) * d_nod(k+1)
!!
!!       r_2nd%fdm(1)%dmat = d1nod_mat_fdm_2
!!       r_2nd%fdm(2)%dmat = d2nod_mat_fdm_2
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
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
!!      subroutine const_4th_fdm_coefs(nlayer_ICB, sph_rj, r_4th)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(inout) :: r_4th
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    4th order derivatives on node by nodal field
!!      dfdr =    r_4th%fdm(1)%dmat(k,-2) *  d_nod(k-2)
!!              + r_4th%fdm(1)%dmat(k,-1) *  d_nod(k-1)
!!              + r_4th%fdm(1)%dmat(k, 0) *  d_nod(k  )
!!              + r_4th%fdm(1)%dmat(k, 1) *  d_nod(k+1)
!!              + r_4th%fdm(1)%dmat(k, 2) *  d_nod(k+2)
!!      d2fdr2 =  r_4th%fdm(2)%dmat(k,-2) *  d_nod(k-2)
!!              + r_4th%fdm(2)%dmat(k,-1) *  d_nod(k-1)
!!              + r_4th%fdm(2)%dmat(k, 1) *  d_nod(k  )
!!              + r_4th%fdm(2)%dmat(k, 0) *  d_nod(k+1)
!!              + r_4th%fdm(2)%dmat(k, 2) *  d_nod(k+2)
!!      d3fdr3 =  r_4th%fdm(3)%dmat(k,-2) *  d_nod(k-2)
!!              + r_4th%fdm(3)%dmat(k,-1) *  d_nod(k-1)
!!              + r_4th%fdm(3)%dmat(k, 0) *  d_nod(k  )
!!              + r_4th%fdm(3)%dmat(k, 1) *  d_nod(k+1)
!!              + r_4th%fdm(3)%dmat(k, 2) *  d_nod(k+2)
!!      d4fdr4 =  r_4th%fdm(4)%dmat(k,-2) *  d_nod(k-2)
!!              + r_4th%fdm(4)%dmat(k,-1) *  d_nod(k-1)
!!              + r_4th%fdm(4)%dmat(k, 0) *  d_nod(k  )
!!              + r_4th%fdm(4)%dmat(k, 1) *  d_nod(k+1)
!!              + r_4th%fdm(4)%dmat(k, 2) *  d_nod(k+2)
!!
!!       r_4th%fdm(1)%dmat = d1nod_mat_fdm_4
!!       r_4th%fdm(2)%dmat = d2nod_mat_fdm_4
!!       r_4th%fdm(3)%dmat = d3nod_mat_fdm_4
!!       r_4th%fdm(4)%dmat = d4nod_mat_fdm_4
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by nodal field
!!      dfdr =    r_4th%wk_mat(2,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(2,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(2,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(2,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(2,5,k) * d_nod(k-2)
!!      d2fdr2 =  r_4th%wk_mat(3,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(3,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(3,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(3,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(3,5,k) * d_nod(k-2)
!!      d3fdr3 =  r_4th%wk_mat(4,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(4,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(4,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(4,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(4,5,k) * d_nod(k-2)
!!      d4fdr4 =  r_4th%wk_mat(5,1,k) * d_nod(k  )
!!              + r_4th%wk_mat(5,2,k) * d_nod(k+1)
!!              + r_4th%wk_mat(5,3,k) * d_nod(k-1)
!!              + r_4th%wk_mat(5,4,k) * d_nod(k+2)
!!              + r_4th%wk_mat(5,5,k) * d_nod(k-2)
!!
!!      r_4th%wk_mat = mat_fdm_4
!! ----------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module const_fdm_coefs
!
      use m_precision
      use m_constants
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_2nd_fdm_matrices(sph_params, sph_rj, r_2nd)
!
      use calypso_mpi
      use set_radius_func_noequi
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(fdm_matrices), intent(inout) :: r_2nd
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), itwo, ione, ione, r_2nd)
      call alloc_fdm_work(sph_rj%nidx_rj(1), r_2nd)
!   Choose radial differences
      call nod_r_2nd_fdm_coefs_nonequi(sph_params%nlayer_ICB,           &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, r_2nd%wk_mat)
      call deallocate_dr_rj_noequi
      call calypso_mpi_barrier
!
      call copy_fdm2_nod_coefs_from_mat(sph_rj%nidx_rj(1), r_2nd)
      call dealloc_fdm_work(r_2nd)
!
      if(my_rank .eq. 0) then
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, r_2nd)
      end if
!
      end subroutine const_2nd_fdm_matrices
!
!
      end module const_fdm_coefs
