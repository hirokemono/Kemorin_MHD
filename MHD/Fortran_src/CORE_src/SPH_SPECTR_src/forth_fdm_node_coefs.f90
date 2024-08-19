!>@file   forth_fdm_node_coefs.f90
!!@brief  module forth_fdm_node_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine const_forth_fdm_coefs(sph_rj, fdm_4th)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_4th
!!      subroutine cal_forth_fdm_node(i_th, kr_in, kr_out,              &
!!     &          sph_rj, fdm_4th, d_rj, dfdr_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_4th
!!        real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    derivatives on node by element field
!!      dfdr_rj(k) =    fdm_4th%fdm(1)%dmat(k,-2) * d_nod(k-2)
!!                    + fdm_4th%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_4th%fdm(1)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_4th%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!                    + fdm_4th%fdm(1)%dmat(k, 2) * d_nod(k+2)
!!      d2fdr2_rj(k) =  fdm_4th%fdm(2)%dmat(k,-2) * d_nod(k-2)
!!                    + fdm_4th%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_4th%fdm(2)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_4th%fdm(2)%dmat(k, 1) * d_nod(k+1)
!!                    + fdm_4th%fdm(2)%dmat(k, 2) * d_nod(k+02)
!!      d3fdr3_rj(k) =  fdm_4th%fdm(3)%dmat(k,-2) * d_nod(k-2)
!!                    + fdm_4th%fdm(3)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_4th%fdm(3)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_4th%fdm(3)%dmat(k, 1) * d_nod(k+1)
!!                    + fdm_4th%fdm(3)%dmat(k, 2) * d_nod(k+2)
!!      d4fdr4_rj(k) =  fdm_4th%fdm(4)%dmat(k,-2) * d_nod(k-2)
!!                    + fdm_4th%fdm(4)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_4th%fdm(4)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_4th%fdm(4)%dmat(k, 1) * d_nod(k+1)
!!                    + fdm_4th%fdm(4)%dmat(k, 2) * d_nod(k+2)
!!
!!    fdm_4th%fdm(1)%dmat = d1nod_mat_fdm_2e
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by element field
!!      dfdr_rj(k) =  fdm_4th%wk_mat(2,4) * d_nod(k-2)
!!                  + fdm_4th%wk_mat(2,2) * d_nod(k-1)
!!                  + fdm_4th%wk_mat(2,1) * d_nod(k  )
!!                  + fdm_4th%wk_mat(2,3) * d_nod(k+1)
!!                  + fdm_4th%wk_mat(2,5) * d_nod(k+2)
!! ----------------------------------------------------------------------
!!     Numbering of node and element
!!      n_k-2 e_k-1 n_k-1  e_k   n_k  e_k+1 n_k+1 e_k+2 n_k+2
!!     ...+-----x-----+-----x-----+-----x-----+-----x-----+.....
!!     r(k-2)       r(k-1)       r(k)       r(k+1)     r(k+2)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module forth_fdm_node_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
      private :: set_forth_fdm_node, copy_forth_fdm_node
      private :: cal_sph_vect_forth_dxr_node
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_forth_fdm_coefs(sph_rj, fdm_4th)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: fdm_4th
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), ione, ifour, itwo, itwo, fdm_4th)
!
      allocate(mat_fdm(5,5,sph_rj%nidx_rj(1)))
      mat_fdm(1:5,1:5,1:sph_rj%nidx_rj(1)) = 0.0d0
!
      call set_forth_fdm_node                                           &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
!
      call copy_forth_fdm_node                                          &
     &   (sph_rj%nidx_rj(1), mat_fdm, fdm_4th%fdm)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, fdm_4th)
      end if
!
      end subroutine const_forth_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine cal_forth_fdm_node(i_th, kr_in, kr_out,                &
     &          sph_rj, fdm_4th, d_rj, dfdr_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      type(fdm_matrices), intent(in) :: fdm_4th
!
      real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!
!
      call cal_sph_vect_forth_dxr_node(kr_in, kr_out, sph_rj,           &
     &    fdm_4th%fdm(i_th), d_rj, dfdr_rj)
!
      end subroutine cal_forth_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_forth_fdm_node(nri, r, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm(5,5,nri)
!
      integer(kind = kint) :: kr, ierr
!
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_n2, dr_n1, dr_p1, dr_p2
      real(kind = kreal), parameter :: asix = 1.0d0 / 6.0d0
      real(kind = kreal), parameter :: a24 = 1.0d0 / 24.0d0
!
!
      do kr = 1, nri
        if (kr .eq. 1) then
          dr_n2 = r(2)
          dr_n1 = r(1)
          dr_p1 = r(2) - r(1)
          dr_p2 = r(3) - r(1)
        else if(kr .eq. 2) then
          dr_n2 = r(2)
          dr_n1 = r(2) - r(1)
          dr_p1 = r(3) - r(2)
          dr_p2 = r(4) - r(2)
        else if(kr .eq. nri-1) then
          dr_n2 = r(nri-1) - r(nri-3)
          dr_n1 = r(nri-1) - r(nri-2)
          dr_p1 = r(nri) - r(nri-1)
          dr_p2 = 2.0 * dr_p1
        else if(kr .eq. nri) then
          dr_n2 = r(nri) - r(nri-2)
          dr_n1 = r(nri) - r(nri-1)
          dr_p1 = dr_n1
          dr_p2 = dr_n2
        else
          dr_n2 = r(kr) - r(kr-2)
          dr_n1 = r(kr) - r(kr-1)
          dr_p1 = r(kr+1) - r(kr)
          dr_p2 = r(kr+2) - r(kr)
        end if
!
        mat_taylor_5(1,1) =  one
        mat_taylor_5(1,2) =  zero
        mat_taylor_5(1,3) =  zero
        mat_taylor_5(1,4) =  zero
        mat_taylor_5(1,5) =  zero
!
        mat_taylor_5(2,1) =  one
        mat_taylor_5(2,2) = -dr_n1
        mat_taylor_5(2,3) =  half * dr_n1**2
        mat_taylor_5(2,4) = -asix * dr_n1**3
        mat_taylor_5(2,5) =  a24 *  dr_n1**4
!
        mat_taylor_5(3,1) =  one
        mat_taylor_5(3,2) =  dr_p1
        mat_taylor_5(3,3) =  half * dr_p1**2
        mat_taylor_5(3,4) =  asix * dr_p1**3
        mat_taylor_5(3,5) =  a24 *  dr_p1**4
!
        mat_taylor_5(4,1) =  one
        mat_taylor_5(4,2) = -dr_n2
        mat_taylor_5(4,3) =  half * dr_n2**2
        mat_taylor_5(4,4) = -asix * dr_n2**3
        mat_taylor_5(4,5) =  a24 *  dr_n2**4
!
        mat_taylor_5(5,1) =  one
        mat_taylor_5(5,2) =  dr_p2
        mat_taylor_5(5,3) =  half * dr_p2**2
        mat_taylor_5(5,4) =  asix * dr_p2**3
        mat_taylor_5(5,5) =  a24 *  dr_p2**4
!
        call cal_inverse_nn_matrix                                      &
     &     (ifive, mat_taylor_5, mat_fdm(1,1,kr), ierr)
      end do
!
      end subroutine set_forth_fdm_node
!
! -----------------------------------------------------------------------
!
      subroutine copy_forth_fdm_node(nri, mat_fdm, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(5,5,nri)
      type(fdm_matrix), intent(inout) :: fdm(4)
!
      integer(kind= kint) :: i, k
!
!
!$omp parallel do private (i,k)
      do i = 1, 4
        do k = 1, nri-1
          fdm(i)%dmat(k,-2) = mat_fdm(i+1,4,k)
          fdm(i)%dmat(k,-1) = mat_fdm(i+1,2,k)
          fdm(i)%dmat(k, 0) = mat_fdm(i+1,1,k)
          fdm(i)%dmat(k, 1) = mat_fdm(i+1,3,k)
          fdm(i)%dmat(k, 2) = mat_fdm(i+1,5,k)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_forth_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_forth_dxr_node(kr_in, kr_out, sph_rj,     &
     &                                       fdm, d_rj, dfdr_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrix), intent(in) :: fdm
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_n2, i_n1, i_p1, i_p2, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_n2,i_n1,i_p1,i_p2,j,k)
      do inod = ist, ied
        i_n1 = inod - sph_rj%nidx_rj(2)
        i_n2 = i_n1 - sph_rj%nidx_rj(2)
        i_p1 = inod + sph_rj%nidx_rj(2)
        i_p2 = i_p1 + sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dfdr_rj(inod) =  fdm%dmat(k,-2) * d_rj(i_n2)                    &
     &                 + fdm%dmat(k,-1) * d_rj(i_n1)                    &
     &                 + fdm%dmat(k, 0) * d_rj(inod)                    &
     &                 + fdm%dmat(k, 1) * d_rj(i_p1)                    &
     &                 + fdm%dmat(k, 2) * d_rj(i_p2)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_forth_dxr_node
!
! -----------------------------------------------------------------------
!
      end module forth_fdm_node_coefs
