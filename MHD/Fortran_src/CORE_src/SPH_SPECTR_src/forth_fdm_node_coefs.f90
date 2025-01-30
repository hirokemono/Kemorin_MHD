!>@file   forth_fdm_node_coefs.f90
!!@brief  module v
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine const_forth_fdm_coefs(id_check, sph_rj, fdm_4th)
!!        integer(kind = kint), intent(in) :: id_check
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_4th
!!      subroutine cal_forth_fdm_node(i_th, kr_in, kr_out, sph_rj,      &
!!     &                              fdm_4th, d_rj, dfdr_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_4th
!!        real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!!
!!      subroutine set_forth_dr(r, delta)
!!        real(kind = kreal), intent(in) :: r(-2:2)
!!        real(kind = kreal), intent(inout) :: delta(-2:1)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    derivatives on node by element field
!!      dfdr_rj(k) =    fdm_4th%dmat(-2,k,1) * d_nod(k-2)
!!                    + fdm_4th%dmat(-1,k,1) * d_nod(k-1)
!!                    + fdm_4th%dmat( 0,k,1) * d_nod(k  )
!!                    + fdm_4th%dmat( 1,k,1) * d_nod(k+1)
!!                    + fdm_4th%dmat( 2,k,1) * d_nod(k+2)
!!      d2fdr2_rj(k) =  fdm_4th%dmat(-2,k,2) * d_nod(k-2)
!!                    + fdm_4th%dmat(-1,k,2) * d_nod(k-1)
!!                    + fdm_4th%dmat( 0,k,2) * d_nod(k  )
!!                    + fdm_4th%dmat( 1,k,2) * d_nod(k+1)
!!                    + fdm_4th%dmat( 2,k,2) * d_nod(k+02)
!!      d3fdr3_rj(k) =  fdm_4th%dmat(-2,k,3) * d_nod(k-2)
!!                    + fdm_4th%dmat(-1,k,3) * d_nod(k-1)
!!                    + fdm_4th%dmat( 0,k,3) * d_nod(k  )
!!                    + fdm_4th%dmat( 1,k,3) * d_nod(k+1)
!!                    + fdm_4th%dmat( 2,k,3) * d_nod(k+2)
!!      d4fdr4_rj(k) =  fdm_4th%dmat(-2,k,4) * d_nod(k-2)
!!                    + fdm_4th%dmat(-1,k,4) * d_nod(k-1)
!!                    + fdm_4th%dmat( 0,k,4) * d_nod(k  )
!!                    + fdm_4th%dmat( 1,k,4) * d_nod(k+1)
!!                    + fdm_4th%dmat( 2,k,4) * d_nod(k+2)
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
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_forth_fdm_coefs(id_check, sph_rj, fdm_4th)
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(inout) :: fdm_4th
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), ione, ifour, itwo, itwo, fdm_4th)
!
      allocate(mat_fdm(5,5,sph_rj%nidx_rj(1)))
      call set_forth_fdm_node                                           &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
      call copy_forth_fdm_node(sph_rj%nidx_rj(1), mat_fdm, fdm_4th)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
        call check_fdm_coefs(id_check, sph_rj%nidx_rj(1),               &
     &                       sph_rj%radius_1d_rj_r, fdm_4th)
      end if
!
      end subroutine const_forth_fdm_coefs
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
      real(kind = kreal) :: delta(-2:1)
!
!
!$omp parallel workshare
      mat_fdm(1:5,1:5,1:nri) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel do private(kr,delta,mat_taylor_5)
      do kr = 1, nri
        if(kr .eq. 1) then
          delta(-2) = r(kr+1)
        else if(kr .eq. 2) then
          delta(-2) = r(kr  )
        else
          delta(-2) = r(kr) - r(kr-2)
        end if
!
        if(kr .eq. 1) then
          delta(-1) = r(kr)
        else
          delta(-1) = r(kr  ) - r(kr-1)
        end if
!
        if(kr .eq. nri) then
          delta( 0) = r(kr  ) - r(kr-1)
        else
          delta( 0) = r(kr+1) - r(kr)
        end if
!
        if(kr .eq. nri-1) then
          delta( 1) = r(kr+1) - r(kr-1)
        else if(kr .eq. nri) then
          delta( 1) = r(kr) -   r(kr-2)
        else
          delta( 1) = r(kr+2) - r(kr)
        end if
!
        call set_forth_taylor_expand(delta, mat_taylor_5)
        call cal_inverse_nn_matrix                                      &
     &     (ifive, mat_taylor_5, mat_fdm(1,1,kr), ierr)
      end do
!$omp end parallel do
!
      end subroutine set_forth_fdm_node
!
! -----------------------------------------------------------------------
!
      subroutine copy_forth_fdm_node(nri, mat_fdm, r_fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(5,5,nri)
      type(fdm_matrices), intent(inout) :: r_fdm
!
      integer(kind= kint) :: i, k
!
!
!$omp parallel do private (i,k)
      do i = 0, 4
        do k = 1, nri-1
          r_fdm%dmat(-2,k,i) = mat_fdm(i+1,4,k)
          r_fdm%dmat(-1,k,i) = mat_fdm(i+1,2,k)
          r_fdm%dmat( 0,k,i) = mat_fdm(i+1,1,k)
          r_fdm%dmat( 1,k,i) = mat_fdm(i+1,3,k)
          r_fdm%dmat( 2,k,i) = mat_fdm(i+1,5,k)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_forth_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_forth_fdm_node(i_th, kr_in, kr_out, sph_rj,        &
     &                              fdm_4th, d_rj, dfdr_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: fdm_4th
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
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
        dfdr_rj(inod) =  fdm_4th%dmat(-2,k,i_th) * d_rj(i_n2)           &
     &                 + fdm_4th%dmat(-1,k,i_th) * d_rj(i_n1)           &
     &                 + fdm_4th%dmat( 0,k,i_th) * d_rj(inod)           &
     &                 + fdm_4th%dmat( 1,k,i_th) * d_rj(i_p1)           &
     &                 + fdm_4th%dmat( 2,k,i_th) * d_rj(i_p2)
      end do
!$omp end parallel do
!
      end subroutine cal_forth_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_forth_taylor_expand(delta, mat_taylor_5)
!
      real(kind = kreal), intent(in) :: delta(-2:1)
      real(kind = kreal), intent(inout) :: mat_taylor_5(5,5)
!
      real(kind = kreal), parameter :: asix = 1.0d0 / 6.0d0
      real(kind = kreal), parameter :: a24 = 1.0d0 / 24.0d0
!
!
      mat_taylor_5(1,1) =  one
      mat_taylor_5(1,2) =  zero
      mat_taylor_5(1,3) =  zero
      mat_taylor_5(1,4) =  zero
      mat_taylor_5(1,5) =  zero
!
      mat_taylor_5(2,1) =  one
      mat_taylor_5(2,2) = -delta(-1)
      mat_taylor_5(2,3) =  half * delta(-1)**2
      mat_taylor_5(2,4) = -asix * delta(-1)**3
      mat_taylor_5(2,5) =  a24 *  delta(-1)**4
!
      mat_taylor_5(3,1) =  one
      mat_taylor_5(3,2) =  delta( 0)
      mat_taylor_5(3,3) =  half * delta( 0)**2
      mat_taylor_5(3,4) =  asix * delta( 0)**3
      mat_taylor_5(3,5) =  a24 *  delta( 0)**4
!
      mat_taylor_5(4,1) =  one
      mat_taylor_5(4,2) = -delta(-2)
      mat_taylor_5(4,3) =  half * delta(-2)**2
      mat_taylor_5(4,4) = -asix * delta(-2)**3
      mat_taylor_5(4,5) =  a24 *  delta(-2)**4
!
      mat_taylor_5(5,1) =  one
      mat_taylor_5(5,2) =  delta( 1)
      mat_taylor_5(5,3) =  half * delta( 1)**2
      mat_taylor_5(5,4) =  asix * delta( 1)**3
      mat_taylor_5(5,5) =  a24 *  delta( 1)**4
!
      end subroutine set_forth_taylor_expand
!
! -----------------------------------------------------------------------
!
      end module forth_fdm_node_coefs
