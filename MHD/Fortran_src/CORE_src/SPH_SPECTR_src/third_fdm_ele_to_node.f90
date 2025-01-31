!>@file   third_fdm_ele_to_node.f90
!!@brief  module third_fdm_ele_to_node
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Third order FDM from element to node
!!
!!@verbatim
!!      subroutine const_third_fdm_ele_to_node(id_check, sph_rj,        &
!!     &                                       fdm_3rd_nod)
!!        integer(kind = kint), intent(in) :: id_check
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_3rd_nod
!!      subroutine cal_third_fdm_ele_to_node(i_th, kr_in, kr_out,       &
!!     &          sph_rj, fdm_3rd_nod, dele_rj, dnod_dr)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_3rd_nod
!!        real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    derivatives on node by element field
!!      dfdr_nod(k) =    fdm_3rd_nod%dmat(-1,k,1) * d_ele(k-1)
!!                     + fdm_3rd_nod%dmat( 0,k,1) * d_ele(k  )
!!                     + fdm_3rd_nod%dmat( 1,k,1) * d_ele(k+1)
!!                     + fdm_3rd_nod%dmat( 2,k,1) * d_ele(k+2)
!!
!! ----------------------------------------------------------------------
!!     Numbering of node and element
!!       e_k-1      e_k  n_k e_k+1     e_k+2
!!    +----x----+----x----+----x----+----x----+---.....
!!  r(k-2)   r(k-1)      r(k)     r(k+1)
!! ----------------------------------------------------------------------
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module third_fdm_ele_to_node
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
      private :: set_third_fdm_ele_to_node, copy_third_fdm_ele_to_node
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_third_fdm_ele_to_node(id_check, sph_rj,          &
     &                                       fdm_3rd_nod)
!
      integer(kind = kint), intent(in) :: id_check
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: fdm_3rd_nod
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), izero, ithree, ione, itwo, fdm_3rd_nod)
!
      allocate(mat_fdm(4,4,sph_rj%nidx_rj(1)))
      mat_fdm(1:4,1:4,1:sph_rj%nidx_rj(1)) = 0.0d0
!
      call set_third_fdm_ele_to_node                                    &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
!
      call copy_third_fdm_ele_to_node(sph_rj%nidx_rj(1), mat_fdm,       &
     &                                fdm_3rd_nod)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
        write(id_check,*) 'check third order FDM from element to node'
        call check_fdm_coefs(id_check, sph_rj%nidx_rj(1),               &
     &                       sph_rj%radius_1d_rj_r, fdm_3rd_nod)
      end if
!
      end subroutine const_third_fdm_ele_to_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_third_fdm_ele_to_node(nri, r, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm(4,4,nri)
!
      integer(kind = kint) :: kr, ierr
!
      real(kind = kreal) :: dr_n2, dr_n1, dr_p1, dr_p2
      real(kind = kreal) :: mat_taylor_4(4,4)
!
!
      do kr = 1, nri
        if(kr .eq. 1) then
          dr_n2 = r(1)
        else if(kr .eq. 2) then
          dr_n2 = r(2)
        else
          dr_n2 = r(kr) - half * (r(kr-1) + r(kr-2))
        end if
!
        if(kr .eq. 1) then
          dr_n1 = half * r(1)
        else
          dr_n1 = half * (r(kr) - r(kr-1))
        end if
!
        if(kr .eq. nri) then
          dr_p1 = half * (r(nri) - r(nri-1))
        else
          dr_p1 = half * (r(kr+1) - r(kr))
        end if
!
        if(kr .eq. nri-1) then
          dr_p2 = r(nri) - r(nri-1)
        else if(kr .eq. nri) then
          dr_p2 = r(nri) - r(nri-1)
        else
          dr_p2 = half * (r(kr+2) + r(kr+1)) - r(kr)
        end if
!
        mat_taylor_4(1,1) =   one
        mat_taylor_4(1,2) = - dr_n1
        mat_taylor_4(1,3) =   half *        dr_n1**2
        mat_taylor_4(1,4) = - (one / six) * dr_n1**3
!
        mat_taylor_4(2,1) =   one
        mat_taylor_4(2,2) =   dr_p1
        mat_taylor_4(2,3) =   half *        dr_p1**2
        mat_taylor_4(2,4) =   (one / six) * dr_p1**3
!
        mat_taylor_4(3,1) =   one
        mat_taylor_4(3,2) = - dr_n2
        mat_taylor_4(3,3) =   half *        dr_n2**2
        mat_taylor_4(3,4) = - (one / six) * dr_n2**3
!
        mat_taylor_4(4,1) =   one
        mat_taylor_4(4,2) =   dr_p2
        mat_taylor_4(4,3) =   half *        dr_p2**2
        mat_taylor_4(4,4) =   (one / six) * dr_p2**3
!
        call cal_inverse_44_matrix                                      &
     &     (mat_taylor_4, mat_fdm(1,1,kr), ierr)
      end do
!
      end subroutine set_third_fdm_ele_to_node
!
! -----------------------------------------------------------------------
!
      subroutine copy_third_fdm_ele_to_node(nri, mat_fdm, r_fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(4,4,nri)
      type(fdm_matrices), intent(inout) :: r_fdm
!
      integer(kind= kint) :: k, i
!
!
!$omp parallel private(i)
      do i = 0, 3
!$omp do private(k)
        do k = 1, nri-1
          r_fdm%dmat(-1,k,i) = mat_fdm(i+1,3,k)
          r_fdm%dmat( 0,k,i) = mat_fdm(i+1,1,k)
          r_fdm%dmat( 1,k,i) = mat_fdm(i+1,2,k)
          r_fdm%dmat( 2,k,i) = mat_fdm(i+1,4,k)
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine copy_third_fdm_ele_to_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_third_fdm_ele_to_node(i_th, kr_in, kr_out,         &
     &          sph_rj, fdm_3rd_nod, dele_rj, dnod_dr)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrices), intent(in) :: fdm_3rd_nod
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
      real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_n1, i_p1, i_p2, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_n1,i_p1,i_p2,j,k)
      do inod = ist, ied
        i_n1 = inod - sph_rj%nidx_rj(2)
        i_p1 = inod + sph_rj%nidx_rj(2)
        i_p2 = i_p1 + sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dnod_dr(inod) =  fdm_3rd_nod%dmat(-1,k,i_th) * dele_rj(i_n1)    &
     &                 + fdm_3rd_nod%dmat( 0,k,i_th) * dele_rj(inod)    &
     &                 + fdm_3rd_nod%dmat( 1,k,i_th) * dele_rj(i_p1)    &
     &                 + fdm_3rd_nod%dmat( 2,k,i_th) * dele_rj(i_p2)
      end do
!$omp end parallel do
!
      end subroutine cal_third_fdm_ele_to_node
!
! -----------------------------------------------------------------------
!
      end module third_fdm_ele_to_node
