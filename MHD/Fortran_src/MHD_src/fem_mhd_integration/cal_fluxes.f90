!
!     module cal_fluxes
!
!      Written by H. Matsui
!
!!      subroutine cal_flux_vector(node, ncomp_nod, i_v, i_s, i_r, d_nod)
!!      subroutine cal_flux_tensor                                      &
!!     &         (node, ncomp_nod, i_v1, i_v2, i_flux, d_nod)
!!      subroutine cal_maxwell_tensor(node, ex_magne,                   &
!!     &          ncomp_nod, i_magne, i_mxwl, d_nod)
!!      subroutine cal_induction_tensor(node, ncomp_nod,                &
!!     &          i_magne, i_velo, i_idct, d_nod)
!
      module cal_fluxes
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_flux_vector(node, ncomp_nod, i_v, i_s, i_r, d_nod)
!
      use t_geometry_data
      use cal_products_smp
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_r, i_s, i_v
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
!$omp parallel
      call cal_vec_scalar_prod_no_coef_smp                              &
     &  (np_smp, node%numnod, node%istack_nod_smp,                      &
     &   d_nod(1,i_v), d_nod(1,i_s), d_nod(1,i_r))
!$omp end parallel
!
      end subroutine cal_flux_vector
!
!-----------------------------------------------------------------------
!
      subroutine cal_flux_tensor                                        &
     &         (node, ncomp_nod, i_v1, i_v2, i_flux, d_nod)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_v1, i_v2, i_flux
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
!$omp parallel
      call cal_flux_tensor_smp                                          &
     &   (node%numnod, np_smp, node%istack_nod_smp,                     &
     &    d_nod(1,i_v1), d_nod(1,i_v2), d_nod(1,i_flux))
!$omp end parallel
!
       end subroutine cal_flux_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_maxwell_tensor(node, ex_magne,                     &
     &          ncomp_nod, i_magne, i_mxwl, d_nod)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_magne, i_mxwl
      real(kind = kreal), intent(in) :: ex_magne(3)
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
!$omp parallel
      call cal_maxwell_tensor_smp                                       &
     &   (node%numnod, np_smp, node%istack_nod_smp,                     &
     &    d_nod(1,i_magne), ex_magne, d_nod(1,i_mxwl))
!$omp end parallel
!
       end subroutine cal_maxwell_tensor
!
!-----------------------------------------------------------------------
!
      subroutine cal_induction_tensor(node, ncomp_nod,                  &
     &          i_magne, i_velo, i_idct, d_nod)
!
      use t_geometry_data
!
      type(node_data), intent(in) :: node
      integer(kind = kint), intent(in) :: ncomp_nod
      integer(kind = kint), intent(in) :: i_magne, i_velo, i_idct
      real(kind = kreal), intent(inout) :: d_nod(node%numnod,ncomp_nod)
!
!
!$omp parallel
      call cal_induction_tensor_smp                                     &
     &   (node%numnod, np_smp, node%istack_nod_smp,                     &
     &    d_nod(1,i_magne), d_nod(1,i_velo), d_nod(1,i_idct))
!$omp end parallel
!
       end subroutine cal_induction_tensor
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_flux_tensor_smp(nnod, np_smp, inod_smp_stack,      &
     &          vec1, vec2, flux)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: vec1(nnod,3)
      real(kind = kreal), intent(in) :: vec2(nnod,3)
!
      real(kind = kreal), intent(inout) :: flux(nnod,6)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          flux(inod,1) = vec1(inod,1) * vec2(inod,1)
          flux(inod,2) = vec1(inod,1) * vec2(inod,2)
          flux(inod,3) = vec1(inod,1) * vec2(inod,3)
          flux(inod,4) = vec1(inod,2) * vec2(inod,2)
          flux(inod,5) = vec1(inod,2) * vec2(inod,3)
          flux(inod,6) = vec1(inod,3) * vec2(inod,3)
        end do
      end do
!$omp end do
!
       end subroutine cal_flux_tensor_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_maxwell_tensor_smp(nnod, np_smp, inod_smp_stack,   &
     &          magne, ex_magne, mxwl)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: magne(nnod,3)
      real(kind = kreal), intent(in) :: ex_magne(3)
!
      real(kind = kreal), intent(inout) :: mxwl(nnod,6)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
           mxwl(inod,1) = ( magne(inod,1)+ex_magne(1) )                 &
     &                  * ( magne(inod,1)+ex_magne(1) )
           mxwl(inod,2) = ( magne(inod,1)+ex_magne(1) )                 &
     &                  * ( magne(inod,2)+ex_magne(2) )
           mxwl(inod,3) = ( magne(inod,1)+ex_magne(1) )                 &
     &                  * ( magne(inod,3)+ex_magne(3) )
           mxwl(inod,4) = ( magne(inod,2)+ex_magne(2) )                 &
     &                  * ( magne(inod,2)+ex_magne(2) )
           mxwl(inod,5) = ( magne(inod,2)+ex_magne(2) )                 &
     &                  * ( magne(inod,3)+ex_magne(3) )
           mxwl(inod,6) = ( magne(inod,3)+ex_magne(3) )                 &
     &                  * ( magne(inod,3)+ex_magne(3) )
        end do
      end do
!$omp end do
!
       end subroutine cal_maxwell_tensor_smp
!
!-----------------------------------------------------------------------
!
      subroutine cal_induction_tensor_smp(nnod, np_smp, inod_smp_stack, &
     &          magne, velocity, idct)
!
      integer (kind=kint), intent(in) :: np_smp, nnod
      integer (kind=kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: magne(nnod,3)
      real(kind = kreal), intent(in) :: velocity(nnod,3)
!
      real(kind = kreal), intent(inout) :: idct(nnod,3)
!
      integer (kind=kint) :: iproc, inod, ist, ied
!
!
!$omp do private(inod,ist,ied)
      do iproc = 1, np_smp
        ist = inod_smp_stack(iproc-1)+1
        ied = inod_smp_stack(iproc)
!
!cdir nodep
        do inod = ist, ied
          idct(inod,1) = magne(inod,2) * velocity(inod,1)               &
     &                - magne(inod,1) * velocity(inod,2)
          idct(inod,2) = magne(inod,3) * velocity(inod,1)               &
     &                - magne(inod,1) * velocity(inod,3)
          idct(inod,3) = magne(inod,3) * velocity(inod,2)               &
     &                - magne(inod,2) * velocity(inod,3) 
        end do
      end do
!$omp end do
!
       end subroutine cal_induction_tensor_smp
!
!-----------------------------------------------------------------------
!
      end module cal_fluxes
