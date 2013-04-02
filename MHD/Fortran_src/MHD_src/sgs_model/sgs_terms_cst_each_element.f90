!sgs_terms_cst_each_element.f90
!      module sgs_terms_cst_each_element
!
!      Written by H. Matsui on July, 2005
!
!      subroutine SGS_vector_cst_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_scalar,      &
!     &          i_sgs, ntot_comp, d_nod, coef, sgs_e, flux_e)
!      subroutine SGS_tensor_cst_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_sgs,         &
!     &          ntot_comp, d_nod, sgs_e, coef, flux_e)
!      subroutine SGS_induct_cst_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_b, i_v, i_sgs,       &
!     &          ntot_comp, d_nod, sgs_e, coef, flux_e)
!
!      subroutine SGS_flux_cst_each_ele_vec(numnod, numele,             &
!     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,        &
!     &          i_vect, i_field, i_sgs, ntot_comp, d_nod, coef,        &
!     &          sgs_e, flux_e)
!      subroutine SGS_induct_vec_cst_each_ele(numnod, numele,           &
!     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,        &
!     &          i_b, i_v, i_sgs, ntot_comp, d_nod, coef,               &
!     &          sgs_e, flux_e)
!
      module sgs_terms_cst_each_element
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_vector_cst_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_scalar,       &
     &          i_sgs, ntot_comp, d_nod, coef, sgs_e, flux_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_scalar, i_vect, i_sgs
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           sgs_e(iele,1) = coef * d_nod(inod,i_sgs  )
           sgs_e(iele,2) = coef * d_nod(inod,i_sgs+1)
           sgs_e(iele,3) = coef * d_nod(inod,i_sgs+2)
           flux_e(iele,1) =  coef * (d_nod(inod,i_sgs  )                &
     &                     + d_nod(inod,i_scalar)*d_nod(inod,i_vect  ))
           flux_e(iele,2) =  coef * (d_nod(inod,i_sgs+1)                &
     &                     + d_nod(inod,i_scalar)*d_nod(inod,i_vect+1))
           flux_e(iele,3) =  coef * (d_nod(inod,i_sgs+2)                &
     &                     + d_nod(inod,i_scalar)*d_nod(inod,i_vect+2))
         end do
       end do
!$omp end parallel do
!
      end subroutine SGS_vector_cst_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_tensor_cst_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_vect, i_sgs,          &
     &          ntot_comp, d_nod, coef, sgs_e, flux_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_vect, i_sgs
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,6)
      real (kind=kreal), intent(inout) :: sgs_e(numele,6)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           sgs_e(iele,1) = coef * d_nod(inod,i_sgs  )
           sgs_e(iele,2) = coef * d_nod(inod,i_sgs+1)
           sgs_e(iele,3) = coef * d_nod(inod,i_sgs+2)
           sgs_e(iele,4) = coef * d_nod(inod,i_sgs+3)
           sgs_e(iele,5) = coef * d_nod(inod,i_sgs+4)
           sgs_e(iele,6) = coef * d_nod(inod,i_sgs+5)
           flux_e(iele,1) =   coef * (d_nod(inod,i_sgs  )               &
     &                     + d_nod(inod,i_vect  )*d_nod(inod,i_vect  ))
           flux_e(iele,2) =   coef * (d_nod(inod,i_sgs+1)               &
     &                     + d_nod(inod,i_vect  )*d_nod(inod,i_vect+1))
           flux_e(iele,3) =   coef * (d_nod(inod,i_sgs+2)               &
     &                     + d_nod(inod,i_vect  )*d_nod(inod,i_vect+2))
           flux_e(iele,4) =   coef * (d_nod(inod,i_sgs+3)               &
     &                     + d_nod(inod,i_vect+1)*d_nod(inod,i_vect+1))
           flux_e(iele,5) =   coef * (d_nod(inod,i_sgs+4)               &
     &                     + d_nod(inod,i_vect+1)*d_nod(inod,i_vect+2))
           flux_e(iele,6) =   coef * (d_nod(inod,i_sgs+5)               &
     &                     + d_nod(inod,i_vect+2)*d_nod(inod,i_vect+2))
         end do
       end do
!$omp end parallel do
!
      end subroutine SGS_tensor_cst_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_cst_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_b, i_v, i_sgs,        &
     &          ntot_comp, d_nod, coef, sgs_e, flux_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
!
!$omp parallel do private(iele,inod,ist,ied)
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           sgs_e(iele,1) = coef * d_nod(inod,i_sgs  )
           sgs_e(iele,2) = coef * d_nod(inod,i_sgs+1)
           sgs_e(iele,3) = coef * d_nod(inod,i_sgs+2)
           flux_e(iele,1) =  coef * (d_nod(inod,i_sgs  )                &
     &                     + d_nod(inod,i_b  )*d_nod(inod,i_v+1)        &
     &                     - d_nod(inod,i_v  )*d_nod(inod,i_b+1))
           flux_e(iele,2) =  coef * (d_nod(inod,i_sgs+1)                &
     &                     + d_nod(inod,i_b+1)*d_nod(inod,i_v+2)        &
     &                     - d_nod(inod,i_v+1)*d_nod(inod,i_b+2))
           flux_e(iele,3) =  coef * (d_nod(inod,i_sgs+2)                &
     &                     + d_nod(inod,i_b+2)*d_nod(inod,i_v  )        &
     &                     - d_nod(inod,i_v+2)*d_nod(inod,i_b  ))
         end do
       end do
!$omp end parallel do
!
!
      end subroutine SGS_induct_cst_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine SGS_flux_cst_each_ele_vec(numnod, numele,              &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_vect, i_field, i_sgs, ntot_comp, d_nod, coef,         &
     &          sgs_e, flux_e)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_vect, i_field, i_sgs
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: n1, n2, n3, nf
!
!
      n1 = i_sgs + l_sim_t(1,nd)
      n2 = i_sgs + l_sim_t(2,nd)
      n3 = i_sgs + l_sim_t(3,nd)
      nf = i_field + nd - 1
!
!$omp parallel do private(iele,inod,ist,ied) 
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           sgs_e(iele,1) = coef * d_nod(inod,n1)
           sgs_e(iele,2) = coef * d_nod(inod,n2)
           sgs_e(iele,3) = coef * d_nod(inod,n3)
           flux_e(iele,1) =  coef * (d_nod(inod,n1)                     &
     &                     + d_nod(inod,i_vect  )*d_nod(inod,nf) )
           flux_e(iele,2) =  coef * (d_nod(inod,n2)                     &
     &                     + d_nod(inod,i_vect+1)*d_nod(inod,nf) )
           flux_e(iele,3) =  coef * (d_nod(inod,n3)                     &
     &                     + d_nod(inod,i_vect+2)*d_nod(inod,nf) )
         end do
       end do
!$omp end parallel do
!
      end subroutine SGS_flux_cst_each_ele_vec
!
!  ---------------------------------------------------------------------
!
      subroutine SGS_induct_vec_cst_each_ele(numnod, numele,            &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2, nd,         &
     &          i_b, i_v, i_sgs, ntot_comp, d_nod, coef,               &
     &          sgs_e, flux_e)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: i_b, i_v, i_sgs
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
!
      real (kind=kreal), intent(inout) :: flux_e(numele,3)
      real (kind=kreal), intent(inout) :: sgs_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele
      integer(kind = kint) :: ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
       n1 = i_sgs + l_asim_t(1,nd,1)
       n2 = i_sgs + l_asim_t(2,nd,1)
       n3 = i_sgs + l_asim_t(3,nd,1)
!$omp parallel do private(iele,inod,ist,ied)
       do iproc = 1, np_smp
         ist = iele_smp_stack(iproc-1) + 1
         ied = iele_smp_stack(iproc)
!cdir nodep
         do iele = ist, ied
           inod = ie(iele,k2)
           sgs_e(iele,1) = coef * dble(l_asim_t(1,nd,2))                &
     &                     * d_nod(inod,n1)
           sgs_e(iele,2) = coef * dble(l_asim_t(2,nd,2))                &
     &                      * d_nod(inod,n2)
           sgs_e(iele,3) = coef * dble(l_asim_t(3,nd,2))                &
     &                      * d_nod(inod,n3)
           flux_e(iele,1) = coef                                        &
     &                     * (dble(l_asim_t(1,nd,2)) * d_nod(inod,n1)   &
     &                     + d_nod(inod,i_b  )*d_nod(inod,i_v+nd-1)     &
     &                     - d_nod(inod,i_v  )*d_nod(inod,i_b+nd-1)) 
           flux_e(iele,2) = coef                                        &
     &                     * (dble(l_asim_t(2,nd,2)) * d_nod(inod,n2)   &
     &                     + d_nod(inod,i_b+1)*d_nod(inod,i_v+nd-1)     &
     &                     - d_nod(inod,i_v+1)*d_nod(inod,i_b+nd-1))
           flux_e(iele,3) = coef                                        &
     &                     * (dble(l_asim_t(3,nd,2)) * d_nod(inod,n3)   &
     &                     + d_nod(inod,i_b+2)*d_nod(inod,i_v+nd-1)     &
     &                     - d_nod(inod,i_v+2)*d_nod(inod,i_b+nd-1))
         end do
       end do
!$omp end parallel do
!
!
      end subroutine SGS_induct_vec_cst_each_ele
!
!  ---------------------------------------------------------------------
!
      end module sgs_terms_cst_each_element
