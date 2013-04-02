!nodal_cst_fld_each_element.f90
!      module nodal_cst_fld_each_element
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine const_scalar_2_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,      &
!     &          d_nod, coef, scalar_e)
!      subroutine const_vector_2_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,      &
!     &          d_nod, coef, vector_e)
!      subroutine const_tensor_2_each_ele(numnod, numele, nnod_4_ele,   &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,      &
!     &          d_nod, coef, tensor_e)
!
!      subroutine const_tensor_2_vec_each_ele(numnod, numele,           &
!     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2,            &
!     &          i_fld, nd, ntot_comp, d_nod, coef, vect_e)
!      subroutine const_as_tsr_2_vec_each_ele(numnod, numele,           &
!     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2,            &
!     &          i_fld, nd, ntot_comp, d_nod, coef, vect_e)
!
      module nodal_cst_fld_each_element
!
      use m_precision
      use m_constants
!
       implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine const_scalar_2_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,       &
     &          d_nod, coef, scalar_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(numele)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
!
!
!$omp parallel do private(iele,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
!cdir nodep
        do iele = ist, ied
           inod = ie(iele,k2)
           scalar_e(iele) = coef * d_nod(inod,i_fld)
        end do
      end do
!$omp end parallel do
!
      end subroutine const_scalar_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine const_vector_2_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,       &
     &          d_nod, coef, vector_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vector_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
!cdir nodep
        do iele = ist, ied
          inod = ie(iele,k2)
!
          vector_e(iele,1) = coef * d_nod(inod,i_fld  )
          vector_e(iele,2) = coef * d_nod(inod,i_fld+1)
          vector_e(iele,3) = coef * d_nod(inod,i_fld+2)
        end do
      end do
!$omp end parallel do
!
      end subroutine const_vector_2_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine const_tensor_2_each_ele(numnod, numele, nnod_4_ele,    &
     &          ie, np_smp, iele_smp_stack, k2, i_fld, ntot_comp,       &
     &          d_nod, coef, tensor_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: tensor_e(numele,6)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
!cdir nodep
        do iele = ist, ied
          inod = ie(iele,k2)
!
          tensor_e(iele,1) = coef * d_nod(inod,i_fld  )
          tensor_e(iele,2) = coef * d_nod(inod,i_fld+1)
          tensor_e(iele,3) = coef * d_nod(inod,i_fld+2)
          tensor_e(iele,4) = coef * d_nod(inod,i_fld+3)
          tensor_e(iele,5) = coef * d_nod(inod,i_fld+4)
          tensor_e(iele,6) = coef * d_nod(inod,i_fld+5)
        end do
      end do
!$omp end parallel do
!
      end subroutine const_tensor_2_each_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_tensor_2_vec_each_ele(numnod, numele,            &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2,             &
     &          i_fld, nd, ntot_comp, d_nod, coef, vect_e)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vect_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = i_fld + l_sim_t(1,nd)
      n2 = i_fld + l_sim_t(2,nd)
      n3 = i_fld + l_sim_t(3,nd)
!
!$omp parallel do private(iele,inod,ist,ied)
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
!cdir nodep
        do iele = ist, ied
          inod = ie(iele,k2)
          vect_e(iele,1) = coef * d_nod(inod,n1)
          vect_e(iele,2) = coef * d_nod(inod,n2)
          vect_e(iele,3) = coef * d_nod(inod,n3)
        end do
      end do
!$omp end parallel do
!
      end subroutine const_tensor_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      subroutine const_as_tsr_2_vec_each_ele(numnod, numele,            &
     &          nnod_4_ele, ie, np_smp, iele_smp_stack, k2,             &
     &          i_fld, nd, ntot_comp, d_nod, coef, vect_e)
!
      use m_phys_constants
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: ntot_comp, i_fld
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vect_e(numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = i_fld + l_asim_t(nd,1,1)
      n2 = i_fld + l_asim_t(nd,2,1)
      n3 = i_fld + l_asim_t(nd,3,1)
!
!$omp parallel do private(iele,inod,ist,ied) 
      do iproc = 1, np_smp
        ist = iele_smp_stack(iproc-1) + 1
        ied = iele_smp_stack(iproc)
!cdir nodep
        do iele = ist, ied
          inod = ie(iele,k2)
          vect_e(iele,1) = dble(l_asim_t(nd,1,2))                       &
     &                     * coef * d_nod(inod,n1)
          vect_e(iele,2) = dble(l_asim_t(nd,2,2))                       &
     &                     * coef * d_nod(inod,n2)
          vect_e(iele,3) = dble(l_asim_t(nd,3,2))                       &
     &                     * coef * d_nod(inod,n3)
        end do
      end do
!$omp end parallel do
!
      end subroutine const_as_tsr_2_vec_each_ele
!
!  ---------------------------------------------------------------------
!
      end module nodal_cst_fld_each_element
