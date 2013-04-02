!
!      module set_nodal_2_each_element
!
!      Written by H. Matsui on July, 2005
!
!      subroutine position_to_local_ele(numnod, numele, nnod_4_ele,     &
!     &          ie, np_smp, iele_smp_stack, k2, xx, radius,            &
!     &          xe, radius_e)
!
!      subroutine scalar_to_local_ele(numnod, numele, nnod_4_ele,       &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld,                 &
!     &          ntot_comp, d_nod, scalar_e)
!      subroutine vector_to_local_ele(numnod, numele, nnod_4_ele,       &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld,                 &
!     &          ntot_comp, d_nod, vect_e)
!      subroutine tensor_to_local_ele(numnod, numele, nnod_4_ele,       &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld,                 &
!     &          ntot_comp, d_nod, tensor_e)
!
!      subroutine tensor_to_local_ele_v(numnod, numele, nnod_4_ele,     &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld, nd, l_sim_t,    &
!     &          ntot_comp, d_nod, vect_e)
!      subroutine as_tensor_to_local_ele_v(numnod, numele, nnod_4_ele,  &
!     &          ie, np_smp, iele_smp_stack, k2, i_fld, nd, l_asim_t,   &
!     &          ntot_comp, d_nod, vect_e)
!
      module set_nodal_2_each_element
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
      subroutine position_to_local_ele(numnod, numele, nnod_4_ele,      &
     &          ie, np_smp, iele_smp_stack, k2, xx, radius,             &
     &          xe, radius_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: xx(numnod,3)
      real (kind=kreal), intent(in) :: radius(numnod)
!
      real (kind=kreal), intent(inout) :: xe(numele,3)
      real (kind=kreal), intent(inout) :: radius_e(numele)
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
          xe(iele,1) = xx(inod,1)
          xe(iele,2) = xx(inod,2)
          xe(iele,3) = xx(inod,3)
          radius_e(iele) = radius(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine position_to_local_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine scalar_to_local_ele(numnod, numele, nnod_4_ele,        &
     &          ie, np_smp, iele_smp_stack, k2, i_fld,                  &
     &          ntot_comp, d_nod, scalar_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, i_fld
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
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
!
           scalar_e(iele) = d_nod(inod,i_fld)
        end do
      end do
!$omp end parallel do
!
      end subroutine scalar_to_local_ele
!
!  ---------------------------------------------------------------------
!
      subroutine vector_to_local_ele(numnod, numele, nnod_4_ele,        &
     &          ie, np_smp, iele_smp_stack, k2, i_fld,                  &
     &          ntot_comp, d_nod, vect_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, i_fld
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(inout) :: vect_e(numele,3)
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
          vect_e(iele,1) = d_nod(inod,i_fld  )
          vect_e(iele,2) = d_nod(inod,i_fld+1)
          vect_e(iele,3) = d_nod(inod,i_fld+2)
        end do
      end do
!$omp end parallel do
!
      end subroutine vector_to_local_ele
!
!  ---------------------------------------------------------------------
!
      subroutine tensor_to_local_ele(numnod, numele, nnod_4_ele,        &
     &          ie, np_smp, iele_smp_stack, k2, i_fld,                  &
     &          ntot_comp, d_nod, tensor_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, i_fld
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
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
          tensor_e(iele,1) = d_nod(inod,i_fld  )
          tensor_e(iele,2) = d_nod(inod,i_fld+1)
          tensor_e(iele,3) = d_nod(inod,i_fld+2)
          tensor_e(iele,4) = d_nod(inod,i_fld+3)
          tensor_e(iele,5) = d_nod(inod,i_fld+4)
          tensor_e(iele,6) = d_nod(inod,i_fld+5)
        end do
      end do
!$omp end parallel do
!
      end subroutine tensor_to_local_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine tensor_to_local_ele_v(numnod, numele, nnod_4_ele,      &
     &          ie, np_smp, iele_smp_stack, k2, i_fld, nd, l_sim_t,     &
     &          ntot_comp, d_nod, vect_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      integer(kind = kint), intent(in) :: l_sim_t(3,3)
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
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
          vect_e(iele,1) = d_nod(inod,n1)
          vect_e(iele,2) = d_nod(inod,n2)
          vect_e(iele,3) = d_nod(inod,n3)
        end do
      end do
!$omp end parallel do
!
      end subroutine tensor_to_local_ele_v
!
!  ---------------------------------------------------------------------
!
      subroutine as_tensor_to_local_ele_v(numnod, numele, nnod_4_ele,   &
     &          ie, np_smp, iele_smp_stack, k2, i_fld, nd, l_asim_t,    &
     &          ntot_comp, d_nod, vect_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: ntot_comp
      integer(kind = kint), intent(in) :: k2, nd, i_fld
      integer(kind = kint), intent(in) :: l_asim_t(3,3,2)
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
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
          vect_e(iele,1) = dble(l_asim_t(nd,1,2)) * d_nod(inod,n1)
          vect_e(iele,2) = dble(l_asim_t(nd,2,2)) * d_nod(inod,n2)
          vect_e(iele,3) = dble(l_asim_t(nd,3,2)) * d_nod(inod,n3)
        end do
      end do
!$omp end parallel do
!
      end subroutine as_tensor_to_local_ele_v
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_2_each_element
