!set_nodal_each_ele_w_const.f90
!      module set_nodal_each_ele_w_const
!
!      Written by H. Matsui on Nov., 2009
!
!      subroutine set_vector_2_each_ele_cnst(k2, vect_e, d_nod, coef)
!      subroutine set_scalar_2_each_ele_cnst(k2, scalar_e, d_nod, coef)
!      subroutine set_tensor_2_vec_each_ele_cnst(k2, nd, l_sim_t,       &
!     &          vect_e, d_nod, coef)
!      subroutine set_as_tsr_2_vec_each_ele_cnst(k2, nd, l_asim_t,      &
!     &          vect_e, d_nod, coef)
!
      module set_nodal_each_ele_w_const
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_geometry_data
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_vector_2_each_ele_cnst(k2, vect_e, d_nod, coef)
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: d_nod(node1%numnod,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
!
!
!$omp parallel do private(iele,inod,ist,ied) 
      do iproc = 1, np_smp
        ist = ele1%istack_ele_smp(iproc-1) + 1
        ied = ele1%istack_ele_smp(iproc)
!cdir nodep
        do iele = ist, ied
          inod = ie(iele,k2)
!
          vect_e(iele,1) = coef * d_nod(inod,1)
          vect_e(iele,2) = coef * d_nod(inod,2)
          vect_e(iele,3) = coef * d_nod(inod,3)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_vector_2_each_ele_cnst
!
!  ---------------------------------------------------------------------
!
      subroutine set_scalar_2_each_ele_cnst(k2, scalar_e, d_nod, coef)
!
      integer(kind = kint), intent(in) :: k2
      real (kind=kreal), intent(in) :: d_nod(node1%numnod)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: scalar_e(ele1%numele)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
!
!
!$omp parallel do private(iele,ist,ied) 
      do iproc = 1, np_smp
        ist = ele1%istack_ele_smp(iproc-1) + 1
        ied = ele1%istack_ele_smp(iproc)
!cdir nodep
        do iele = ist, ied
           inod = ie(iele,k2)
           scalar_e(iele) = coef * d_nod(inod)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_scalar_2_each_ele_cnst
!
!  ---------------------------------------------------------------------
!
      subroutine set_tensor_2_vec_each_ele_cnst(k2, nd, l_sim_t,        &
     &          vect_e, d_nod, coef)
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: l_sim_t(3,3)
      real (kind=kreal), intent(in) :: d_nod(node1%numnod,6)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = ione + l_sim_t(1,nd)
      n2 = ione + l_sim_t(2,nd)
      n3 = ione + l_sim_t(3,nd)
!
!$omp parallel do private(iele,inod,ist,ied)
      do iproc = 1, np_smp
        ist = ele1%istack_ele_smp(iproc-1) + 1
        ied = ele1%istack_ele_smp(iproc)
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
      end subroutine set_tensor_2_vec_each_ele_cnst
!
!  ---------------------------------------------------------------------
!
      subroutine set_as_tsr_2_vec_each_ele_cnst(k2, nd, l_asim_t,       &
     &          vect_e, d_nod, coef)
!
      integer(kind = kint), intent(in) :: k2, nd
      integer(kind = kint), intent(in) :: l_asim_t(3,3,2)
      real (kind=kreal), intent(in) :: d_nod(node1%numnod,3)
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(inout) :: vect_e(ele1%numele,3)
!
      integer(kind = kint) :: iproc, inod, iele, ist, ied
      integer(kind = kint) :: n1, n2, n3
!
!
      n1 = ione + l_asim_t(nd,1,1)
      n2 = ione + l_asim_t(nd,2,1)
      n3 = ione + l_asim_t(nd,3,1)
!
!$omp parallel do private(iele,inod,ist,ied) 
      do iproc = 1, np_smp
        ist = ele1%istack_ele_smp(iproc-1) + 1
        ied = ele1%istack_ele_smp(iproc)
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
      end subroutine set_as_tsr_2_vec_each_ele_cnst
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_each_ele_w_const
