!gravity_scl_on_each_element.f90
!      module gravity_scl_on_each_element
!
!      Written by H. Matsui on July, 2005
!      Modified by H. Matsui on May, 2009
!
!      subroutine const_g_each_element                                  &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          nd, k2, i_field, ntot_comp, d_nod, grav, ak_buo, buo_e)
!      subroutine radial_g_each_element                                 &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          xx, a_radius, nd, k2, i_field, ntot_comp, d_nod,       &
!     &          ak_buo, buo_e)
!      subroutine self_g_each_element                                   &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          xx, nd, k2, i_field, ntot_comp, d_nod, ak_buo, buo_e)
!
!      subroutine const_double_g_each_element                           &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          nd, k2, i_src1, i_src2, ntot_comp, d_nod, grav,        &
!     &          ak_buo1, ak_buo2, buo_e)
!      subroutine radial_double_g_each_element                          &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          xx, a_radius, nd, k2, i_src1, i_src2, ntot_comp, d_nod,&
!     &          ak_buo1, ak_buo2, buo_e)
!      subroutine self_double_g_each_element                            &
!     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack,&
!     &          xx, nd, k2, i_src1, i_src2, ntot_comp, d_nod,          &
!     &          ak_buo1, ak_buo2, buo_e)
!
      module gravity_scl_on_each_element
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
      subroutine const_g_each_element                                   &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          nd, k2, i_field, ntot_comp, d_nod, grav, ak_buo, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd, k2, i_field
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo(numele)
      real (kind=kreal), intent(in) :: grav(3)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = ak_buo(iele)*grav(nd) * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine const_g_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine radial_g_each_element                                  &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          xx, a_radius, nd, k2, i_field, ntot_comp, d_nod,        &
     &          ak_buo, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: xx(numnod,3)
      real (kind=kreal), intent(in) :: a_radius(numnod)
!
      integer(kind = kint), intent(in) :: nd, k2, i_field
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo(numele)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = ak_buo(iele) * xx(inod,nd) * a_radius(inod)  &
     &                  * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine radial_g_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine self_g_each_element                                    &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          xx, nd, k2, i_field, ntot_comp, d_nod, ak_buo, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nd, k2, i_field
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo(numele)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = ak_buo(iele) * xx(inod,nd)                  &
     &                  * d_nod(inod,i_field)
         end do
       end do
!$omp end parallel do
!
      end subroutine self_g_each_element
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine const_double_g_each_element                            &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          nd, k2, i_src1, i_src2, ntot_comp, d_nod, grav,         &
     &          ak_buo1, ak_buo2, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
!
      integer(kind = kint), intent(in) :: nd, k2, i_src1, i_src2
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo1(numele), ak_buo2(numele)
      real (kind=kreal), intent(in) :: grav(3)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = (ak_buo1(iele) * d_nod(inod,i_src1)            &
     &                  + ak_buo2(iele) * d_nod(inod,i_src2))           &
     &                   * grav(nd)
         end do
       end do
!$omp end parallel do
!
      end subroutine const_double_g_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine radial_double_g_each_element                           &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          xx, a_radius, nd, k2, i_src1, i_src2, ntot_comp, d_nod, &
     &          ak_buo1, ak_buo2, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: xx(numnod,3)
      real (kind=kreal), intent(in) :: a_radius(numnod)
!
      integer(kind = kint), intent(in) :: nd, k2, i_src1, i_src2
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo1(numele), ak_buo2(numele)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = (ak_buo1(iele) * d_nod(inod,i_src1)            &
     &                  + ak_buo2(iele) * d_nod(inod,i_src2))           &
     &                   * xx(inod,nd) * a_radius(inod)
         end do
       end do
!$omp end parallel do
!
      end subroutine radial_double_g_each_element
!
!  ---------------------------------------------------------------------
!
      subroutine self_double_g_each_element                             &
     &         (numnod, numele, nnod_4_ele, ie, np_smp, iele_smp_stack, &
     &          xx, nd, k2, i_src1, i_src2, ntot_comp, d_nod,           &
     &          ak_buo1, ak_buo2, buo_e)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: iele_smp_stack(0:np_smp)
      real (kind=kreal), intent(in) :: xx(numnod,3)
!
      integer(kind = kint), intent(in) :: nd, k2, i_src1, i_src2
      integer(kind = kint), intent(in) :: ntot_comp
      real (kind=kreal), intent(in) :: d_nod(numnod,ntot_comp)
      real (kind=kreal), intent(in) :: ak_buo1(numele), ak_buo2(numele)
!
      real (kind=kreal), intent(inout) :: buo_e(numele)
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
           buo_e(iele) = (ak_buo1(iele) * d_nod(inod,i_src1)            &
     &                     + ak_buo2(iele) * d_nod(inod,i_src2))        &
     &                      * xx(inod,nd)
         end do
       end do
!$omp end parallel do
!
      end subroutine self_double_g_each_element
!
!  ---------------------------------------------------------------------
!
      end module gravity_scl_on_each_element
