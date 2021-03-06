!cal_skv_to_ff_vector_smp
!     module cal_skv_to_ff_vector_smp
!
!> @brief RHS vector assembling from element integration
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!      subroutine reset_skv_vector(numele, nnod_4_ele, numdir, sk_v)
!
!      subroutine add_skv_vector_2_ff_smp(numele, nnod_4_ele,           &
!     &          np_smp, maxnod_4_smp, inod_ele_max,                    &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, ff_v_smp, sk_v)
!      subroutine add_skv_vector_coef_2_ff_smp(numele, nnod_4_ele,      &
!     &          np_smp, maxnod_4_smp, inod_ele_max,                    &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
!      subroutine sub_skv_vector_2_ff_smp(numele, nnod_4_ele,           &
!     &          np_smp, maxnod_4_smp, inod_ele_max,                    &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, ff_v_smp, sk_v)
!      subroutine sub_skv_vector_coef_2_ff_smp(numele, nnod_4_ele,      &
!     &          np_smp, maxnod_4_smp, inod_ele_max,                    &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      module cal_skv_to_ff_vector_smp
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine reset_skv_vector(numele, nnod_4_ele, numdir, sk_v)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele, numdir
      real (kind=kreal), intent(inout)  :: sk_v(numele,6,nnod_4_ele)
!
      integer(kind = kint) :: iele, k1, nd
!
!
!$omp parallel private(iele,nd)
      do k1 = 1, nnod_4_ele
        do nd = 1, numdir
!$omp do
          do iele = 1, numele
             sk_v(iele,nd,k1) = 0.0d0
          end do
!$omp end do nowait
        end do
      end do
!$omp end parallel
!
      end subroutine reset_skv_vector
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_skv_vector_2_ff_smp(numele, nnod_4_ele,            &
     &          np_smp, maxnod_4_smp, inod_ele_max,                     &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &            :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
!
      real (kind=kreal), intent(in)  :: sk_v(numele,6,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_v_smp(inod,1,ip) = ff_v_smp(inod,1,ip)                   &
     &                              + sk_v(iele,1,iconn)
            ff_v_smp(inod,2,ip) = ff_v_smp(inod,2,ip)                   &
     &                              + sk_v(iele,2,iconn)
            ff_v_smp(inod,3,ip) = ff_v_smp(inod,3,ip)                   &
     &                              + sk_v(iele,3,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine add_skv_vector_2_ff_smp
!
! ----------------------------------------------------------------------
!
      subroutine add_skv_vector_coef_2_ff_smp(numele, nnod_4_ele,       &
     &          np_smp, maxnod_4_smp, inod_ele_max,                     &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &            :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)  :: sk_v(numele,6,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_v_smp(inod,1,ip) = ff_v_smp(inod,1,ip)                   &
     &                              + coef * sk_v(iele,1,iconn)
            ff_v_smp(inod,2,ip) = ff_v_smp(inod,2,ip)                   &
     &                              + coef * sk_v(iele,2,iconn)
            ff_v_smp(inod,3,ip) = ff_v_smp(inod,3,ip)                   &
     &                              + coef * sk_v(iele,3,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine add_skv_vector_coef_2_ff_smp
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine sub_skv_vector_2_ff_smp(numele, nnod_4_ele,            &
     &          np_smp, maxnod_4_smp, inod_ele_max,                     &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &            :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
!
      real (kind=kreal), intent(in)  :: sk_v(numele,6,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_v_smp(inod,1,ip) = ff_v_smp(inod,1,ip)                   &
     &                              - sk_v(iele,1,iconn)
            ff_v_smp(inod,2,ip) = ff_v_smp(inod,2,ip)                   &
     &                              - sk_v(iele,2,iconn)
            ff_v_smp(inod,3,ip) = ff_v_smp(inod,3,ip)                   &
     &                              - sk_v(iele,3,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sub_skv_vector_2_ff_smp
!
! ----------------------------------------------------------------------
!
      subroutine sub_skv_vector_coef_2_ff_smp(numele, nnod_4_ele,       &
     &          np_smp, maxnod_4_smp, inod_ele_max,                     &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numele, nnod_4_ele
      integer(kind = kint), intent(in) :: np_smp
      integer(kind = kint), intent(in) :: maxnod_4_smp
      integer(kind = kint), intent(in) :: inod_ele_max, num_sort_smp
      integer(kind = kint), intent(in)                                  &
     &            :: nod_stack_smp(0:inod_ele_max*np_smp)
      integer(kind = kint), intent(in) :: iele_sort_smp(num_sort_smp)
      integer(kind = kint), intent(in) :: iconn_sort_smp(num_sort_smp)
!
      real (kind=kreal), intent(in) :: coef
      real (kind=kreal), intent(in)  :: sk_v(numele,6,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
!
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_v_smp(inod,1,ip) = ff_v_smp(inod,1,ip)                   &
     &                              - coef * sk_v(iele,1,iconn)
            ff_v_smp(inod,2,ip) = ff_v_smp(inod,2,ip)                   &
     &                              - coef * sk_v(iele,2,iconn)
            ff_v_smp(inod,3,ip) = ff_v_smp(inod,3,ip)                   &
     &                              - coef * sk_v(iele,3,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine sub_skv_vector_coef_2_ff_smp
!
! ----------------------------------------------------------------------
!
      end module cal_skv_to_ff_vector_smp
