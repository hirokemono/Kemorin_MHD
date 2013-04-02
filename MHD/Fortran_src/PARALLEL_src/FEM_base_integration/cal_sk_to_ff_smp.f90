!cal_sk_to_ff_smp
!     module cal_sk_to_ff_smp
!
!> @brief RHS vector assembling from element integration
!
!     Written by H. Matsui on June, 2005
!     Modified by H. Matsui on March, 2009
!     Modified by H. Matsui on March, 2012
!
!      subroutine cal_sk_2_ff_smp (numele, nnod_4_ele, np_smp,          &
!     &          maxnod_4_smp, inod_ele_max, num_sort_smp,              &
!     &          nod_stack_smp, iele_sort_smp, iconn_sort_smp,          &
!     &          nd, ff_smp, sk_s)
!      subroutine cal_sk_2_ff_smp_w_coef(numele, nnod_4_ele,            &
!     &          np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,      &
!     &          nod_stack_smp, iele_sort_smp, iconn_sort_smp, nd,      &
!     &          coef, ff_smp, sk_s)
!
!      subroutine cal_sk_vect_2_ff_smp(numdir, numele,                  &
!     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,        &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, ff_v_smp, sk_v)
!      subroutine cal_sk_vect_2_ff_smp_w_coef(numdir, numele,           &
!     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,        &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
!      subroutine subtract_skv_2_ff_smp(numdir, numele,                 &
!     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,        &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, ff_v_smp, sk_v)
!      subroutine subtract_skv_2_ff_smp_w_coef(numdir, numele,          &
!     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,        &
!     &          num_sort_smp, nod_stack_smp, iele_sort_smp,            &
!     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      module cal_sk_to_ff_smp
!
      use m_precision
!
      use m_phys_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
       contains
!
! ----------------------------------------------------------------------
!
      subroutine cal_sk_2_ff_smp (numele, nnod_4_ele, np_smp,           &
     &          maxnod_4_smp, inod_ele_max, num_sort_smp,               &
     &          nod_stack_smp, iele_sort_smp, iconn_sort_smp,           &
     &          nd, ff_smp, sk_s)
!
      integer(kind = kint), intent(in) :: nd
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
      real (kind=kreal), intent(in)    :: sk_s(numele,nnod_4_ele)
      real (kind=kreal), intent(inout) :: ff_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_smp(inod,nd,ip) = ff_smp(inod,nd,ip) + sk_s(iele,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sk_2_ff_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_sk_2_ff_smp_w_coef(numele, nnod_4_ele,             &
     &          np_smp, maxnod_4_smp, inod_ele_max, num_sort_smp,       &
     &          nod_stack_smp, iele_sort_smp, iconn_sort_smp, nd,       &
     &          coef, ff_smp, sk_s)
!
      integer(kind = kint), intent(in) :: nd
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
      real (kind=kreal), intent(in)    :: coef
      real (kind=kreal), intent(in)    :: sk_s(numele,nnod_4_ele)
      real (kind=kreal), intent(inout) :: ff_smp(maxnod_4_smp,3,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do inum = 1, inod_ele_max
          inn = inum + inod_ele_max*(ip-1)
          ist = nod_stack_smp(inn-1)+1
          ied = nod_stack_smp(inn)
!
!cdir nodep
          do in = ist, ied
            inod = in - nod_stack_smp(inn-1)
            iele = iele_sort_smp(in)
            iconn = iconn_sort_smp(in)
!
            ff_smp(inod,nd,ip) = ff_smp(inod,nd,ip)                     &
     &                          + coef * sk_s(iele,iconn)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sk_2_ff_smp_w_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_sk_vect_2_ff_smp(numdir, numele,                   &
     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,         &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numdir
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
      real (kind=kreal), intent(in)                                     &
     &             :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum, nd
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(nd,inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do nd = 1,  numdir
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(ip-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!cdir nodep
            do in = ist, ied
!
              inod = in - nod_stack_smp(inn-1)
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              ff_v_smp(inod,nd,ip) = ff_v_smp(inod,nd,ip)               &
     &                            + sk_v(iele,nd,iconn)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sk_vect_2_ff_smp
!
! ----------------------------------------------------------------------
!
      subroutine cal_sk_vect_2_ff_smp_w_coef(numdir, numele,            &
     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,         &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numdir
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
      real (kind=kreal), intent(in)                                     &
     &             :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum, nd
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(nd,inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do nd = 1,  numdir
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(ip-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!cdir nodep
            do in = ist, ied
!
              inod = in - nod_stack_smp(inn-1)
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              ff_v_smp(inod,nd,ip) = ff_v_smp(inod,nd,ip)               &
     &                            + coef * sk_v(iele,nd,iconn)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine cal_sk_vect_2_ff_smp_w_coef
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine subtract_skv_2_ff_smp(numdir, numele,                  &
     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,         &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numdir
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
      real (kind=kreal), intent(in)                                     &
     &             :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum, nd
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(nd,inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do nd = 1,  numdir
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(ip-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!cdir nodep
            do in = ist, ied
!
              inod = in - nod_stack_smp(inn-1)
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              ff_v_smp(inod,nd,ip) = ff_v_smp(inod,nd,ip)               &
     &                              - sk_v(iele,nd,iconn)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine subtract_skv_2_ff_smp
!
! ----------------------------------------------------------------------
!
      subroutine subtract_skv_2_ff_smp_w_coef(numdir, numele,           &
     &          nnod_4_ele, np_smp, maxnod_4_smp, inod_ele_max,         &
     &          num_sort_smp, nod_stack_smp, iele_sort_smp,             &
     &          iconn_sort_smp, coef, ff_v_smp, sk_v)
!
      integer(kind = kint), intent(in) :: numdir
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
      real (kind=kreal), intent(in)                                     &
     &             :: sk_v(numele,n_sym_tensor,nnod_4_ele)
!
      real (kind=kreal), intent(inout)                                  &
     &             :: ff_v_smp(maxnod_4_smp,n_vector,np_smp)
!
      integer(kind = kint) :: ip, inod, iele, inum, nd
      integer(kind = kint) :: inn, iconn, ist, ied, in
!
!
!$omp parallel do private(nd,inod,inum,iele,iconn,inn,in,ist,ied) 
      do ip = 1, np_smp
        do nd = 1,  numdir
          do inum = 1, inod_ele_max
!
            inn = inum + inod_ele_max*(ip-1)
            ist = nod_stack_smp(inn-1)+1
            ied = nod_stack_smp(inn)
!cdir nodep
            do in = ist, ied
!
              inod = in - nod_stack_smp(inn-1)
              iele = iele_sort_smp(in)
              iconn = iconn_sort_smp(in)
!
              ff_v_smp(inod,nd,ip) = ff_v_smp(inod,nd,ip)               &
     &                              - coef * sk_v(iele,nd,iconn)
            end do
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine subtract_skv_2_ff_smp_w_coef
!
! ----------------------------------------------------------------------
!
      end module cal_sk_to_ff_smp
