!
!      module expand_near_element
!
!      Written by H. Matsui on Oct., 2006
!
!      subroutine allocate_imark_4_ele(np_smp, numele)
!      subroutine deallocate_imark_4_ele
!
!      subroutine count_expanded_near_element(np_smp,                   &
!     &          numnod, numele, inod_smp_stack, iflag_expand,          &
!     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,       &
!     &          ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod, &
!     &          ntot_ele_near_nod, nele_near_nod, iele_stack_near_nod, &
!     &          iele_near_nod, nele_near_nod_w)
!      subroutine set_expanded_near_element(np_smp,                     &
!     &          numnod, numele, inod_smp_stack, iflag_expand,          &
!     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,       &
!     &          ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod, &
!     &          ntot_ele_near_nod, nele_near_nod, iele_stack_near_nod, &
!     &          iele_near_nod, ntot_ele_near_nod_w, nele_near_nod_w,   &
!     &          iele_stack_near_nod_w, iele_near_nod_w)
!
      module expand_near_element
!
      use m_precision
!
      implicit none
!
      integer(kind= kint), allocatable :: imark_4_ele(:,:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_imark_4_ele(np_smp, numele)
!
      integer(kind = kint), intent(in) :: numele, np_smp
!
!
      allocate(imark_4_ele(numele,np_smp))
      imark_4_ele = 0
!
      end subroutine allocate_imark_4_ele
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_imark_4_ele
!
      deallocate(imark_4_ele)
!
      end subroutine deallocate_imark_4_ele
!
! -----------------------------------------------------------------------
!
      subroutine count_expanded_near_element(np_smp,                    &
     &          numnod, numele, inod_smp_stack, iflag_expand,           &
     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,        &
     &          ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,  &
     &          ntot_ele_near_nod, nele_near_nod, iele_stack_near_nod,  &
     &          iele_near_nod, nele_near_nod_w)
!
      integer(kind = kint), intent(in) :: np_smp, numnod, numele
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iflag_expand(numnod)
!
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele_4_node)
!
      integer(kind = kint), intent(in) :: ntot_nod_near_nod
      integer(kind = kint), intent(in) :: inod_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in) :: inod_near_nod(0:numnod)
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: nele_near_nod(numnod)
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &                       :: iele_near_nod(ntot_ele_near_nod)
!
      integer(kind = kint), intent(inout) :: nele_near_nod_w(numnod)
!
      integer(kind = kint) :: ip, ist_smp, ied_smp
      integer(kind = kint) :: inod, jnod, jele
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum
!
!
      nele_near_nod_w(1:numnod) = 0
!$omp parallel do private(ist_smp,ied_smp,inod,ist,ied,inum,            &
!$omp&                    jst,jed,jnum,jele)
      do ip = 1, np_smp
!
        ist_smp = inod_smp_stack(ip-1) + 1
        ied_smp = inod_smp_stack(ip)
        do inod = ist_smp, ied_smp
!
          nele_near_nod_w(inod) = nele_near_nod(inod)
!
          if ( iflag_expand(inod) .eq. 1 ) then
!
            imark_4_ele(1:numele,ip) = 0
!
            jst = iele_stack_near_nod(inod-1) + 1
            jed = iele_stack_near_nod(inod)
            do jnum = jst, jed
              jele = iele_near_nod(jnum)
              imark_4_ele(jele,ip) = inod
            end do
!
            ist = inod_stack_near_nod(inod-1) + 1
            ied = inod_stack_near_nod(inod)
            do inum = ist, ied
!
              jnod = inod_near_nod(inum)
              jst = iele_stack_4_node(jnod-1) + 1
              jed = iele_stack_4_node(jnod)
              do jnum = jst, jed
!
                jele = iele_4_node(jnum)
                if ( imark_4_ele(jele,ip) .eq. 0) then
                  imark_4_ele(jele,ip) = 1
                  nele_near_nod_w(inod) = nele_near_nod_w(inod) + 1
                end if
              end do
!
            end do
          end if
!
        end do
      end do
!$omp end parallel do
!
      end subroutine count_expanded_near_element
!
! ----------------------------------------------------------------------
!
      subroutine set_expanded_near_element(np_smp,                      &
     &          numnod, numele, inod_smp_stack, iflag_expand,           &
     &          ntot_ele_4_node, iele_stack_4_node, iele_4_node,        &
     &          ntot_nod_near_nod, inod_stack_near_nod, inod_near_nod,  &
     &          ntot_ele_near_nod, nele_near_nod, iele_stack_near_nod,  &
     &          iele_near_nod, ntot_ele_near_nod_w, nele_near_nod_w,    &
     &          iele_stack_near_nod_w, iele_near_nod_w)
!
      use quicksort
!
      integer(kind = kint), intent(in) :: np_smp, numnod, numele
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      integer(kind = kint), intent(in) :: iflag_expand(numnod)
!
      integer(kind = kint), intent(in) :: ntot_ele_4_node
      integer(kind = kint), intent(in) :: iele_stack_4_node(0:numnod)
      integer(kind = kint), intent(in) :: iele_4_node(ntot_ele_4_node)
!
      integer(kind = kint), intent(in) :: ntot_nod_near_nod
      integer(kind = kint), intent(in) :: inod_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in) :: inod_near_nod(0:numnod)
!
      integer(kind = kint), intent(in) :: ntot_ele_near_nod
      integer(kind = kint), intent(in) :: nele_near_nod(numnod)
      integer(kind = kint), intent(in) :: iele_stack_near_nod(0:numnod)
      integer(kind = kint), intent(in)                                  &
     &                       :: iele_near_nod(ntot_ele_near_nod)
      integer(kind = kint), intent(in) :: ntot_ele_near_nod_w
      integer(kind = kint), intent(in)                                  &
     &                       :: iele_stack_near_nod_w(0:numnod)
!
      integer(kind = kint), intent(inout) :: nele_near_nod_w(numnod)
      integer(kind = kint), intent(inout)                               &
     &                      :: iele_near_nod_w(ntot_ele_near_nod_w)
!
      integer(kind = kint) :: ip, ist_smp, ied_smp
      integer(kind = kint) :: inod, jnod, jele, icou
      integer(kind = kint) :: ist, ied, inum
      integer(kind = kint) :: jst, jed, jnum
!
!
      nele_near_nod_w(1:numnod) = 0
!$omp parallel do private(ist_smp,ied_smp,inod,icou,ist,ied,inum,       &
!$omp&                    jst,jed,jnum,jele)
      do ip = 1, np_smp
!
        ist_smp = inod_smp_stack(ip-1) + 1
        ied_smp = inod_smp_stack(ip)
        do inod = ist_smp, ied_smp
!
          imark_4_ele(1:numele,ip) = 0
!
          jst = iele_stack_near_nod(inod-1) + 1
          jed = iele_stack_near_nod(inod)
          do jnum = jst, jed
            jele = iele_near_nod(jnum)
            imark_4_ele(jele,ip) = inod
            nele_near_nod_w(inod) = nele_near_nod_w(inod) + 1
            icou = iele_stack_near_nod_w(inod-1)                        &
     &              + nele_near_nod_w(inod)
            iele_near_nod_w(icou) = jele
          end do
!
          if ( iflag_expand(inod) .eq. 1 ) then
            ist = inod_stack_near_nod(inod-1) + 1
            ied = inod_stack_near_nod(inod)
            do inum = ist, ied
!
              jnod = inod_near_nod(inum)
              jst = iele_stack_4_node(jnod-1) + 1
              jed = iele_stack_4_node(jnod)
              do jnum = jst, jed
!
                jele = iele_4_node(jnum)
                if ( imark_4_ele(jele,ip) .eq. 0) then
                  imark_4_ele(jele,ip) = 1
                  nele_near_nod_w(inod) = nele_near_nod_w(inod) + 1
                  icou = iele_stack_near_nod_w(inod-1)                  &
     &                  + nele_near_nod_w(inod)
                  iele_near_nod_w(icou) = jele
                end if
              end do
!
            end do
          end if
!
        end do
      end do
!$omp end parallel do
!
      do inod = 1, numnod
        ist = iele_stack_near_nod_w(inod-1) + nele_near_nod(inod) + 1
        ied = iele_stack_near_nod_w(inod)
        call quicksort_int(ntot_ele_near_nod_w, iele_near_nod_w,        &
     &      ist, ied)
      end do
!
      end subroutine set_expanded_near_element
!
! ----------------------------------------------------------------------
!
      end module expand_near_element
