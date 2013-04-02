!
!     module set_off_diagonal
!
!      programmed by H.Matsui on July 2002
!      Modified by Hiroaki Matsui on Oct., 2006
!
!      subroutine s_set_off_diagonal(N, NP, NPL, NPU,                   &
!     &          INL, INU, IAL, IAU, nod1, nod2, mat_num)
!
      module set_off_diagonal
!
      use m_precision
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_off_diagonal(N, NP, NPL, NPU,                    &
     &          INL, INU, IAL, IAU, nod1, nod2, mat_num)
!
!
      integer (kind = kint), intent(in) :: N, NP
      integer (kind = kint), intent(in) :: NPL, NPU
      integer (kind = kint), intent(in) :: INL(0:NP), INU(0:NP)
      integer (kind = kint), intent(in) :: IAL(NPL),  IAU(NPU)
      integer (kind = kint), intent(in) :: nod1, nod2
!
      integer (kind = kint), intent(inout) :: mat_num
!
      integer (kind = kint) :: ik
      integer (kind = kint) :: ist, ied
!
!
      mat_num = 0
!
      if (nod1 .le. N) then
!
!     upper component!!
!
        if (nod2 .gt. nod1) then
!
          ist = INU(nod1-1)+1
          ied = INU(nod1)
          do ik = ist, ied
            if (IAU(ik) .eq. nod2) then
              mat_num = ik + NP + NPL
            end if
          end do
!
        end if
!
!     lower component!!
!
        if (nod2 .lt. nod1) then
!
          ist = INL(nod1-1)+1
          ied = INL(nod1)
          do ik = ist, ied
            if (IAL(ik) .eq. nod2) then
              mat_num = ik + NP
            end if
          end do
!
        end if
!
!
      end if
!
      if (nod1 .eq. nod2) then
       mat_num = nod1
      end if
!
      end subroutine s_set_off_diagonal
!
!-----------------------------------------------------------------------
!
      end module set_off_diagonal
