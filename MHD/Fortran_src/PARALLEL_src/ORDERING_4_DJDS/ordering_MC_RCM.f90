!
!     module ordering_MC_RCM
!
!      Written by K. Nakajima in 2001
!        modified by H. Matsui on May. 2002
!        modified by H. Matsui on June. 2006
!        modified by H. Matsui on Jan., 2009
!
!!      subroutine check_dependency_RCM_MC(id_rank, NP,                 &
!!     &          NPL_mc, NPU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,       &
!!     &          NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!!      subroutine set_color_tbl_RCM_MC(NP, NHYP, NCOLORtot, IVECT_rcm, &
!!     &          IVECmc, ICHK, IVnew, IW)
!!      subroutine set_RCM_MC_table(N, NP, NCOLORtot, IVnew,            &
!!     &          NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      module ordering_MC_RCM
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_color_tbl_RCM_MC(NP, NHYP, NCOLORtot, IVECT_rcm,   &
     &          IVECmc, ICHK, IVnew, IW)
!
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NHYP
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVECT_rcm(0:NP)
!
      integer(kind = kint), intent(inout) :: IVECmc(0:NCOLORtot)
      integer(kind = kint), intent(inout) :: ICHK(NHYP)
      integer(kind = kint), intent(inout) :: IVnew(NP)
      integer(kind = kint), intent(inout) :: IW(NP)
!
      integer(kind = kint) :: i, k, ic1, ic2
      integer(kind = kint) :: icou, icolt
      integer(kind = kint) :: MCtot, icoug
!
!
      IW(1:NP) = 0
!
      do i= 1, NCOLORtot
        ICHK =  0
        icou =  i - NCOLORtot
        icolt = 0
        do k= 1, NHYP
          icou= icou + NCOLORtot
          if (icou.gt.NHYP) exit
          IW(i)= IW(i) + IVECT_rcm(icou)-IVECT_rcm(icou-1)
          icolt= icolt + 1
          ICHK(icolt)= icou
        enddo

        MCtot= icolt
        icoug= 0
        do ic1= 1, MCtot
          ic2= ICHK(ic1)
          do k= IVECT_rcm(ic2-1)+1, IVECT_rcm(ic2)
            icoug = icoug + 1
            IVnew(icoug+IVECmc(i-1))= k
          enddo
        enddo
        IVECmc(i)= IVECmc(i-1) + icoug
      enddo
!
      end subroutine set_color_tbl_RCM_MC
!
! ----------------------------------------------------------------------
!
      subroutine check_dependency_RCM_MC(id_rank, NP,                   &
     &          NPL_mc, NPU_mc, INL_mc, INU_mc, IAL_mc, IAU_mc,         &
     &          NCOLORtot, IVECmc, IVnew, IW, IFLAG)
!
      integer(kind = kint), intent(in) :: id_rank
      integer(kind = kint), intent(in) :: NP
      integer(kind = kint), intent(in) :: NPL_mc, NPU_mc
      integer(kind = kint), intent(in) :: INL_mc(0:NP)
      integer(kind = kint), intent(in) :: INU_mc(0:NP)
      integer(kind = kint), intent(in) :: IAL_mc(NPL_mc)
      integer(kind = kint), intent(in) :: IAU_mc(NPU_mc)
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVECmc(0:NCOLORtot)
      integer(kind = kint), intent(in) :: IVnew(NP)
!
      integer(kind = kint), intent(inout) :: IFLAG
      integer(kind = kint), intent(inout) :: IW(NP)
!
      integer(kind = kint) :: istart, iend, in
      integer(kind = kint) :: kst, ked, k, j, i, ip
!
!C
!C-- CHECK dependency
      IFLAG= 0
      do i= 1, NCOLORtot
        iStart= IVECmc(i-1) + 1
        iEnd=   IVECmc(i)
        IW= 0
        do j= iStart, iEnd
          in= IVnew(j)
          IW(in)= 1
        end do
        do j= iStart, iEnd
          in= IVnew(j)
!
          kst = INL_mc(in-1) + 1
          ked = INL_mc(in)
          do k = kst, ked
            ip= IAL_mc(k)
            if (IW(ip).eq.1) then
!              write(*,*) 'dep. for lower', id_rank, in, k, ip
              IFLAG= 1
              exit
            end if
          enddo
!
          kst = INU_mc(in-1) + 1
          ked = INU_mc(in)
          do k = kst, ked
            ip= IAU_mc(k)
            if (IW(ip).eq.1) then
!              write(*,*) 'dep. for upper', id_rank, in, k, ip
              IFLAG= 1
              exit
            end if
          enddo
          if ( IFLAG .eq. 1) exit
        enddo
        if ( IFLAG .eq. 1) exit
      enddo
!
      end subroutine check_dependency_RCM_MC
!
! ----------------------------------------------------------------------
!
      subroutine set_RCM_MC_table(N, NP, NCOLORtot, IVnew,              &
     &          NHYP, OLDtoNEWmc, NEWtoOLDmc)
!
      integer(kind = kint), intent(in) :: NP, N
      integer(kind = kint), intent(in) :: NCOLORtot
      integer(kind = kint), intent(in) :: IVnew(NP)
!
      integer(kind = kint), intent(inout) :: NHYP
      integer(kind = kint), intent(inout) :: OLDtoNEWmc(NP)
      integer(kind = kint), intent(inout) :: NEWtoOLDmc(NP)
!
      integer(kind = kint) :: i, in
!
!
!CDIR NOVECTOR
        do i= 1, N
          in= IVnew(i)
          OLDtoNEWmc(in)= i
        enddo

!CDIR NOVECTOR
        do i= 1, N
          in= OLDtoNEWmc(i)
          NEWtoOLDmc(in)= i
        enddo

!CDIR NODEP
        do i= N+1, NP
          OLDtoNEWmc(i)= i
          NEWtoOLDmc(i)= i
        enddo
!C
!C-- TRANSFER
        NHYP= NCOLORtot
!
      end subroutine set_RCM_MC_table
!
! ----------------------------------------------------------------------
!
      end module ordering_MC_RCM
