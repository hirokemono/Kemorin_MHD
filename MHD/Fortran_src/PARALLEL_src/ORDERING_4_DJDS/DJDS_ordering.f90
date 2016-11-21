!>@file   DJDS_ordering.f90
!!@brief  module DJDS_ordering
!!
!!@author K. Nakajima and H. Matsui
!!@date      Written by K. Nakajima in 2001
!!@n        modified by H. Matsui in May. 2002
!!@n        modified by H. Matsui in June. 2006
!!@n        modified by H. Matsui in Jan., 2008
!
!>      DJDS ordering from RCM ordered matrix
!!
!!@verbatim
!!      subroutine set_djds_ordering(np_smp, NP, N, inter_smp_stack,    &
!!     &     NHYP, IVECT, STACKmcG, STACKmc, PEon, npLX1, npUX1,        &
!!     &     NLmax, NUmax, NLmaxHYP, NUmaxHYP, indexDJDS_L, indexDJDS_U,&
!!     &     NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                          &
!!     &     OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U)
!!@endverbatim
!
      module DJDS_ordering
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
      subroutine set_djds_ordering(np_smp, NP, N, inter_smp_stack,      &
     &      NHYP, IVECT, STACKmcG, STACKmc, PEon, npLX1, npUX1,         &
     &      NLmax, NUmax, NLmaxHYP, NUmaxHYP, indexDJDS_L, indexDJDS_U, &
     &      NEWtoOLD_DJDS_L, NEWtoOLD_DJDS_U,                           &
     &      OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,                           &
     &      INLmc, INUmc, IWKX, IW, inumDJDS_L, inumDJDS_U)
!
      use mat_conect_SORT
!
      integer (kind = kint), intent(in) :: np_smp
      integer (kind = kint), intent(in) :: NP, N
      integer (kind = kint), intent(in) :: inter_smp_stack(0:np_smp)
!
      integer (kind = kint), intent(in) :: NHYP
      integer (kind = kint), intent(in) :: NLmax, NUmax
      integer (kind = kint), intent(in) :: npLX1, npUX1
      integer (kind = kint), intent(in) :: IVECT(0:NHYP)
      integer (kind = kint), intent(in) :: NLmaxHYP(NHYP)
      integer (kind = kint), intent(in) :: NUmaxHYP(NHYP)
!
      integer (kind = kint), intent(inout) :: STACKmcG(0:np_smp)
      integer (kind = kint), intent(inout) :: STACKmc(0:np_smp*NHYP)
      integer (kind = kint), intent(inout) :: PEon(NP)
      integer (kind = kint), intent(inout) :: NEWtoOLD_DJDS_L(NP)
      integer (kind = kint), intent(inout) :: NEWtoOLD_DJDS_U(NP)
      integer (kind = kint), intent(inout) :: OLDtoNEW_DJDS_L(NP)
      integer (kind = kint), intent(inout) :: OLDtoNEW_DJDS_U(NP)
      integer (kind = kint), intent(inout)                              &
     &       :: indexDJDS_L(0:np_smp*NLmax*NHYP)
      integer (kind = kint), intent(inout)                              &
     &       :: indexDJDS_U(0:np_smp*NUmax*NHYP)
!
      integer(kind=kint), intent(in) :: INLmc(NP), INUmc(NP)
      integer(kind=kint), intent(inout) :: IWKX(0:NP,3)
      integer(kind=kint), intent(inout) :: IW(NP)
      integer(kind=kint), intent(inout) :: inumDJDS_L(0:npLX1*NHYP)
      integer(kind=kint), intent(inout) :: inumDJDS_U(0:npUX1*NHYP)
!
      integer (kind = kint), allocatable :: IW0(:,:)
!
      integer (kind = kint) :: NCmax
      integer (kind = kint) :: i, j, k, kk, ic2, icc, iv, iC, iM
      integer (kind = kint) :: ip, icou, nn, in, inC, iStart, iEnd
!
!
      STACKmcG(0:np_smp) = inter_smp_stack(0:np_smp)
!
      do i= N+1, NP
        NEWtoOLD_DJDS_L(i)= i
        NEWtoOLD_DJDS_U(i)= i
        OLDtoNEW_DJDS_L(i)= i
        OLDtoNEW_DJDS_U(i)= i
      end do

      STACKmc= 0
      PEon   = 0

      icc= 0
      do iv= 1, NHYP
        do ip= 1, np_smp
          icou= ip - np_smp + IVECT(iv-1)
          ic2 = 0
!voption novec
!cdir novector
          do k= IVECT(iv-1)+1, IVECT(iv)
            icou = icou + np_smp
            if (icou.gt.IVECT(iv)) exit
            ic2= ic2  + 1
            icc= icc  + 1
            STACKmc((iv-1)*np_smp+ ip) = ic2
            PEon(icc)= ip
          enddo
        enddo
      enddo
!
!C
!C== LOWER part
      do iv= 1, NHYP
        nn= IVECT(iv) - IVECT(iv-1)
        allocate (IW0(nn,2))
        NCmax= -NP
!
!       write(*,*) 'iv, ivect(iv)', iv, ivect(iv)
!
!CDIR NOVECTOR
        do k= IVECT(iv-1)+1, IVECT(iv)
          kk= k - IVECT(iv-1)
          IWKX(kk,1)= INLmc(k)
          IWKX(kk,2)= 0
          IW0 (kk,1)= INLmc(k)
          IW0 (kk,2)= 0
          NCmax     = max(NCmax,INLmc(k))
        enddo
!
!        write(*,*) 'NCmax', ncmax
!
        if (NCmax.ne.0) then
          call matconSORT (IW0(1,1), IW0(1,2), nn, NCmax)
!CDIR NOVECTOR
          do k= IVECT(iv-1)+1, IVECT(iv)
            kk= k - IVECT(iv-1)
            inC= IW0(kk,2)+IVECT(iv-1)
            NEWtoOLD_DJDS_L(k  )= inC
            OLDtoNEW_DJDS_L(inC)= k
          enddo


          icc= 0
           IW= 0

          do ip= 1, np_smp
            icou= ip - np_smp + IVECT(iv-1)
!voption novec
!cdir novector
            do k= IVECT(iv-1)+1, IVECT(iv)
              icou= icou + np_smp
              if (icou.gt.IVECT(iv)) exit
              icc= icc + 1
              IW(icc)= NEWtoOLD_DJDS_L(icou)
            enddo
          enddo

!voption novec
!cdir novector
          do k= IVECT(iv-1)+1, IVECT(iv)
            kk= k - IVECT(iv-1)
             i= IW(kk)
            NEWtoOLD_DJDS_L(k)= i
            OLDtoNEW_DJDS_L(i)= k
          enddo        

          IW= 0
!voption novec
!cdir novector
          do ip= 1, np_smp
            IW(ip)= IW(ip-1) + STACKmc((iv-1)*np_smp+ip)
          enddo

          do j= 1, NLmaxHYP(iv)
            do ip= 1, np_smp
              icou= 0              
                iStart= IW(ip-1) + 1 + IVECT(iv-1)
                iEnd= IW(ip  )     + IVECT(iv-1)
!voption novec
!cdir novector
              do i= iStart, iEnd
                in= NEWtoOLD_DJDS_L(i)
                if (INLmc(in).ge.j) then
                  icou= icou + 1
                  inumDJDS_L(npLX1*(iv-1)+NLmax*(ip-1)+j)= icou
                endif
              enddo
            enddo
          enddo
         else
!voption novec
!cdir novector
          do k= IVECT(iv-1)+1, IVECT(iv)
            NEWtoOLD_DJDS_L(k)= k
            OLDtoNEW_DJDS_L(k)= k
          enddo
        endif
        deallocate (IW0)
      enddo

!cdir novector
      do iv= 1, NHYP
!cdir novector
        do ip= 1, np_smp
!voption novec
!cdir novector
          do j= 1, NLmax
            iC= np_smp*NLmax*(iv-1) + NLmax*(ip-1) + j
            iM= np_smp*NLmax*(iv-1) + NLmax*(ip-1) + j - 1
            indexDJDS_L(iC)= indexDJDS_L(iM) + inumDJDS_L(iC)
          enddo
        enddo
      enddo

!      write(*,*) 'indexDJDS_L'
!      write(*,'(i15)') indexDJDS_L
!      write(*,*) 'inumDJDS_L'
!      write(*,'(i15)') inumDJDS_L

!C
!C== UPPER part
      do iv= 1, NHYP
        nn= IVECT(iv) - IVECT(iv-1)
        allocate (IW0(nn,2))
        NCmax= -NP
!CDIR NOVECTOR
        do k= IVECT(iv-1)+1, IVECT(iv)
          kk= k - IVECT(iv-1)
          IWKX(kk,1)= INUmc(k)
          IWKX(kk,2)= 0
          IW0 (kk,1)= INUmc(k)
          IW0 (kk,2)= 0
          NCmax     = max(NCmax,INUmc(k))
        enddo

        if (NCmax.ne.0) then
          call matconSORT (IW0(1,1), IW0(1,2), nn, NCmax)

!CDIR NOVECTOR
          do k= IVECT(iv-1)+1, IVECT(iv)
            kk= k - IVECT(iv-1)
            inC= IW0(kk,2)+IVECT(iv-1)
            NEWtoOLD_DJDS_U(k  )= inC
            OLDtoNEW_DJDS_U(inC)= k
          enddo

          icc= 0
           IW= 0
          do ip= 1, np_smp
            icou= ip - np_smp + IVECT(iv-1)
!voption novec
!cdir novector
            do k= IVECT(iv-1)+1, IVECT(iv)
              icou= icou + np_smp
              if (icou.gt.IVECT(iv)) exit     
              icc= icc + 1
              IW(icc)= NEWtoOLD_DJDS_U(icou)
            enddo
          enddo

!voption novec
!cdir novector
          do k= IVECT(iv-1)+1, IVECT(iv)
            kk= k - IVECT(iv-1)
             i= IW(kk)
            NEWtoOLD_DJDS_U(k)= i
            OLDtoNEW_DJDS_U(i)= k
          enddo        

          IW= 0
!voption novec
!cdir novector
          do ip= 1, np_smp
            IW(ip)= IW(ip-1) + STACKmc((iv-1)*np_smp+ip)
          enddo

          do j= 1, NUmaxHYP(iv)
            do ip= 1, np_smp
              icou= 0              
                iStart= IW(ip-1) + 1 + IVECT(iv-1)
                iEnd= IW(ip  )     + IVECT(iv-1)
!voption novec
!cdir novector
              do i= iStart, iEnd
                in= NEWtoOLD_DJDS_U(i)
                if (INUmc(in).ge.j) then
                  icou= icou + 1
                  inumDJDS_U(npUX1*(iv-1)+NUmax*(ip-1)+j)= icou
                endif
              enddo
            enddo
          enddo
         else
!voption novec
!cdir novector
          do k= IVECT(iv-1)+1, IVECT(iv)
            NEWtoOLD_DJDS_U(k)= k
            OLDtoNEW_DJDS_U(k)= k
          enddo
        endif
        deallocate (IW0)
      enddo
!
!CDIR NODEP
      do i= N+1, NP
        OLDtoNEW_DJDS_L(i)= i
        OLDtoNEW_DJDS_U(i)= i
      enddo
!
!
!
!cdir novector
      do iv= 1, NHYP
!cdir novector
        do ip= 1, np_smp
!voption novec
!cdir novector
          do j= 1, NUmax
            iC= np_smp*NUmax*(iv-1) + NUmax*(ip-1) + j
            iM= np_smp*NUmax*(iv-1) + NUmax*(ip-1) + j - 1
            indexDJDS_U(iC)= indexDJDS_U(iM) + inumDJDS_U(iC)
          end do
        end do
      end do

!      write(*,*) 'indexDJDS_U'
!      write(*,'(i15)') indexDJDS_U
!      write(*,*) 'inumDJDS_U'
!      write(*,'(i15)') inumDJDS_U
!
!voption novec
!cdir novector
      do iv = 1, (NHYP*np_smp)
        STACKmc(iv)= STACKmc(iv-1) + STACKmc(iv)
      end do
!
      end subroutine set_djds_ordering
!
! ----------------------------------------------------------------------
!
      end module DJDS_ordering
