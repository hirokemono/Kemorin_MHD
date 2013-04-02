!
!     module set_DJDS_off_diagonal
!
!      programmed by H.Matsui on July 2002
!      Modified by Hiroaki Matsui on Oct., 2006
!
!      subroutine s_set_DJDS_off_diagonal(N, NP, PEsmpTOT,              &
!     &          NLmax, NUmax, NPL, NPU, npLX1, npUX1,                  &
!     &          NHYP, STACKmc, NLmaxHYP, NUmaxHYP,                     &
!     &          OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,            &
!     &          indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,      &
!     &          PEon, COLORon, nod1, nod2, mat_num)
!
      module set_DJDS_off_diagonal
!
      use m_precision
!
      implicit none
!
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_set_DJDS_off_diagonal(N, NP, PEsmpTOT,               &
     &          NLmax, NUmax, NPL, NPU, npLX1, npUX1,                   &
     &          NHYP, STACKmc, NLmaxHYP, NUmaxHYP,                      &
     &          OLDtoNEW, OLDtoNEW_DJDS_L, OLDtoNEW_DJDS_U,             &
     &          indexDJDS_L, indexDJDS_U, itemDJDS_L, itemDJDS_U,       &
     &          PEon, COLORon, nod1, nod2, mat_num)
!
!
      integer (kind = kint), intent(in) :: NP, N
      integer (kind = kint), intent(in) :: PEsmpTOT
!
      integer (kind = kint), intent(in) :: NHYP
      integer (kind = kint), intent(in) :: NPL, NPU
      integer (kind = kint), intent(in) :: NUmax, NLmax
      integer (kind = kint), intent(in) :: npUX1, npLX1
      integer (kind = kint), intent(in) :: STACKmc(0:PEsmpTOT*NHYP)
      integer (kind = kint), intent(in) :: NLmaxHYP(NHYP)
      integer (kind = kint), intent(in) :: NUmaxHYP(NHYP)
      integer (kind = kint), intent(in) :: OLDtoNEW(NP)
      integer (kind = kint), intent(in) :: OLDtoNEW_DJDS_L(NP)
      integer (kind = kint), intent(in) :: OLDtoNEW_DJDS_U(NP)
!
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_L(0:PEsmpTOT*NLmax*NHYP)
      integer (kind = kint), intent(in)                                 &
     &       :: indexDJDS_U(0:PEsmpTOT*NUmax*NHYP)
      integer (kind = kint), intent(in) :: itemDJDS_L(NPL)
      integer (kind = kint), intent(in) :: itemDJDS_U(NPU)
!
      integer (kind = kint), intent(in) :: PEon(NP)
      integer (kind = kint), intent(in) :: COLORon(NP)
      integer (kind = kint), intent(in) :: nod1, nod2
!
      integer (kind = kint), intent(inout) :: mat_num
!
      integer (kind = kint) :: ipU, jpU, kz
      integer (kind = kint) :: ipL, jpL
      integer (kind = kint) :: ip, jp, kp
      integer (kind = kint) :: nn, is, iv
!
      mat_num = 0
!
!      write(*,*) 'nod1,nod2', nod1,nod2
      ip = OLDtoNEW( nod1 )
      jp = OLDtoNEW( nod2 )
!      write(*,*) 'ip,jp', ip,jp
!
!      write(*,*) 'STACKmc', size(STACKmc), PEsmpTOT, NHYP
!      write(*,*) 'NLmaxHYP', size(NLmaxHYP), NHYP
!      write(*,*) 'NUmaxHYP', size(NUmaxHYP), NHYP
!      write(*,*) 'OLDtoNEW', size(OLDtoNEW), NP
!      write(*,*) 'OLDtoNEW_DJDS_L', size(OLDtoNEW_DJDS_L), NP
!      write(*,*) 'OLDtoNEW_DJDS_U', size(OLDtoNEW_DJDS_U), NP
!      write(*,*) 'indexDJDS_L', size(indexDJDS_L), PEsmpTOT,NLmax,NHYP
!      write(*,*) 'indexDJDS_U', size(indexDJDS_U), PEsmpTOT,NUmax,NHYP
!      write(*,*) 'itemDJDS_L', size(itemDJDS_L), NPL
!      write(*,*) 'itemDJDS_U', size(itemDJDS_U), NPU
!      write(*,*) 'PEon', size(PEon), NP
!      write(*,*) 'COLORon', size(COLORon), NP
!
      if (ip .le. N) then
!
!  construct indices for off diagonal component
!
!     upper component!!
!
        if (jp .gt. ip) then
!        write(my_rank+50,*) 'upper'
!
!
          ipU= OLDtoNEW_DJDS_U(ip)
          jpU= OLDtoNEW_DJDS_U(jp)
          kp = PEon(ipU)
          iv = COLORon(ipU)
          nn = ipU - STACKmc( (iv-1)*PEsmpTOT+kp-1)
          do kz= 1, NUmaxHYP(iv)
            iS= indexDJDS_U(npUX1*(iv-1)+NUmax*(kp-1)+kz-1) + nn
            if ( itemDJDS_U(iS).eq.jpU) then
              mat_num= iS + NP + NPL
              exit
            endif
          enddo
!
        endif
!
!        lower component!!
!
        if (jp.lt.ip) then
!
!        write(my_rank+50,*) 'lower'
!
          ipL= OLDtoNEW_DJDS_L(ip)
          jpL= OLDtoNEW_DJDS_L(jp)
          kp = PEon(ipL)
          iv = COLORon(ipL)
          nn = ipL - STACKmc( (iv-1)*PEsmpTOT+kp-1)
!          write(*,*) 'kz', NLmaxHYP(iv)
          do kz= 1, NLmaxHYP(iv)
            iS= indexDJDS_L(npLX1*(iv-1)+NLmax*(kp-1)+kz-1) + nn
!            write(*,*) 'iS', kz, iS, size(itemDJDS_L)
            if ( itemDJDS_L(iS).eq.jpL) then
              mat_num= iS + NP
              exit
            endif
          enddo
!
        endif
!
        if (jp.eq.ip) then
!         write(my_rank+50,*) 'diag'
          mat_num = ip
        endif
!
      end if
!
      end subroutine s_set_DJDS_off_diagonal
!
!-----------------------------------------------------------------------
!
      end module set_DJDS_off_diagonal
