!precond_incomplete_lu.f90
!      module precond_incomplete_lu
!
!      Written by K. Nakajima in 2001
!      Modified by H. Matsui on Jan., 2006
!
!      subroutine precond_ilu(N, NP, PEsmpTOT, OtoN_L, NVECT,           &
!     &         NL, IVECT, NLhyp, LtoU, STACKmc, NPL, D,                &
!     &         INL, IAL, AL, ALU_L, ALU_U, sigma_diag)
!
!
      module precond_incomplete_lu
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
      subroutine precond_ilu(N, NP, PEsmpTOT, OtoN_L, NVECT,            &
     &         NL, IVECT, NLhyp, LtoU, STACKmc, NPL, D,                 &
     &         INL, IAL, AL, ALU_L, ALU_U, sigma_diag)
!
      integer(kind=kint), intent(in) :: N
!       number of internal nodes (exclude external node)
      integer(kind=kint), intent(in) :: NP
!       number of nodes          (include external node)
      integer(kind=kint), intent(in) :: PEsmpTOT
      integer(kind=kint), intent(in) :: NVECT
      integer(kind=kint), intent(in) :: NL
      integer(kind=kint), intent(in) :: NPL
!
      integer(kind=kint), intent(in) :: NLhyp(NVECT)
      integer(kind=kint), intent(in) :: IVECT(0:NVECT)
      integer(kind=kint), intent(in) :: LtoU(NP)
      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: IAL(NPL)
      integer(kind=kint), intent(in) :: INL(0:PEsmpTOT*NL*NVECT)
      integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
!
      real(kind=kreal), intent(in) :: AL(NPL)
      real(kind=kreal), intent(in) :: D(NP)
      real(kind=kreal), intent(in) :: sigma_diag
!
      real(kind=kreal), intent(inout) :: ALU_L(N)
      real(kind=kreal), intent(inout) :: ALU_U(N)
!
      integer(kind=kint) :: i, j, k, kk
      integer(kind=kint) :: in1, ip, iv, iv0, iStart, iEnd
      real(kind=kreal) :: ALUG(NP)
      real(kind=kreal) :: x_vec(NP)
!  preconditiong by iLU
!
!
!
      ALUG  = 0.d0
      ALU_U= 0.d0
      ALU_L= 0.d0

!
!CDIR NODEP
!VOPTION INDEP, VEC
      do i= 1, N
        in1 = OtoN_L(i)
        ALU_L(in1)= D(i)
        x_vec(in1)= D(i)*sigma_diag
      enddo

      do iv= 1, NVECT
        iv0= STACKmc(PEsmpTOT*(iv-1)+ip- 1)
        do ip= 1, PEsmpTOT
!poption noparallel
        do  j= 1, NLhyp(iv)
          iStart= INL(PEsmpTOT*NL*(iv-1)+NL*(ip-1)+j-1 )
          iEnd=   INL(PEsmpTOT*NL*(iv-1)+NL*(ip-1)+j   )
!OCL VECTOR, NOVREC
!cdir nodep
!voption indep (W,IAL,AL)
!CDIR NODEP
!VOPTION INDEP, VEC
          do i= iv0+1, iv0+iEnd-iStart
             k= i+iStart - iv0
            kk= IAL(k)
            ALU_L(i)= ALU_L(i) - AL(k)*AL(k)*ALUG(kk)
          enddo
        enddo

        iStart= IVECT(iv-1) + 1
        iEnd= IVECT(iv)
!CDIR NODEP
!VOPTION INDEP, VEC
        do i= iStart, iEnd
          ALUG(i)= 1.d0/ALU_L(i)
        enddo

        iStart= IVECT(iv-1) + 1
        iEnd= IVECT(iv)
!CDIR NODEP
!VOPTION INDEP, VEC
        do i= iStart, iEnd
          ALU_L(i)= ALUG(i)
        end do
       end do
      end do

!CDIR NODEP
!VOPTION INDEP, VEC
      do i= 1, N
          in1      = LtoU(i)
        ALU_U(in1)= ALU_L(i)
      enddo
!
!
      end subroutine precond_ilu
!
! ----------------------------------------------------------------------
!
      end module precond_incomplete_lu
