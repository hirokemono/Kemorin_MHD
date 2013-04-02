!
!      module preconditioning_DJDS11
!
!     Written by H. Matsui on Jan., 2005
!
!      subroutine precond_DJDS11                                        &
!     &         ( N, NP, NL, NPL, NVECT, PEsmpTOT, STACKmcG, STACKmc,   &
!     &           NLhyp, IVECT, OtoN_L, OtoN_U, LtoU, D, INL, IAL,      &
!     &           AL, ALU_L, ALU_U, PRECOND, sigma_diag)
!
      module preconditioning_DJDS11
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
      subroutine precond_DJDS11                                         &
     &         ( N, NP, NL, NPL, NVECT, PEsmpTOT, STACKmcG, STACKmc,    &
     &           NLhyp, IVECT, OtoN_L, OtoN_U, LtoU, D, INL, IAL,       &
     &           AL, ALU_L, ALU_U, PRECOND, sigma_diag)
!
      use precond_incomplete_lu
      use precond_ssor_gausszeidel
      use precond_diagonal_scaling
!
      integer(kind=kint), intent(in) :: N, NP, NL, NPL
      integer(kind=kint), intent(in) :: PEsmpTOT, NVECT
      character(len=kchara), intent(in)  :: PRECOND

      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: LtoU(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)
      integer(kind=kint), intent(in) :: IVECT(0:NVECT)
      integer(kind=kint), intent(in) :: NLhyp(NVECT)

      integer(kind=kint), intent(in) :: INL(0:NL*NVECT*PEsmpTOT)
      integer(kind=kint), intent(in) :: IAL(NPL)

      integer(kind=kint), intent(in) :: STACKmc(0:PEsmpTOT*NVECT)
      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

      real(kind=kreal), intent(in) :: D(NP )
      real(kind=kreal), intent(in) :: AL(NPL)
!
      real(kind=kreal), intent(in) :: sigma_diag

      real(kind=kreal), intent(inout) :: ALU_L(N), ALU_U(N)
!
      real(kind = kreal), parameter :: one = 1.0d0
!
!
      if (PRECOND(1:2).eq.'IC' .or. PRECOND(1:3).eq.'ILU') then
       call precond_ilu(N, NP, PEsmpTOT, OtoN_L, NVECT,                 &
     &         NL, IVECT, NLhyp, LtoU, STACKmc, NPL, D,                 &
     &         INL, IAL, AL, ALU_L, ALU_U, sigma_diag)
!
      else if ( ((PRECOND(1:1).eq.'G').or.(PRECOND(1:1).eq.'g')) .and.  &
     &          ((PRECOND(2:2).eq.'A').or.(PRECOND(2:2).eq.'a')) .and.  &
     &          ((PRECOND(3:3).eq.'U').or.(PRECOND(3:3).eq.'u')) .and.  &
     &          ((PRECOND(4:4).eq.'S').or.(PRECOND(4:4).eq.'s')) .and.  &
     &          ((PRECOND(5:5).eq.'S').or.(PRECOND(5:5).eq.'s')) ) then
        call precond_gauss_zeidel(N, NP, PEsmpTOT, STACKmcG,            &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!
      else if ( ((PRECOND(1:1).eq.'J').or.(PRECOND(1:1).eq.'j')) .and.  &
     &          ((PRECOND(2:2).eq.'A').or.(PRECOND(2:2).eq.'a')) .and.  &
     &          ((PRECOND(3:3).eq.'C').or.(PRECOND(3:3).eq.'c')) .and.  &
     &          ((PRECOND(4:4).eq.'O').or.(PRECOND(4:4).eq.'o')) .and.  &
     &          ((PRECOND(5:5).eq.'B').or.(PRECOND(5:5).eq.'b')) .and.  &
     &          ((PRECOND(6:6).eq.'I').or.(PRECOND(6:6).eq.'i')) ) then
        call precond_diagonal(N, NP, D, ALU_L, ALU_U, one)
!
      else if (PRECOND(1:4).eq.'SSOR') then
        call precond_ssor(N, NP, PEsmpTOT, STACKmcG, OtoN_L, OtoN_U,    &
     &         D, ALU_L, ALU_U, sigma_diag)
!
      else if (PRECOND(1:4).eq.'DIAG') then
        call precond_diagonal(N, NP, D, ALU_L, ALU_U, sigma_diag)
      endif
!
      end subroutine precond_DJDS11
!
! ----------------------------------------------------------------------
!
      end module preconditioning_DJDS11
