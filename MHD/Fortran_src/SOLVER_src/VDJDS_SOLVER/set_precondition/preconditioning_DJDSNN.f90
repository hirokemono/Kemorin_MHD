!
!      module preconditioning_DJDSNN
!
!     Written by H. Matsui on Jan., 2005
!
!      subroutine precond_DJDSnn                                        &
!     &         ( N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, OtoN_U,        &
!     &           D, ALU_L, ALU_U, PRECOND, sigma_diag)
!
      module preconditioning_DJDSNN
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
      subroutine precond_DJDSnn                                         &
     &         ( N, NP, NB, PEsmpTOT, STACKmcG, OtoN_L, OtoN_U,         &
     &           D, ALU_L, ALU_U, PRECOND, sigma_diag)
!
      use precond_block_ilu_nn
      use precond_ssor_gausszeidel_nn
      use precond_diagonal_scaling_nn
!
      integer(kind=kint), intent(in) :: N, NP, NB
      integer(kind=kint), intent(in) :: PEsmpTOT
      character(len=kchara), intent(in)  :: PRECOND

      integer(kind=kint), intent(in) :: OtoN_L(NP)
      integer(kind=kint), intent(in) :: OtoN_U(NP)

      integer(kind=kint), intent(in) :: STACKmcG(0:PEsmpTOT)

      real(kind=kreal), intent(in) :: D(NB*NB*NP )
!
      real(kind=kreal), intent(in) :: sigma_diag

      real(kind=kreal), intent(inout) :: ALU_L(NB*NB*N), ALU_U(NB*NB*N)
!
!
      if (PRECOND(1:6).eq.'BL_ILU') then
        call precond_bl_ilu_nn(N, NP, NB, PEsmpTOT, STACKmcG,           &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      else if ( ((PRECOND(1:1).eq.'G').or.(PRECOND(1:1).eq.'g')) .and.  &
     &          ((PRECOND(2:2).eq.'A').or.(PRECOND(2:2).eq.'a')) .and.  &
     &          ((PRECOND(3:3).eq.'U').or.(PRECOND(3:3).eq.'u')) .and.  &
     &          ((PRECOND(4:4).eq.'S').or.(PRECOND(4:4).eq.'s')) .and.  &
     &          ((PRECOND(5:5).eq.'S').or.(PRECOND(5:5).eq.'s')) ) then
        call precond_gauss_zeidel_nn(N, NP, NB, PEsmpTOT, STACKmcG,     &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U)
!
      else if (PRECOND(1:4).eq.'SSOR') then
        call precond_ssor_nn(N, NP, NB, PEsmpTOT, STACKmcG,             &
     &          OtoN_L, OtoN_U, D, ALU_L, ALU_U, sigma_diag)
!
      else if (PRECOND(1:4).eq.'DIAG') then
        call precond_diagonal_nn(N, NP, NB, PEsmpTOT, STACKmcG,         &
     &          D, ALU_L, ALU_U, sigma_diag)
      endif
!
      end subroutine precond_DJDSnn
!
! ----------------------------------------------------------------------
!
      end module preconditioning_DJDSNN
