!>@file   init_djds_matrix_lump.f90
!!@brief  module init_djds_matrix_lump
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2005
!
!
!> @brief Set lumped mass matrix for Crnak-Nicolson scheme
!!
!!@verbatim
!!      subroutine init_11_matrix_lump(numnod, numnod_fld, inod_fld,    &
!!     &          OLDtoNEW, ml_o, num_non0, aiccg)
!!      subroutine init_33_matrix_lump(numnod, numnod_fld, inod_fld,    &
!!     &          OLDtoNEW, ml_o, num_non0, aiccg)
!!@endverbatim
!!
!!@n @param  numnod      number of node for simulation domain
!!@n @param  numnod_fld  number of node for mass matrix
!!@n @param  inod_fld    node ID for field
!!@n @param  OLDtoNEW    DJDS ordering table
!!@n @param  ml_o        lumped mass matrix
!!@n @param  num_non0    Number of components of field
!!@n @param  aiccg       DJDS compressed array
!
      module init_djds_matrix_lump
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
      subroutine init_11_matrix_lump(numnod, numnod_fld, inod_fld,      &
     &          OLDtoNEW, ml_o, num_non0, aiccg)
!
!
      integer(kind = kint), intent(in) :: numnod, numnod_fld
      integer(kind = kint), intent(in) :: inod_fld(numnod_fld)
      integer(kind = kint), intent(in) :: OLDtoNEW(numnod)
      real(kind=kreal), intent(in) :: ml_o(numnod)
      integer(kind = kint), intent(in) :: num_non0
      real(kind=kreal), intent(inout) :: aiccg(0:num_non0)
!
      integer (kind = kint) :: inod, inum, in
!
!
!$omp do private(inum,inod,in) 
!cdir nodep
      do inum = 1, numnod_fld
        inod = inod_fld(inum)
        in = OLDtoNEW(inod)
        aiccg(in) = ml_o(inod)
      end do
!$omp end do nowait
!
      end subroutine init_11_matrix_lump
!
! ----------------------------------------------------------------------
!
      subroutine init_33_matrix_lump(numnod, numnod_fld, inod_fld,      &
     &          OLDtoNEW, ml_o, num_non0, aiccg)
!
      integer(kind = kint), intent(in) :: numnod, numnod_fld
      integer(kind = kint), intent(in) :: inod_fld(numnod_fld)
      integer(kind = kint), intent(in) :: OLDtoNEW(numnod)
      real(kind=kreal), intent(in) :: ml_o(numnod)
      integer(kind = kint), intent(in) :: num_non0
      real(kind=kreal), intent(inout) :: aiccg(-8:num_non0)
!
      integer (kind = kint) :: inod, inum, in
!
!
!$omp do private(inum,inod,in) 
!cdir nodep
      do inum = 1, numnod_fld
        inod = inod_fld(inum)
        in = OLDtoNEW(inod)
        aiccg(in*9-8) = ml_o(inod)
        aiccg(in*9-4) = ml_o(inod)
        aiccg(in*9  ) = ml_o(inod)
      end do
!$omp end do nowait
!
      end subroutine init_33_matrix_lump
!
! ----------------------------------------------------------------------
!
      end module init_djds_matrix_lump
