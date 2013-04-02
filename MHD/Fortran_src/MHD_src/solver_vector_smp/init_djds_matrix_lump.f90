!
!      module init_djds_matrix_lump
!
!     programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!     modified by H. Matsui on Aug., 2005
!
!      subroutine init_11_matrix_lump(numnod, numnod_fld, inod_fld,     &
!     &          OLDtoNEW, ml_o, num_comp, aiccg)
!      subroutine init_33_matrix_lump(numnod, numnod_fld, inod_fld,     &
!     &          OLDtoNEW, ml_o, num_comp, aiccg)
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
     &          OLDtoNEW, ml_o, num_comp, aiccg)
!
!
      integer(kind = kint), intent(in) :: numnod, numnod_fld
      integer(kind = kint), intent(in) :: inod_fld(numnod_fld)
      integer(kind = kint), intent(in) :: OLDtoNEW(numnod)
      real(kind=kreal), intent(in) :: ml_o(numnod)
      integer(kind = kint), intent(in) :: num_comp
      real(kind=kreal), intent(inout) :: aiccg(0:num_comp)
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
     &          OLDtoNEW, ml_o, num_comp, aiccg)
!
      integer(kind = kint), intent(in) :: numnod, numnod_fld
      integer(kind = kint), intent(in) :: inod_fld(numnod_fld)
      integer(kind = kint), intent(in) :: OLDtoNEW(numnod)
      real(kind=kreal), intent(in) :: ml_o(numnod)
      integer(kind = kint), intent(in) :: num_comp
      real(kind=kreal), intent(inout) :: aiccg(-8:num_comp)
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
