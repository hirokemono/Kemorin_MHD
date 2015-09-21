!set_fixed_phys_boundary.f90
!      module set_fixed_phys_boundary
!
!      Written by H. Matsui on july, 2005
!
!!      subroutine set_fixed_bc_vect_phys(nmax_phys_bc, num_phys_bc,    &
!!     &          ibc_id, bc_id_apt, numnod, ncomp_nod, i_vect, d_nod)
!!      subroutine set_fixed_bc_scalar_phys(num_phys_bc, ibc_id,        &
!!     &          bc_id_apt, numnod, ncomp_nod, i_comp, d_nod)
!!      subroutine del_vector_phys_on_bc(nmax_phys_bc, num_phys_bc,     &
!!     &         ibc_id, numnod, ncomp_nod, i_vect, d_nod)
!!      subroutine del_vector_phys_on_1bc(num_phys_bc, ibc_id,          &
!!     &          numnod, ncomp_nod, i_vect, d_nod)
!!      subroutine del_scalar_phys_on_bc(num_phys_bc, ibc_id,           &
!!     &          numnod, ncomp_nod, i_comp, d_nod)
!
      module set_fixed_phys_boundary
!
      use m_precision
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_bc_vect_phys(nmax_phys_bc, num_phys_bc,      &
     &          ibc_id, bc_id_apt, numnod, ncomp_nod, i_vect, d_nod)
!
      integer (kind = kint), intent(in) :: nmax_phys_bc
      integer (kind = kint), intent(in) :: num_phys_bc(3)
      integer (kind = kint), intent(in) :: ibc_id(nmax_phys_bc,3)
      real   (kind = kreal), intent(in) :: bc_id_apt(nmax_phys_bc,3)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_vect
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: nd, inum, inod
      integer (kind = kint) :: i_comp
!
!
      do nd = 1, 3
        if ( num_phys_bc(nd).gt.0) then
          i_comp = i_vect + nd - 1
          do inum=1, num_phys_bc(nd)
            inod = ibc_id(inum,nd)
            d_nod(inod,i_comp) = bc_id_apt(inum,nd)
          end do
        end if
      end do
!
      end subroutine set_fixed_bc_vect_phys
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_bc_scalar_phys(num_phys_bc, ibc_id,          &
     &          bc_id_apt, numnod, ncomp_nod, i_comp, d_nod)
!
      integer (kind = kint), intent(in) :: num_phys_bc
      integer (kind = kint), intent(in) :: ibc_id(num_phys_bc)
      real   (kind = kreal), intent(in) :: bc_id_apt(num_phys_bc)
!
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_comp
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inum, inod
!
!
      do inum=1, num_phys_bc
        inod = ibc_id(inum)
        d_nod(inod,i_comp) = bc_id_apt(inum)
      end do
!
      end subroutine set_fixed_bc_scalar_phys
!
!  ---------------------------------------------------------------------
!
      subroutine del_vector_phys_on_bc(nmax_phys_bc, num_phys_bc,       &
     &          ibc_id, numnod, ncomp_nod, i_vect, d_nod)
!
      integer (kind = kint), intent(in) :: nmax_phys_bc
      integer (kind = kint), intent(in) :: num_phys_bc(3)
      integer (kind = kint), intent(in) :: ibc_id(nmax_phys_bc,3)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_vect
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: nd, inum, inod
      integer (kind = kint) :: i_comp
!
!
      do nd = 1, 3
        if ( num_phys_bc(nd) .gt. 0 ) then
          i_comp = i_vect + nd - 1
          do inum=1, num_phys_bc(nd)
            inod = ibc_id(inum,nd)
            d_nod(inod, i_comp) = 0.0d0
          end do
        end if
      end do
!
      end subroutine del_vector_phys_on_bc
!
!  ---------------------------------------------------------------------
!
      subroutine del_vector_phys_on_1bc(num_phys_bc, ibc_id,            &
     &          numnod, ncomp_nod, i_vect, d_nod)
!
      integer (kind = kint), intent(in) :: num_phys_bc
      integer (kind = kint), intent(in) :: ibc_id(num_phys_bc)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_vect
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inum, inod
!
!
      do inum=1, num_phys_bc
        inod = ibc_id(inum)
        d_nod(inod, i_vect  ) = 0.0d0
        d_nod(inod, i_vect+1) = 0.0d0
        d_nod(inod, i_vect+2) = 0.0d0
      end do
!
      end subroutine del_vector_phys_on_1bc
!
!  ---------------------------------------------------------------------
!
      subroutine del_scalar_phys_on_bc(num_phys_bc, ibc_id,             &
     &          numnod, ncomp_nod, i_comp, d_nod)
!
      integer (kind = kint), intent(in) :: num_phys_bc
      integer (kind = kint), intent(in) :: ibc_id(num_phys_bc)
      integer (kind = kint), intent(in) :: numnod, ncomp_nod
      integer (kind = kint), intent(in) :: i_comp
      real(kind = kreal), intent(inout) :: d_nod(numnod,ncomp_nod)
!
      integer (kind = kint) :: inum, inod
!
!
      do inum=1, num_phys_bc
        inod = ibc_id(inum)
        d_nod(inod, i_comp)= 0.0d0
      end do
!
      end subroutine del_scalar_phys_on_bc
!
!  ---------------------------------------------------------------------
!
      end module set_fixed_phys_boundary
