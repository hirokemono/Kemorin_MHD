!set_fixed_boundaries.f90
!      module set_fixed_boundaries
!
!      Written by H. Matsui on july, 2005
!
!      subroutine set_fixed_bc_ff_scalar(num_phys_bc, ibc_id,           &
!     &    bc_id_apt)
!      subroutine set_fixed_bc_zero_ff_vect(nmax_phys_bc, num_phys_bc,  &
!     &          ibc_id)
!      subroutine set_fixed_boundary_zero_ff(num_phys_bc, ibc_id)
!
      module set_fixed_boundaries
!
      use m_precision
!
      use m_geometry_parameter
      use m_finite_element_matrix
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_bc_ff_scalar(num_phys_bc, ibc_id,            &
     &    bc_id_apt)
!
       integer (kind = kint) :: num_phys_bc
       integer (kind = kint), intent(in) :: ibc_id(num_phys_bc)
       real   (kind = kreal), intent(in) :: bc_id_apt(num_phys_bc)
!
      integer (kind = kint) :: inum, inod
!
       do inum=1, num_phys_bc
         inod = ibc_id(inum)
         ff(inod,1)=bc_id_apt(inum)
       end do
!
      end subroutine set_fixed_bc_ff_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_bc_zero_ff_vect(nmax_phys_bc, num_phys_bc,   &
     &          ibc_id)
!
       integer (kind = kint), intent(in) :: nmax_phys_bc
       integer (kind = kint), intent(in) :: num_phys_bc(3)
       integer (kind = kint), intent(in) :: ibc_id(nmax_phys_bc,3)
!
      integer (kind = kint) :: nd, inum, inod
!
       do nd = 1, 3
        if (num_phys_bc(nd).gt.0) then
         do inum=1, num_phys_bc(nd)
           inod = ibc_id(inum,nd)
           ff(inod,nd)=    0.0d0
           ff_nl(inod,nd)= 0.0d0
         end do
        end if
       end do
!
      end subroutine set_fixed_bc_zero_ff_vect
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_boundary_zero_ff(num_phys_bc, ibc_id)
!
       integer (kind = kint), intent(in) :: num_phys_bc
       integer (kind = kint), intent(in) :: ibc_id(num_phys_bc)
!
      integer (kind = kint) :: inum, inod
!
!
       do inum=1, num_phys_bc
         inod = ibc_id(inum)
         ff(inod,1)=    0.0d0
         ff_nl(inod,1)= 0.0d0
       end do
!
      end subroutine set_fixed_boundary_zero_ff
!
!  ---------------------------------------------------------------------
!
      end module set_fixed_boundaries
