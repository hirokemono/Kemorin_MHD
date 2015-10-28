!
!     module m_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
!       subroutine allocate_bc_magne(numnod)
!       subroutine allocate_bc_magne_4_element(nnod_4_ele)
!
!       subroutine deallocate_ibc_4_magne
!       subroutine deallocate_bc2_magne
!
      module m_bc_data_magne
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
      type(vect_fixed_nod_bc_type) :: nod_bc1_b
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_b
!sgs_bc1_b%ibc_stack_smp
!
!
      real (kind=kreal)  , allocatable :: bc_b_id_apt(:,:)
!
      real (kind=kreal),   allocatable :: bc_b_sgs_id_apt(:,:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_magne
!
!
      allocate(bc_b_id_apt(nod_bc1_b%nmax_bc,3))
      if (nod_bc1_b%nmax_bc .gt. 0) bc_b_id_apt=0.0d00
!
      end subroutine allocate_bc_magne
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_b_sgs
!
!
      allocate(bc_b_sgs_id_apt(sgs_bc1_b%nmax_bc,3))
      if (sgs_bc1_b%nmax_bc .gt. 0)  bc_b_sgs_id_apt=0.0d00
!
      end subroutine allocate_bc_b_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_magne
