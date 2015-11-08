!
!     module m_bc_data_ene
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!       subroutine allocate_bc_ene(numnod)
!       subroutine allocate_bc_temp_4_element(nnod_4_ele)
!       subroutine deallocate_ibc_4_temp
!       subroutine deallocate_bc2_temp
!
!
      module m_bc_data_ene
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
!
      type(scaler_fixed_nod_bc_type) :: nod_bc1_t
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_t
!sgs_bc1_t%ibc_stack_smp
!
      real(kind=kreal), allocatable :: bc_e_id_apt(:)
! 
      real(kind=kreal), allocatable :: bc_t_sgs_id_apt(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_bc_ene
!
!
      allocate(bc_e_id_apt(nod_bc1_t%num_bc_nod))
      if (nod_bc1_t%num_bc_nod .gt. 0) bc_e_id_apt = 0.0d00
!
      end subroutine allocate_bc_ene
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_bc_t_sgs
!
!
      allocate(bc_t_sgs_id_apt(sgs_bc1_t%num_bc_nod))
      if (sgs_bc1_t%num_bc_nod .gt. 0)  bc_t_sgs_id_apt=0.0d00 
!
      end subroutine allocate_bc_t_sgs
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_ene
