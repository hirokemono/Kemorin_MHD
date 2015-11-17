!
!     module m_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
      module m_bc_data_magne
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
      type(vect_fixed_nod_bc_type) :: nod_bc1_a
!
      type(vect_fixed_nod_bc_type) :: nod_bc1_b
!
      type(vect_fixed_nod_bc_type) :: nod_bc1_j
!
      type(scaler_fixed_nod_bc_type) :: nod_bc1_f
!
      type(scaler_fixed_nod_bc_type) :: nod_bc1_fcd
!
      type(scaler_fixed_nod_bc_type) :: nod_bc1_fins
!nod_bc1_fins%ibc_stack_smp
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_a
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_b
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_f
!
      end module m_bc_data_magne
