!
!     module m_bc_data_ene
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!!      subroutine allocate_bc_ene
!!      subroutine allocate_bc_composition
!!
!!      subroutine allocate_bc_t_sgs
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
      type(scaler_fixed_nod_bc_type) :: nod_bc1_c
!
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_t
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_c
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_id_temp
      end subroutine set_bc_id_temp
!
!  ---------------------------------------------------------------------
!
      end module m_bc_data_ene
