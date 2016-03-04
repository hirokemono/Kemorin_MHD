!
!     module m_bc_data_ene
!.......................................................................
!
!      Written by Kemorin on Feb., 2004
!
!!      subroutine set_bc_temp_id                                       &
!!     &         (node, ele, fluid, nod_grp, iphys, nod_fld)
!!      subroutine set_bc_composition_id(node, ele, fluid, nod_grp)
!
!
      module m_bc_data_ene
!
      use m_precision
      use t_bc_data_temp
!
      implicit  none
!
!
!>      Structure for nodal boudnary for temperature
      type(nodal_bcs_4_scalar_type), save :: Tnod1_bcs
!
!>      Structure for nodal boudnary for composition
      type(nodal_bcs_4_scalar_type), save :: Cnod1_bcs
!
!
      type(scaler_fixed_nod_bc_type), save :: nod_bc1_t
!
      end module m_bc_data_ene
