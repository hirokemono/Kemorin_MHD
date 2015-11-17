!
!     module m_bc_data_velo
!.......................................................................
!
!      Written by H. Matsui
!
!       subroutine allocate_bc_velo
!
      module m_bc_data_velo
!
      use m_precision
      use t_nodal_bc_data
!
      implicit  none
!
!>      Structure for nodal boudnary for fixed velocity
      type(vect_fixed_nod_bc_type) :: nod_bc1_v
!>      Structure for nodal boudnary for SGS fixed velocity
      type(vect_fixed_nod_bc_type) :: sgs_bc1_v
!>      Structure for nodal boudnary for fixed pressure
      type(scaler_fixed_nod_bc_type) :: nod_bc1_p
!>      Structure for nodal boudnary for SGS fixed presure
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_p
!
!
!>      Structure for nodal boudnary for non-radial velocity
      type(scaler_fixed_nod_bc_type) :: nod_bc1_vr0
!>      Structure for nodal boudnary for free-slip velocity on plane
      type(scaler_fixed_nod_bc_type) :: nod_bc1_vfree
!>      Structure for nodal boudnary for special velocity on plane
      type(scaler_fixed_nod_bc_type) :: nod_bc1_vsp
!
!>      Structure for nodal boudnary for rotation
      type(scaler_rotaion_nod_bc_type) :: nod_bc1_rot
!
      end module m_bc_data_velo
