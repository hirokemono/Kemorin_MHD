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
!nod_bc1_rot%ibc_stack_smp
!
      real(kind=kreal), allocatable :: bc_v_id_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_vr0_id_apt(:)
!
      real(kind=kreal), allocatable :: bc_fr_id_apt(:)
!
      real(kind=kreal), allocatable :: bc_v_sgs_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_vsp_id_apt(:)
!
!
      real(kind=kreal), allocatable :: bc_p_id_apt(:)
!
      real(kind=kreal), allocatable :: bc_ps_id_apt(:)
!
! -----------------------------------------------------------------------
!
      contains 
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_velo
!
!
       allocate(bc_v_id_apt(nod_bc1_v%nmax_bc,3))
       if (nod_bc1_v%nmax_bc/=0)  bc_v_id_apt=0.0d00
!
       end subroutine allocate_bc_velo
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_press
!
!
       allocate(bc_p_id_apt(nod_bc1_p%num_bc_nod))
       if (nod_bc1_p%num_bc_nod .gt. 0)  bc_p_id_apt=0.0d00
!
       end subroutine allocate_bc_press
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vr0
!
! 
       allocate(bc_vr0_id_apt(nod_bc1_vr0%num_bc_nod))
       if (nod_bc1_vr0%num_bc_nod .gt. 0)  bc_vr0_id_apt=0.0d00
!
       end subroutine allocate_bc_vr0
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_vfr
!
!
      allocate(bc_fr_id_apt(nod_bc1_vfree%num_bc_nod))
      if (nod_bc1_vfree%num_bc_nod .gt. 0)  bc_fr_id_apt=0.0d00
!
      end subroutine allocate_bc_vfr
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_vsp
!
!
       allocate(bc_vsp_id_apt(nod_bc1_vsp%num_bc_nod))
       if (nod_bc1_vsp%num_bc_nod .gt. 0)  bc_vsp_id_apt = 0.0d00
!
       end subroutine allocate_bc_vsp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_v_sgs
!
!
       allocate(bc_v_sgs_apt(sgs_bc1_v%nmax_bc,3))
       if (sgs_bc1_v%nmax_bc .gt. 0) bc_v_sgs_apt=0.0d00
!
       end subroutine allocate_bc_v_sgs
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_p_sgs
!
!
       allocate(bc_ps_id_apt(sgs_bc1_p%num_bc_nod))
       if (sgs_bc1_p%num_bc_nod .gt. 0) bc_ps_id_apt = 0.0d00
!
       end subroutine allocate_bc_p_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_velo
