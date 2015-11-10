!
!     module m_bc_data_magne
!.......................................................................
!
!     Written by Kemorin
!
!!      subroutine allocate_bc_vect_p
!!      subroutine allocate_bc_b_sgs
!!
!!      subroutine allocate_bc_magne
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
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_a
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_b
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_f
!
!
      real(kind=kreal), allocatable :: bc_vp_id_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_b_id_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_j_id_apt(:,:)
!
      real (kind=kreal), allocatable :: bc_mag_p_id_apt(:)
!
!
      real(kind=kreal), allocatable :: bc_a_sgs_id_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_b_sgs_id_apt(:,:)
!
      real (kind=kreal), allocatable :: bc_mp_sgs_id_apt(:)
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
      subroutine allocate_bc_vect_p
!
!
      allocate(bc_vp_id_apt(nod_bc1_a%nmax_bc,3))
      if (nod_bc1_a%nmax_bc .gt. 0)  bc_vp_id_apt=0.0d00
!
      end subroutine allocate_bc_vect_p
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_current
!
!
      allocate(bc_j_id_apt(nod_bc1_j%nmax_bc,3))
      if (nod_bc1_j%nmax_bc/=0)   bc_j_id_apt=0.0d00
!
      end subroutine allocate_bc_current
!
! -----------------------------------------------------------------------
!
       subroutine allocate_bc_magne_p
!
!
       allocate(bc_mag_p_id_apt(nod_bc1_f%num_bc_nod))
       if (nod_bc1_f%num_bc_nod .gt. 0)  bc_mag_p_id_apt=0.0d00
!
       end subroutine allocate_bc_magne_p
!
! -----------------------------------------------------------------------
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
       subroutine allocate_bc_vecp_sgs
!
!
       allocate(bc_a_sgs_id_apt(sgs_bc1_a%nmax_bc,3))
       if (sgs_bc1_a%nmax_bc .gt. 0)  bc_a_sgs_id_apt=0.0d00
!
       end subroutine allocate_bc_vecp_sgs
!
! -----------------------------------------------------------------------
!
      subroutine allocate_bc_magp_sgs
!
!
      allocate(bc_mp_sgs_id_apt(sgs_bc1_f%num_bc_nod))
      if (sgs_bc1_f%num_bc_nod .gt. 0) bc_mp_sgs_id_apt=0.0d00
!
      end subroutine allocate_bc_magp_sgs
!
! -----------------------------------------------------------------------
!
      end module m_bc_data_magne
