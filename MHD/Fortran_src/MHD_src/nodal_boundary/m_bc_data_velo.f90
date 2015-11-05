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
      type(vect_fixed_nod_bc_type) :: nod_bc1_v
!
      type(vect_fixed_nod_bc_type) :: sgs_bc1_v
!
      type(scaler_fixed_nod_bc_type) :: nod_bc1_p
!
      type(scaler_fixed_nod_bc_type) :: sgs_bc1_p
!
!
      real(kind=kreal), allocatable :: bc_v_id_apt(:,:)
!
      real(kind=kreal), allocatable :: bc_v_sgs_apt(:,:)
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
